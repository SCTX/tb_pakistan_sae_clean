library(INLA)
library(dplyr)
library(matrixStats)
library(RColorBrewer)
library(ggplot2)

##############################################################################
## Load utility functions (i.e. the `build.prediction.frame` function)

source('prediction-utils.R')

##############################################################################
## Set the model formula
# model.formula <- response ~ 1 + ( kpk + underweight_2012:kpk ) + ( sindh + age_65up:sindh ) +
#                             ( age_1524 * female )
model.formula <- response ~ 1 + ( kpk * underweight_2012 ) + ( sindh * age_65up ) +
                            ( age_1524 * female )

##############################################################################
## Build the prediction frame

pred.frame <- build.prediction.frame()

## Copy names from training set (to match the model formula)
# TODO: this is kind of an ugly trick to have the prediction covariate names 
# match the training ones...
pred.frame$dirtyfuel_2011 <- pred.frame$dirtyfuel_2017
pred.frame$bsgmi_2011 <- pred.frame$bsgmi_2018
pred.frame$underweight_2012 <- pred.frame$underweight_2017

## Set the desired age groups
pred.frame$age_65up <- pred.frame$age_6569 + pred.frame$age_7074 + 
                       pred.frame$age_7579 + pred.frame$age_80up
pred.frame$age_1524 <- pred.frame$age_1519 + pred.frame$age_2024

# # Filter columns to save RAM (optional)
# pred.frame <- pred.frame[,c("female", "age_1524", "age_65up", 
#                             "bsgmi_2011", "dirtyfuel_2011", "underweight_2012", 
#                             "kpk", "sindh", 
#                             "LONGITUDE", "LATITUDE",
#                             "pop_tot_2010", "pop_tot_2018"
#                             )]

# Make interaction terms
# TODO: pull names of interactions from formula and construct automatically
# pred.frame[["age_1524:female"]] <- pred.frame[["age_1524"]] * pred.frame[["female"]]
# pred.frame[["kpk:dirtyfuel_2011"]] <- pred.frame[["kpk"]] * pred.frame[["dirtyfuel_2011"]]
# pred.frame[["sindh:age_65up"]] <- pred.frame[["sindh"]] * pred.frame[["age_65up"]]
# pred.frame[["kpk:underweight_2012"]] <- pred.frame[["kpk"]] * pred.frame[["underweight_2012"]]
interact.labels <- attributes(terms(model.formula))$term.labels
for (label in interact.labels[grep(":", interact.labels)]) {
  print(label)
  pred.frame[[label]] <- pred.frame[[unlist(strsplit(label, ":"))[1]]] *
    pred.frame[[unlist(strsplit(label, ":"))[2]]]
}

for (i in 1:3) {
  print(i)
  tryCatch(i > 1, error = print("err"))
}

# And the intercept (which must be there to build the probabilities from samples later)
pred.frame[['(Intercept)']] <- 1

## Load the training data frame
train.frame <- read.csv(file.path("data", "training", "cluster_training_dataset.csv"))
# train.frame$tag <- "trn"


###############################################################################
## Fit the model on the training data.frame

## Fit model
model.fitted <- inla(formula = model.formula, 
                     family="binomial", 
                     Ntrials = train.frame$count, 
                     data = train.frame, 
                     control.predictor = list(link = 1), 
                     control.compute = list(config = TRUE),
                     control.inla = list(strategy = "laplace"),
                     verbose = TRUE)

summary(model.fitted)


###############################################################################
## Make prediction on fitted model

## Check validity of pred.frame
for(name in model.fitted$names.fixed){
    if(!(name %in% names(pred.frame))) 
        message(sprintf("WARNING: Column `%s` is is the model but missing in the prediction frame", name))
}


## Draw from posterior
n.draws <- 1000
draws <- inla.posterior.sample(n.draws, model.fitted)

## get samples as matrices
par_names <- rownames(draws[[1]]$latent)

# fixed effects
l_idx <- match(model.fitted$names.fixed, par_names)    # Get indices of fixed effects in draws
pred_l <- sapply(draws, function (x) x$latent[l_idx])  # Extract the sampled fixed effect values (matrix dim: num. covariates x num. draws)
rownames(pred_l) <- model.fitted$names.fixed
vals <- pred.frame[,model.fitted$names.fixed]          # Extract the covariate values (matrix dim: num. pixels x num. covariates)
cell_l <- unname(as.matrix(as(data.matrix(vals), "dgeMatrix") %*% pred_l))  # Matrix multiplication covariates (matrix dim: num. pixels x num. draws)
# cell_l <- as.matrix(vals) %*% pred_l                 # This also works

## Apply link function (inverse logit)
cell_pred <- plogis(cell_l)

## Rescale prevalence rate into absolute values
cell_pred <- cell_pred * pred.frame$pop_tot_2018

## Format the predictions as a stack of raster (each layer is one posterior draw) -- slow
pred.raster <- raster::stack(SpatialPixelsDataFrame(
  points = as.matrix(pred.frame[,c("LONGITUDE", "LATITUDE")]), 
  data = as.data.frame(cell_pred)))

###############################################################################
## Clip predictions onto the official district shapes

OFFICIAL.SHAPE.FILE.NAME <- file.path("data", "shapefiles", "pak_adm2.shp")

# Load the official disctrict shapes
shape.adm2 <- shapefile(OFFICIAL.SHAPE.FILE.NAME)

# Format the total population per pixel as a raster (i.e., `pop_tot_2018` column in pred.frame)
pop.tot.raster <- raster(SpatialPixelsDataFrame(
  points = as.matrix(pred.frame[,c("LONGITUDE", "LATITUDE")]), 
  data = data.frame(pred.frame[,"pop_tot_2018"])))

# Clip each prediction (for each posterior draw) onto the districts
pred.clipped.numer <- extract(pred.raster, shape.adm2, fun = sum, na.rm = T)
# Clip the total populations onto the districts
pred.clipped.denom <- extract(pop.tot.raster, shape.adm2, fun = sum, na.rm = T)

# Normalize the predictions into prevalence per 100'000
pred.prev.per.district <- pred.clipped.numer / pred.clipped.denom[,1] * 1e5

# Compute statistics from the prediction draws
shape.adm2$pred.prev.mean <- rowMeans(pred.prev.per.district)
shape.adm2$pred.prev.std <- rowSds(pred.prev.per.district)
shape.adm2$pred.quantile.025 <- apply(pred.prev.per.district, 1, function(x){ quantile(x, 0.025) })
shape.adm2$pred.quantile.975 <- apply(pred.prev.per.district, 1, function(x){ quantile(x, 0.975) })

# Plot the results
spplot(shape.adm2, zcol = "pred.prev.mean", main="Average Predicted Prevalence per District")
spplot(shape.adm2, zcol = "pred.prev.std", main="Standard Deviation of Prevalence per District")

###############################################################################
## Generate additional outputs

# Plot results using ggplot

library(broom)
library(rgeos)
library(tools)
library(ggplot2)
library(ggrepel)

shapefile_df <- suppressWarnings(broom::tidy(shape.adm2, region = "district"))
shapefile_df_ext <- merge(x = shapefile_df, y = shape.adm2@data[, c("district", "pred.prev.mean", "pred.prev.std")],
                          by.x = "id", by.y = "district", all.x = TRUE, sort = FALSE)
centroids <- rgeos::gCentroid(shape.adm2, byid = TRUE)
centroids_df <- data.frame(long = coordinates(centroids)[, 1], lat = coordinates(centroids)[,2],
                           label = tools::toTitleCase(tolower(as.character(shape.adm2@data$district))))
min_val <- round(min(shape.adm2@data$pred.prev.mean), -2)
mid_val <- round(mean(shape.adm2@data$pred.prev.mean), -2)
max_val <- round(max(shape.adm2@data$pred.prev.mean), -2)

p_prev <- ggplot() +
  geom_polygon(data = shapefile_df_ext, aes(x = long, y = lat, group = group, fill = pred.prev.mean), color = "gray", size = 0.05) +
  # geom_text_repel(data = centroids_df, aes(x = long, y = lat, label = label), box.padding = 0.05, force = 0.3, direction = "both", color = "black", size = 1, segment.size = 0.05) +
  theme_bw() +
  coord_map() +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mid_val,
                       breaks = round(seq(from = min_val, to = max_val, length.out = 11), 2),
                       guide = guide_legend(title = "Predicted\nprev 2018\nper 100 000"))
print(p_prev)

pdf(file = "output/pred_maps.pdf", height = 8, width = 8)
spplot(shape.adm2, zcol = "pred.prev.mean", main="Average Predicted Prevalence per District")
spplot(shape.adm2, zcol = "pred.prev.std", main="Standard Deviation of Prevalence per District")
print(p_prev)
dev.off()

# Output as csv
write.csv(file = "output/pred_maps.csv",
          x = subset(shape.adm2@data, select = c(district, province, pred.prev.mean, pred.prev.std,
                                                 pred.quantile.025, pred.quantile.975)),
          row.names = F)
