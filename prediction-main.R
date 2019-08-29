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
model.formula <- response ~ 1 + ( kpk + underweight_2012:kpk ) + ( sindh + age_65up:sindh ) +
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
pred.frame[["age_1524:female"]] <- pred.frame[["age_1524"]] * pred.frame[["female"]]
pred.frame[["kpk:dirtyfuel_2011"]] <- pred.frame[["kpk"]] * pred.frame[["dirtyfuel_2011"]]
pred.frame[["sindh:age_65up"]] <- pred.frame[["sindh"]] * pred.frame[["age_65up"]]
pred.frame[["kpk:underweight_2012"]] <- pred.frame[["kpk"]] * pred.frame[["underweight_2012"]]
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

## Apply link function (inverse logit)
cell_pred <- plogis(cell_l)

## Rescale prevalence rate into absolute values
cell_pred <- cell_pred * pred.frame$pop_tot_2018

## Format the predictions as a stack of raster (each layer is one posterior draw)
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
