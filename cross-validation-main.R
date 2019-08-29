library(INLA)
library(ggplot2)

##############################################################################
## Define input files and directories

DATA.DIR <- file.path("data")

TRAINING.DATASET.PATH <- file.path(DATA.DIR, "training", "cluster_training_dataset.csv")

##############################################################################
## Set model formula
model.formula <- response ~ 1 + ( kpk + underweight_2012:kpk ) + ( sindh + age_65up:sindh ) +
                            ( age_1524 * female )

##############################################################################
## Run leave-one-out cross-validation

# Load training set
prev_survey_clean_df <- read.csv(TRAINING.DATASET.PATH)

message(sprintf("Fit model: prev ~ %s", as.character(model.formula[3])))
cluster_list <- sort(unique(prev_survey_clean_df$CLUSTER))

pred_list <- list()
i <- 1
for (cluster_num in cluster_list) {

    message(sprintf("\rFit cross-validation step %d/%d", i, length(cluster_list)), appendLF = FALSE)

    ## Select index of validation
    idx.valid <- prev_survey_clean_df$CLUSTER == cluster_num
    ## Copy dataframe
    df <- prev_survey_clean_df
    ## Set validation response to NA
    df[idx.valid,]$response <- NA
    ## Fit model
    lm_loo <- inla(formula = model.formula,
                   family="zeroinflated.binomial.0", Ntrials = df$count, data = df,
                   control.predictor = list(link = 1), 
                   control.fixed = list(prec = 0.001))
    
    ## Extract fitted value of validation index (and format to matrix with names)
    pred_loo <- t(matrix(lm_loo$summary.fitted.values[idx.valid,]))
    colnames(pred_loo) <- names(lm_loo$summary.fitted.values)
    rownames(pred_loo) <- prev_survey_clean_df[prev_survey_clean_df$CLUSTER == cluster_num,"CLUSTER"]
    
    pred_loo <- data.frame(subset(prev_survey_clean_df,
                                  subset = CLUSTER == cluster_num,
                                  select = c(raw_prev_rate, urban)),
                           pred_loo)
    
    pred_list[[i]] <- pred_loo

    i <- i+1
}
message("")

##############################################################################
## Process results (compute mse and rse) and plot them

(pred_prov  = do.call(rbind, pred_list))
pred_prov <- as.data.frame(lapply(pred_prov, unlist))

pred_prov$fit <- pred_prov$mean ## Can change pred at mode or mean (or anything you'd like)
pred_prov$sq_err <- (pred_prov$fit - pred_prov$raw_prev_rate) ** 2

(mse_prov <- mean(pred_prov$sq_err) * 1e10)
rse_denom <- sum((mean(prev_survey_clean_df$raw_prev_rate) - (prev_survey_clean_df$raw_prev_rate)) ** 2)
(rse_prov <- sum(pred_prov$sq_err) / rse_denom)
(cor(x = pred_prov$fit, y =pred_prov$raw_prev_rate) ** 2)

p_prov <- ggplot() +
    geom_point(data = pred_prov, aes(x = raw_prev_rate, y = fit, color = urban), alpha = 0.7, shape = 1) +
    geom_abline(slope = 1, linetype = "dashed") +
    ggtitle(paste("prev = ", as.character(model.formula)[3],
                  "\nmse =", round(mse_prov, 2), ", rse =", round(rse_prov, 2))) +
    theme_bw() +
    theme(plot.title = element_text(size = 10))
print(p_prov)
