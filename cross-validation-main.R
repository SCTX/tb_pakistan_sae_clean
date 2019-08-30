library(INLA)
library(ggplot2)

source('cross-validation-utils.R')

##############################################################################
## Define input files/directories and load the required dataset

DATA.DIR <- file.path("data")

TRAINING.DATASET.PATH <- file.path(DATA.DIR, "training", "cluster_training_dataset.csv")

# Load training set
prev_survey_clean_df <- read.csv(TRAINING.DATASET.PATH)

## Set model formula
model.formula <- response ~ 1 + ( kpk + underweight_2012:kpk ) + ( sindh + age_65up:sindh ) +
                            ( age_1524 * female )


##############################################################################
## Run leave-one-out cross-validation

pred_prov <- run.cross.validation(prev_survey_clean_df, model.formula)

##############################################################################
## Process results (compute mse and rse) and plot them


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