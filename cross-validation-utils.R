message('  <<< LOADING CROSS-VALIDATION UTILITIES >>> ')

library(INLA)

#' Run leave-one-out cross-validation on the input data.frame `prev_survey_clean_df`
#' using the model formula `model.formula`
#'
#' @param prev_survey_clean_df Input data.frame
#' @param model.formula Model formula
#' 
#' @return data.frame containing the model fit for each cv iteration
run.cross.validation <- function(prev_survey_clean_df, model.formula) {
    
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
  
  (pred_prov  = do.call(rbind, pred_list))
  pred_prov <- as.data.frame(lapply(pred_prov, unlist))
  
  pred_prov$fit <- pred_prov$mean ## Can change pred at mode or mean (or anything you'd like)
  pred_prov$sq_err <- (pred_prov$fit - pred_prov$raw_prev_rate) ** 2
  
  return(pred_prov)
}