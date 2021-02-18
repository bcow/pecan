##' @name metric_PPMC
##' @title Pearson Product Moment Correlation
##' @export
##' @param metric_dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_PPMC <- function(metric_dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Pearson Product Moment Correlation")
  # numer <- sum((metric_dat$obvs - mean(metric_dat$obvs)) * (metric_dat$model - mean(metric_dat$model)))
  # denom <- sqrt(sum((metric_dat$obvs - mean(metric_dat$obvs)) ^ 2)) * sqrt(sum((metric_dat$model - mean(metric_dat$model)) ^ 2))
  # return(numer / denom)
  
  if(calc_method == "single_calc"){
    
    return(cor(metric_dat$obvs, metric_dat$model))
    
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
} # metric_PPMC
