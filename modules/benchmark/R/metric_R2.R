##' @name metric_R2
##' @title Coefficient of Determination (R2)
##' @export
##' @param metric_dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_R2 <- function(metric_dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Coefficient of Determination (R2)")
  
  if(calc_method == "single_calc"){
    
    numer <- sum((metric_dat$obvs - mean(metric_dat$obvs)) * (metric_dat$model - mean(metric_dat$model)))
    denom <- sqrt(sum((metric_dat$obvs - mean(metric_dat$obvs)) ^ 2)) * sqrt(sum((metric_dat$model - mean(metric_dat$model)) ^ 2))
    
    out <- (numer / denom) ^ 2
    
    if(is.na(out)){
      fit <- lm(metric_dat$model ~ metric_dat$obvs)
      out <- summary(fit)$r.squared
    }
    
    return(out)
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
} # metric_R2
