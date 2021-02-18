##' @name metric_RAE
##' @title Relative Absolute Error
##' @export
##' @param metric_dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_RAE <- function(metric_dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Relative Absolute Error")
  
  if(calc_method == "single_calc"){
    
    metric_dat <- na.omit(metric_dat)
    numer <- mean(abs(metric_dat$obvs - metric_dat$model))
    denom <- mean(abs(metric_dat$obvs - mean(metric_dat$obvs)))
    return(numer/denom)
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  

} # metric_RAE
