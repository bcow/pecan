##' @name metric_MAE
##' @title Mean Absolute Error
##' @export
##' @param dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery
##' 
metric_MAE <- function(dat, calc_method = "single_calc", ...) {
  
  PEcAn.logger::logger.info("Metric: Mean Absolute Error")
  
  if(calc_method == "single_calc"){
    
    return(mean(abs(dat$model - dat$obvs),na.rm=TRUE))
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
} # metric_MAE
