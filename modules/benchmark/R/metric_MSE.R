##' @name metric_MSE
##' @title Mean Square Error
##' @export
##' @param dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_MSE <- function(dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Mean Square Error")
  
  if(calc_method == "single_calc"){
    
    return(mean((dat$model - dat$obvs) ^ 2,na.rm=TRUE))
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
} # metric_MSE
