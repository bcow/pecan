##' @name metric_RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_RMSE <- function(dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Root Mean Square Error")
  
  if(calc_method == "single_calc"){
    
    return(sqrt(mean((dat$model - dat$obvs) ^ 2,na.rm=TRUE)))
    
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
} # metric_RMSE
