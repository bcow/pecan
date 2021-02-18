##' @name metric_AME
##' @title Absolute Maximum Error
##' @export
##' @param dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_AME <- function(dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Absolute Maximum Error")
  
  if(calc_method == "single_calc"){
    
    return(max(abs(dat$model - dat$obvs),na.rm = TRUE))
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
  
} # metric_AME
