##' @name metric_cor
##' @title Correlation Coefficient
##' @export
##' @param dat dataframe
##' @param calc_method
##' 
##' @author Mike Dietze

metric_cor <- function(dat, calc_method = "single_calc", ...) {
  PEcAn.logger::logger.info("Metric: Correlation Coefficient")
  
  if(calc_method == "single_calc"){
    
    return(cor(dat$model,dat$obvs,use ="pairwise.complete.obs"))
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  
} # metric_cor
