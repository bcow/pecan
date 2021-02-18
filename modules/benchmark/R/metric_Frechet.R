##' @name metric_Frechet
##' @title Frechet Distance
##' @export
##' @param metric_dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery

metric_Frechet <- function(metric_dat, calc_method = "single_calc", ...) {
  logger.info("Metric: Frechet Distance")
  
  if(calc_method == "single_calc"){
    
    dat.no.na <- na.omit(metric_dat)
    Fdist <- SimilarityMeasures::Frechet(as.matrix(dat.no.na$obvs), as.matrix(dat.no.na$model))
    return(Fdist)
    
  }else if(calc_method == "ensemble_calc"){
    
    logger.warn("Not implemented for ensemble runs yet")
    return()
    
  }
  

} # metric_Frechet
