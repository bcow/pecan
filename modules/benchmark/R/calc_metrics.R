##' @name calc_metrics
##' @title calc_metrics
##' @export
##' @param model.calc model data
##' @param obvs.calc observational data
##' @param var variables to be used 
##' @param metrics metrics to be used
##' @param calc_method 
##' @param ensemble.id id of ensemble run
##' @param bm_dir directory where benchmarking outputs will be saved
##' 
##' 
##' @author Betsy Cowdery
calc_metrics <- function(model.calc, obvs.calc, var, metrics, calc_method, 
                         ensemble.id, bm_dir) {
  
  # Remove leading and trailing NA's (not the same as na.omit)
  obvs.calc <- zoo::na.trim(obvs.calc, sides = "both", is.na = "any")
  if(calc_method == "single_calc"){
    # We should cut off and NA's at the beginning of a single model run 
    # But if there are multiple ensemble runs, the matrix should remain intact
    model.calc <- zoo::na.trim(model.calc, sides = "both", is.na = "any")}
  
  # There is probably a more efficient way of doing this
  
  if(ncol(model.calc)>2){
    not_posix_columns <- setdiff(names(model.calc), "posix")
    dat_list <- list()
    for(i in seq_along(not_posix_columns)){
      model.calc_in <- model.calc %>% dplyr::select("posix", not_posix_columns[i])
      colnames(model.calc_in) <- c("posix", var)
      dat_list[[i]] <- align_data(model.calc_in, obvs.calc, calc_method, var, 
                                  align_method = "mean_over_larger_timestep")
      if(i %% 100 == 0){print(i)}
    }
    dat <- dat_list %>% purrr::map_dfc(dplyr::select, paste0(var,".m"))
    dat[,paste0(var,".o")] <- dat_list[[1]][,paste0(var,".o")]
    dat$posix <- dat_list[[1]]$posix
    
    # Some of the means may have resulted in NaNs 
    # It will actually be easier to deal with them as just NA's 
    dat <- dat %>% mutate_all( ~ case_when(
      !is.nan(.x) ~ .x,
      is.nan(.x) ~ NA_real_))
  }else{
    dat <- align_data(model.calc, obvs.calc, var, 
                      align_method = "mean_over_larger_timestep")
  }
  
  # Make sure that anything that comes through align.data as NA doesn't get included. 
  # This is because there may be missing data. 
  # We may not want to do this automatically but rather have this as an option. 
  # dat <- dat[apply(dat,1, function(x) all(!is.na(x))),]
  
  results <- as.data.frame(matrix(NA, nrow = length(metrics$name), ncol = 3))
  colnames(results) <- c("metric", "variable", "score")
  results$metric <- metrics$name
  
  if(calc_method == "ensemble_calc"){
    n_ens <- ncol(dat)-2
    col_order <- c(paste(paste0(var,".m"),1:n_ens,sep = "..."), paste0(var,".o"), "posix")
    simple_col_name <- c(paste("model", 1:n_ens, sep = "."), "obvs", "time")
  }else if(calc_method == "single_calc"){
    col_order <- c(paste(var, c("m", "o"), sep = "."), "posix")
    simple_col_name <- c("model", "obvs", "time")
  }
  
  metric_dat <- dat[, col_order]
  colnames(metric_dat) <- simple_col_name
  
  for (m in seq_along(metrics$name)) {
    
    # For testing ensembles > 1
    # m = which(metrics$name == "CRPS")
    
    print(metrics$name[m])
    
    fcn <- paste0("metric_", metrics$name[m])
    results[m,"metric"] <- metrics$name[m]
    results[m,"variable"] <- var
    
    if (tail(unlist(strsplit(fcn, "_")), 1) == "plot") {
      filename <- file.path(bm_dir, 
                            paste("benchmark", metrics$name[m], var, ensemble.id, "pdf", sep = "."))
      do.call(fcn, args <- list(metric_dat, var, calc_method, filename))
      results[m,"score"] <- filename
    } else {
      results[m,"score"] <- as.character(do.call(fcn, args <- list(metric_dat, var, calc_method)))
    }
    
  }  #end loop over metrics
  
  return(list(benchmarks = results, dat = dat))
} # calc_metrics
