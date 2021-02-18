##' @name metric_CRPS
##' @title Continuous Rank Probability Score
##' @export
##' @param metric_dat dataframe
##' @param calc_method
##' 
##' @author Betsy Cowdery
##' 
metric_CRPS <- function(metric_dat, calc_method = "ensemble_calc", ...) {
  PEcAn.logger::logger.info("Metric: Continuous Rank Probability Score")
  # I know this isn't how you are supposed to do it 
  library(scoringRules)
  
  if(calc_method == "single_calc"){
    
    logger.warn("Not implemented for single runs")
    return()
    
  }else if(calc_method == "ensemble_calc"){
    
    metric_dat$crps <- NA
    for(i in seq_along(metric_dat$crps)){
      model_no <- paste("model", i, sep = ".")
      metric_dat$crps[i] <- scoringRules::crps_sample(y = metric_dat$obvs[i], 
                                                      dat = metric_dat[,model_no])
    }
    
    
    metric_dat_summary <- metric_dat %>% rowwise() %>% 
      mutate(
        mean = mean(c_across(starts_with("model")), na.rm = TRUE),
        N = sum(!is.na(c_across(starts_with("model")))),
        SD = sd(c_across(starts_with("model")), na.rm = TRUE), 
        CI_upp = quantile(c_across(starts_with("model")), probs  = .975, na.rm = TRUE),
        CI_low = quantile(c_across(starts_with("model")), probs = .025, na.rm = TRUE)
      )
    metric_dat_summary$N
    
    plot_data <- metric_dat_summary %>% dplyr::select("time", "mean", "CI_upp", "CI_low", 
                                                      "obvs", "crps")
    plot_data_long <- 
      tidyr::pivot_longer(plot_data, cols = c("mean", "CI_upp", "CI_low", "obvs", "crps"), 
                          names_to = "names") %>% 
      mutate(plot = case_when(names == "crps" ~ "Continuous Rank Probability Score",
                              TRUE ~ "Model-Data Comparison"))
    
    ggplot(plot_data_long) + 
      geom_smooth(aes(x = time, y = value, color = names), size = 2) + 
      facet_wrap(~plot, ncol =1)
    
    ggplot(metric_dat_summary) + 
      geom_smooth(aes(x = mean, y = crps), size = 2)
    
    ggplot(metric_dat_summary) + 
      geom_smooth(aes(x = SD, y = crps), size = 2) 
    
    metric_dat_summary$SD
    
    return <- jsonlite::toJSON(crps)  
    
  }
  
  return(crps)
  
} # metric_CRPS
