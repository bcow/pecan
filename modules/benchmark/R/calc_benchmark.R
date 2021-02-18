##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name calc_benchmark 
##' @title Calculate benchmarking statistics
##' @param bm.ensemble object, either from create_BRR or start.bm.ensemle
##' @param bety database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' @importFrom dplyr tbl filter rename collect select  
calc_benchmark <- function(settings, bety, start_year = NA, end_year = NA) {
  
  # run.score <- run.success.check(settings)
  
  if("benchmarking" %in% names(settings)){
    
    # If "run" is in the list of benchmarking metrics, add run.score record to the database
    # How are we dealing with ensemble runs? This is still an issue that has not been dealt with elsewhere in the code. 
    # For now this design only works with sigle run ensembles. 
    
    ##### This is where calc_benchmarks originally started 
    
    # Update benchmarks_ensembles and benchmarks_ensembles_scores tables
    
    ensemble <- tbl(bety,'ensembles') %>% filter(workflow_id == !!settings$workflow$id) %>% collect()
    
    # Retrieve/create benchmark ensemble database record
    bm.ensemble <- tbl(bety,'benchmarks_ensembles') %>% 
      filter(reference_run_id == !!settings$benchmarking$reference_run_id,
             ensemble_id %in% !!ensemble$id,  # ensemble$id has more than one element
             model_id == !!settings$model$id) %>%
      collect()
    
    if(nrow(bm.ensemble) == 0){
      bm.ensemble <- db.query(paste0("INSERT INTO benchmarks_ensembles",
                                     "(reference_run_id, ensemble_id, model_id, ",
                                     "user_id, citation_id)",
                                     "VALUES(",settings$benchmarking$reference_run_id,
                                     ", ",ensemble$id,
                                     ", ",settings$model$id,", ",settings$info$userid,
                                     ", 1000000001 ) RETURNING *;"), bety$con)
    }else if(nrow(bm.ensemble) >1){
      PEcAn.logger::logger.error("Duplicate record entries in benchmarks_ensembles")
    }
    bm.ensemble <- bm.ensemble %>% 
      hablar::convert(hablar::int("id", "reference_run_id", "ensemble_id", 
                                  "model_id", "citation_id", "user_id"))
    
    # --------------------------------------------------------------------------------------------- #
    # Setup
    
    site <- PEcAn.DB::query.site(site_id = as.numeric(settings$run$site$id), dbcon =  bety)
    
    run_ids <- tbl(bety, 'runs') %>% filter(ensemble_id == !!settings$ensemble$ensemble.id) %>% pull(id)
    
    if(length(run_ids) > 1){
      logger.info(sprintf( "Ensemble size of %.0f If you chose single run metrics, scores will be calculated for mean and CI's. Otherwise ensemble scores will be calculated.", length(run_ids)))
      calc_method <- "ensemble_calc"
    }else if(length(run_ids) == 1){
      logger.info("Ensemble size of 1 If you chose single run metrics, scores will be calculated for 1 run")
      calc_method <- "single_calc"
    }
    
    ###############
    # The calc_method determines how and where the model outputs are loaded
    
    # model_run <- dir(settings$modeloutdir, full.names = TRUE, include.dirs = TRUE)[1]
    # # How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
    
    # # All benchmarking records for the given benchmarking ensemble id
    # The benchmark entries returned from this query will include all previous 
    # benchmarks that have ever been done for the ensemble id. 
    # For now all benchmarks will be (re)calculated.  
    # This is could become problematic if previous benchmarks were
    # calculated with multiple inputs, which would mean that all of that data 
    # would need to be loaded and aligned again. 
    
    bm_id_search_list <- unlist(settings$benchmarking[which(names(settings$benchmarking) == "benchmark_id")])
    
    bms <- tbl(bety,'benchmarks') %>% dplyr::rename(benchmark_id = id) %>% 
      left_join(tbl(bety, "benchmarks_benchmarks_reference_runs"), by="benchmark_id") %>% 
      filter(reference_run_id == !!settings$benchmarking$reference_run_id,
             benchmark_id %in% bm_id_search_list) %>% 
      dplyr::select("benchmark_id", "input_id", "site_id", 
                    "variable_id", "reference_run_id") %>%
      collect() %>% hablar::convert(
        int("benchmark_id", "input_id", "site_id", 
            "variable_id", "reference_run_id"))
    
    var.ids <- bms$variable_id
    
    
    # --------------------------------------------------------------------------------------------- #
    # Determine how many data sets inputs are associated with the benchmark id's
    # bm.ids are split up in to groups according to their input data. 
    # So that the input data is only loaded once. 
    
    results <- list()
    
    # input.id = unique(bms$input_id) # For testing
    for (input.id in unique(bms$input_id)) {
      
      # Create directory that will hold benchmarking results
      bm_dir <- file.path(dirname(settings$modeloutdir), "benchmarking", input.id)
      if(!dir.exists(dirname(bm_dir))){dir.create(dirname(bm_dir))}
      if(!dir.exists(bm_dir)){dir.create(bm_dir)}
      
      bm.ids <- bms$benchmark_id[which(bms$input_id == input.id)]
      data.path <- PEcAn.DB::query.file.path(input.id, settings$host$name, bety)
      format_full <- format <- 
        PEcAn.DB::query.format.vars(input.id = input.id, 
                                    bety, format.id = NA, var.ids=var.ids)
      time.row <- format$time.row
      
      # ---- LOAD INPUT DATA ---- #
      vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
      
      # For testing
      # start_year = NA
      # end_year = NA
      
      if(is.na(start_year)) start_year <- lubridate::year(settings$run$start.date)
      if(is.na(end_year))   end_year <- lubridate::year(settings$run$end.date)
      
      obvs <- load_data(data.path, format, start_year = start_year, 
                        end_year = end_year, site, vars.used.index, time.row)
      dat_vars <- format$vars$pecan_name  # IF : is this line redundant?
      obvs_full <- obvs
      
      # ---- LOAD MODEL DATA ---- #
      
      
      model_vars <- format$vars$pecan_name[-time.row]  
      
      
      # Setup to deal with the question that Istem wrote,
      # but doesn't actually fix anything yet .... 
      # IF : what will happen when time.row is NULL?
      if(is.na(time.row)){
        model_vars <- format$vars$pecan_name # time.row is NULL
      }
      
      # For example 'AmeriFlux.level2.h.nc' format (38) has time vars year-day-hour listed, 
      # but storage type column is empty and it should be because in load_netcdf we extract
      # the time from netcdf files using the time dimension we can remove time variables from
      # this format's related variables list or can hardcode 'time.row=NULL' in load_x_netcdf function
      
      # Now we have to decide what runs we are using 
      # and where we are loading them from
      
      if(calc_method == "ensemble_calc"){
        
        logger.info("Loading multiple run outputs")
        read.model.list <- list()
        for(i in seq_along(run_ids)){
          read.model.list[[i]] <- read.output(runid = run_ids[i], 
                                              outdir = file.path(settings$modeloutdir,run_ids[i]), 
                                              start.year = start_year, 
                                              end.year = end_year,
                                              c("time", model_vars), dataframe = TRUE)
          if(i%%100==0){print(i)}
        }
        read.model <- read.model.list %>% purrr::map_dfc(dplyr::select, model_vars)
        
        remaining_cols <- setdiff(colnames(read.model.list[[1]]),colnames(read.model))
        read.model$posix <- read.model.list[[1]]$posix
      }else{
        read.model <- read.output(runid = run_ids, # should just be a single value 
                                  outdir = file.path(settings$modeloutdir,run_ids), 
                                  start.year = start_year, 
                                  end.year = end_year,
                                  c("time", model_vars), dataframe = TRUE)
      }
      model <- read.model
      model_names <- names(model) %>% 
        stringr::str_split("[.]{3}", n= 2,  simplify = TRUE) %>%
        bind_cols()
      colnames(model_names) <- c("v")
      
      model_names <- model_names %>% rowwise() %>%
        mutate(which_idx = case_when(v[1] %in% format$vars$pecan_name ~ TRUE,
                                     TRUE ~ FALSE))
      model_full <- model
      
      # ---- CALCULATE BENCHMARK SCORES ---- #
      
      results.list <- list()
      dat.list <- list()
      var.list <- c()
      
      # Loop over benchmark ids
      # i = 1 # for testing
      for (i in seq_along(bm.ids)) {
        bm <- tbl(bety, 'benchmarks') %>% filter(id == !!bm.ids[i]) %>% collect() 
        
        metrics <- tbl(bety, 'benchmarks_metrics') %>% 
          filter(benchmark_id == !!bm.ids[i]) %>% 
          left_join(tbl(bety, 'metrics') %>% rename(metric_id = id)) %>% collect()
        
        data("metric_ensemble_calc", package = "PEcAn.benchmark")
        metrics <- left_join(metrics, metric_ensemble_calc) 

        #"run" metric needs to be removed from metrics so it isn't computed twice
        var <- format$vars %>% 
          filter(variable_id == !!bm$variable_id) %>% 
          pull("pecan_name")
        var.list <- c(var.list, var)
        
        obvs.calc <- obvs_full %>% dplyr::select("posix", all_of(var)) %>% 
          hablar::convert(hablar::num(all_of(var)))
        
        msub_idx <- model_names %>% rowwise() %>%
          mutate(vsubset = case_when(v[1] %in% c("posix", var) ~ TRUE,
                                       TRUE ~ FALSE)
          ) %>% pull(vsubset)

        model.calc <- model_full[,msub_idx]

        # Check that the variables actually got loaded, otherwise don't send to calc_metrics
        # Not dealing with this right now but this should come back .... 
        # if(!(var %in% names(obvs.calc))|!(var %in% names(model.calc))){
        #   PEcAn.logger::logger.warn(paste0("Load did not work for ",var,". No metrics will be calculated."))
        #   next
        # }
        
        # TODO: If the scores have already been calculated, don't redo
        ensemble.id = bm.ensemble$ensemble_id # this is just to make debugging easier
        
        out.calc_metrics <- calc_metrics(model.calc, 
                                         obvs.calc, 
                                         var, 
                                         metrics,
                                         calc_method,
                                         ensemble.id,
                                         bm_dir)
        
        for(metric.id in metrics$id){
          metric.name <- filter(metrics,id == metric.id)[["name"]]
          score <- out.calc_metrics[["benchmarks"]] %>% 
            filter(metric == metric.name) %>% dplyr::select(score)
          
          # Update scores in the database
          
          score.entry <- tbl(bety, "benchmarks_ensembles_scores") %>%
            filter(benchmark_id == bm.ids[i]) %>%
            filter(benchmarks_ensemble_id == bm.ensemble$id) %>%
            filter(metric_id == metric.id) %>% 
            collect()
          
          # If the score is already in the database, should check if it is the same as the calculated 
          # score. But this requires a well written regular expression since it can be matching text. 
          
          if(dim(score.entry)[1] == 0){
            db.query(paste0(
              "INSERT INTO benchmarks_ensembles_scores",
              "(score, benchmarks_ensemble_id, benchmark_id, metric_id) VALUES ",
              "('",score,"',",bm.ensemble$id,", ",bm$id,",",metric.id,")"),bety$con)
          }else if(dim(score.entry)[1] >1){
            PEcAn.logger::logger.error("Duplicate record entries in scores")
          }
        }
        results.list <- append(results.list, list(out.calc_metrics[["benchmarks"]]))
        dat.list <- append(dat.list, list(out.calc_metrics[["dat"]]))
      }  #end loop over benchmark ids
      
      table.filename <- file.path(bm_dir, paste("benchmark.scores", var, bm.ensemble$ensemble_id, "pdf", sep = "."))
      pdf(file = table.filename)
      gridExtra::grid.table(do.call(rbind, results.list))
      dev.off()
      
      var.names <- c()
      for(k in seq_along(dat.list)){
        var.names <- c(var.names,unlist(strsplit(names(dat.list[[k]])[grep("[.]m", names(dat.list[[k]]))],"[.]"))[1]) # This is horrifying. Sorry future self. 
      }
      names(dat.list) <- var.names
      
      result.out <- list(bench.results = do.call(rbind, results.list),
                         data.path = data.path, 
                         format = format_full$vars, 
                         model = model_full, 
                         obvs = obvs_full, 
                         aligned.dat = dat.list)
      save(result.out, file = file.path(bm_dir,"benchmarking.output.Rdata"))
      
      results <- append(results, list(result.out)) # For testing
    } # end loop over input ids
    
    names(results) <- sprintf("input.%0.f", unique(bms$input_id)) # For testing
    return(invisible(results)) # For testing
  }
} # calc_benchmark
