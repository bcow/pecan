##' @name write_ic
##' @title write_ic
##' @param in.path
##' @param in.name
##' @param start_date
##' @param end_date
##' @param outfolder
##' @param model_info   dataframe with the modeltype id and modeltype name
##' @param new_site
##' @param pfts
##' @param source
##' @param overwrite
##' @export
##' @author Istem Fer
write_ic <- function(in.path, in.name, start_date, end_date, 
                     outfolder, model_info, new_site, pfts,
                     source = input_veg$source, overwrite = FALSE, ...){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  rds_file <- file.path(in.path, in.name)
  veg_info <- readRDS(rds_file) 
  
  #--------------------------------------------------------------------------------------------------#
  # Match PFTs
  
  obs <- as.data.frame(veg_info[[2]], stringsAsFactors = FALSE)
  
  # NOTE : match_pft may return NAs for unmatched dead trees
  pft.info <- PEcAn.data.land::match_pft(obs$bety_species_id, pfts, model_info$modeltype_id)
  
  ### merge with other stuff
  obs$pft <- pft.info$pft
  
  veg_info[[2]] <- obs
  
  #--------------------------------------------------------------------------------------------------#
  # veg2model
  
  ## Set model-specific functions
  pkg <- paste0("PEcAn.", model_info$modeltype_name)
  do.call("library", list(pkg))
  fcnx <- paste("veg2model.", model_info$modeltype_name, sep = "")
  if (!exists(fcnx)) {
    PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
  }else{
    fcn <- match.fun(fcnx)
  }
  
  out <- fcn(outfolder, veg_info, start_date, new_site, source)
  

  # Build results dataframe for convert.input
  results <- data.frame(file = out$filepath, 
                        host = c(PEcAn.remote::fqdn()), 
                        mimetype = out$mimetype, 
                        formatname = out$formatname, 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = out$filename, 
                        stringsAsFactors = FALSE)
  
  ### return for convert.inputs
  return(invisible(results))

  
} # write_ic
