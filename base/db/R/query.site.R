##' Given site_id, return site table
##'
##' @param site_id numeric
##' @param dbcon : database connection
##' @export query.site
##'
##' @author Betsy Cowdery
##'
query.site <- function(site_id, dbcon){
  
  site <- tbl(dbcon, 'sites') %>% dplyr::filter(id == site_id) %>% collect()
  coords <- sf::st_as_sfc(site$geometry) %>% st_coordinates() %>% as.data.frame()

  if(all(c("X","Y") %in% colnames(coords))){
    site$lat <- coords$X
    site$lon <- coords$Y
    try(site$elev <- coords$Z)
  }else{
    logger.error("Could not retrieve site coordinates")
  }
  return(site)
}
