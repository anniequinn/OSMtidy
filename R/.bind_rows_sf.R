.bind_rows_sf <- function(dl, crs = 4326) {
  
  require(dplyr)
  require(sf)
  
  dt <-
    rbindlist(
      lapply(dl, function(x) {
        tmp <- select_(as.data.table(x, stringsAsFactors = FALSE),
                       paste0("-", attr(x, "sf_column", exact = TRUE)))
        if(nrow(tmp) == 0) {
          tmp <- data.table(tmpCol = rep(NA, x %>% nrow))
        }
        return(tmp)
      }),
      fill = TRUE)
  
  geo <- lapply(dl, function(x) { x %>% as_tibble %>% st_as_sf %>% st_geometry }) %>% unlist(recursive = FALSE) %>% st_sfc
  
  output <- st_sf(dt, geometry = geo, crs = st_crs(crs)) %>% .rmCols
  
  return(output)
  
}