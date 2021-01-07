dataShapefile <- function(filename, crs = NA) {

  # Read in shapefile
  shp <-  st_read(filename, quiet = TRUE)

  # Set projection
  # Assumes crs = 4326 by default
  if(!is.na(crs)) { shp <- shp %>% st_set_crs(crs) }
  output <- shp %>% st_transform(4326) %>% select(geometry)

  # Return output
  class(output) <- c(class(output), "OSMtidy_dataShapefile")
  return(output)

}
