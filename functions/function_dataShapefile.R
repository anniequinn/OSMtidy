dataShapefile <- function(name, crs = NA) {
  
  source("functions/functions_internal.R", local = TRUE)
  
  
  # Determine file name
  if(str_detect(name, ".shp")) { filename = name } else { filename = paste0("shapefiles/", name, ".shp") }
  
  # Read in shapefile
  shp =  st_read(filename, quiet = TRUE)
  
  # Set projection
  if(!is.na(crs)) { shp = shp %>% st_set_crs(crs) }
  output = shp %>% st_transform(4326) %>% select(geometry)
  
  # Set plot as an attribute
  attr(output, "plot") <- 
    output %>% 
    ggplot() + 
    geom_sf(fill = "lightgrey", colour = "darkgrey") + 
    theme_minimal()
  
  # Return output
  class(output) <- c(class(output), "OSMtidy_dataInput")
  return(output)
  
}