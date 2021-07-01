exportOSMtidy <- function(dg, 
                          locationName, 
                          path,
                          ext = c(".RDS", ".csv", ".shp")) {
  
  require(sf)
  
  # Create vector of dg class(es)
  class <- class(dg) %>% as.vector()
  
  if("sf" %in% class) { # If dg class includes "sf" (simple feature)
    
    dg <- 
      dg %>% 
      as.data.frame() %>% 
      mutate(type = sf::st_geometry_type(geometry), # Add column for geometry type
             geometry = sf::st_as_text(geometry)) # Reformat geometry as character class for speedier wrangling
    
  } else {
    
    dg <- dg %>% as.data.frame()
    
  }
  
  prefix <- paste0(path, locationName, "_postProcessing") 
  
  if(ext == ".RDS") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    
    dg %>% select(-type) %>% saveRDS(filename)
    
  }
  
  if(ext == ".csv") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    
    dg %>% select(-type) %>% write_csv(filename)
    
  }
  
  if(ext == ".shp") {
    
    vec = c("POINT", "LINESTRING", "POLYGON")
    
    filename <- 
      
      lapply(vec, function(x) {
        
        filename <- paste0(prefix, "_", x, "_",
                           format(Sys.time(), "%Y%m%d-%H%M%S"), ".shp")
        
        dg %>% 
          filter(str_detect(type, x)) %>% 
          select(desc, geometry) %>% 
          st_write(filename, quiet = TRUE)
        
        return(filename)
        
      })
    
  }
  
  filename <- filename %>% unlist() 
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filename))
  
}