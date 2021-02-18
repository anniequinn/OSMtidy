exportOSMtidy <- function(dg, locationName, type = c(".RDS", ".csv", ".shp")) {
  
  prefix <- paste0("outputs/", locationName, "_postProcessing") 
  
  if(type == ".RDS") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    dg %>% select(-type) %>% saveRDS(filename)
    
  }
  
  if(type == ".csv") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    dg %>% select(-type) %>% write_csv(filename)
    
  }
  
  if(type == ".shp") {
    
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