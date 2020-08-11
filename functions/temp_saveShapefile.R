saveShapefile <- function(dg) { 
  
  lapply(c("POINT", "LINESTRING", "POLYGON"), function(x) { 
    
    dg %>% 
      filter(str_detect(type, x)) %>% 
      select(desc, geometry) %>% st_write(paste0(x, ".shp"))
    
  })
  
}