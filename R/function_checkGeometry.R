checkGeometry <- function(geoData) { 
  vGeo <- c("POINT", "LINESTRING", "POLYGON")
  vCheck <- geoData$geometry %>% st_geometry_type %>% as.character %>% unique
  
  check1 <- sapply(vGeo, function(x) { x %in% vCheck }) %>% sum == 3
  check2 <- length(vCheck) == 3
  
  if(check1 == FALSE | check2 == FALSE) { warning("Spatial feature not simplified to 3 geometries: point, linestring and polygon. Some data preprocessing functions may not work.") }
  
  list(check1 = check1, check2 = check2)
  
}