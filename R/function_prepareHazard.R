prepareHazard <- function(shapefileOSM, shapefileHazard, dTol) {
  
  sf <- 
    st_read(shapefileOSM, quiet = TRUE) %>% 
    st_transform(4326)
  
  hazard <- 
    st_read(shapefileHazard, quiet = TRUE) %>% 
    st_make_valid() %>%
    st_transform(27700) %>% 
    st_simplify(dTolerance = dTol) %>% 
    st_transform(4326) 
  
  cookieCutter(sf = sf, sfToCut = hazard) %>% as_tibble %>% st_as_sf %>% select(geometry)
  
}