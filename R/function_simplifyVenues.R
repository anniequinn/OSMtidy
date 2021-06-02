simplifyVenues <- function(dg, 
                           descSearch = "Exhibition", 
                           distance = 1, 
                           maxIterations = 100, 
                           rbind = TRUE) {
  
  output <- 
    dg %>% 
    filter(str_detect(desc, descSearch)) %>%
    simplifyPoints(distance = distance, maxIterations = maxIterations) %>%
    mutate(type = st_geometry_type(geometry)) %>% 
    select(desc, type, geometry)
  
  if (rbind == TRUE) {
    
    output <- 
      list(
        dg %>% filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))), 
        output) %>% 
      .bind_rows_sf()
    
  }
  
  return(output)
  
}