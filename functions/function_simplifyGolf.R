simplifyGolf <- function(dg, 
                         descSearch = "(golf)", 
                         descNew = "Sports and games; Golf",
                         maxIterations = 100,
                         rbind = TRUE) {
  
  source("functions/functions_internal_simplifyIntersects.R", local = TRUE)
  
  output <- 
    dg %>% 
    filter(str_detect(str_to_lower(desc), str_to_lower(descSearch))) %>%
    
    simplifyIntersects(desc = descNew, maxIterations = maxIterations) %>% 
    
    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)
  
  if(rbind == TRUE) { 
    output <- 
      list(dg %>% 
             filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))), 
           output) %>% 
      mapedit:::combine_list_of_sf()
  }
  
  return(output)
  
}