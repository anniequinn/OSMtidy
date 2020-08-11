simplifyGolf <- function(dg, 
                         descSearch = "(golf)", 
                         descNew = "Sports and games; Golf",
                         maxIterations = 100,
                         rbind = TRUE) {
  
  source("functions/functions_internal_simplifyIntersects.R", local = TRUE)
  
  output <- 
    dg %>% 
    filter(str_detect(desc, descSearch)) %>%
    
    simplifyIntersects(desc = descNew, maxIterations = maxIterations) %>% 
    
    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)
  
  if(rbind == TRUE) { 
    output <- 
      list(dg %>% 
             filter(!str_detect(desc, descSearch)), 
           output) %>% 
      mapedit:::combine_list_of_sf()
  }
  
  return(output)
  
}