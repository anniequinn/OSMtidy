simplifyGolf <-
  function (dg, descSearch = "golf", maxIterations = 100, rbind = TRUE) {
    
    source("R/functions_internal_simplifyIntersects.R", local = TRUE)
    
    output <- 
      dg %>% 
      dplyr::filter(str_detect(str_to_lower(desc), str_to_lower(descSearch))) %>% 
      simplifyIntersects(maxIterations = maxIterations) %>%
      mutate(type = st_geometry_type(geometry)) %>% 
      select(desc, type, geometry)
    
    if (rbind == TRUE) {
      output <- 
        list(
          dg %>% 
            dplyr::filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))), 
          output) %>% 
        .bind_rows_sf()
    }
    
    return(output)
    
  }