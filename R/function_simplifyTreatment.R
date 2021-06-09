simplifyTreatment <- 
  function(dg, distance = 1000, maxIterations = 100, rbind = TRUE) {

    waterKey <- 
      c("Buildings; Pumping station", 
        "Wastewater; Sanitary dump", 
        "Wastewater; Treatment works", 
        "Wastewater; Treatment works and facilities", 
        "Water", 
        "Water; Storage tower", 
        "Water; Treatment works and facilities")
    
    output <- 
      dg %>% 
      dplyr::filter(desc %in% waterKey) %>%
      simplifyPoints(distance = distance, maxIterations = maxIterations) %>%
      mutate(type = st_geometry_type(geometry)) %>% 
      select(desc, type, geometry)
    
    if (rbind == TRUE) {
      output <- 
        list(
          dg %>% 
            dplyr::filter(!desc %in% waterKey), 
          output) %>% 
        .bind_rows_sf()
    }
    
    return(output)
    
  }