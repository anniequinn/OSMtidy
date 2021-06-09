simplifyAirports <- 
  function(dg, threshold_distanceBuildings = 1000, 
           threshold_distanceInfrastructure = 2500, threshold_outline = 0.5, 
           threshold_area = 20000, rbind = TRUE) {

    descSearch <- "irport|elipad"
    
    dt <- dg %>% dplyr::filter(str_detect(desc, descSearch))
    
    if (nrow(dt) == 0) {
      stop("There is no airport data; simplifyAirports is not needed.") 
    }
    
    else {
      
      output1 <- 
        dt %>% 
        function_groupAirports(threshold = threshold_distanceBuildings) 
      
      output2 <- 
        output1 %>% 
        function_airportBoundaries(threshold = threshold_outline) 
      
      output3 <- 
        function_groupAirportInfrastructure(
          input = dt,
          airports = output1, 
          airportsTidy = output2, 
          threshold = threshold_distanceInfrastructure)
      
      output4 <- 
        output3 %>% 
        function_thresholdArea(thresholdArea = threshold_area)
      
      output_pt <-
        dt %>% dplyr::filter(type == "POINT")
      
      if ((nrow(output_pt) < 1) == FALSE) {
        output4 <- output4 %>% rbind(output_pt)
      }
      
      output5 <-
        output4 %>%
        st_as_sf() %>%
        simplifyIntersects(maxIterations = 100) %>%
        mutate(type = st_geometry_type(geometry))
      
      if (rbind == TRUE) {
        output <- 
          list(dg %>% dplyr::filter(!str_detect(desc, descSearch)), output5) %>% 
          .bind_rows_sf()
      }
      
      return(output)
      
    }
    
  }