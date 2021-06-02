simplifyAirports <- function(dg,
                             threshold_distanceBuildings = 1000,
                             threshold_distanceInfrastructure = 2500,
                             threshold_outline = 0.5,
                             threshold_area = 20000,
                             rbind = TRUE) {

  source("functions/functions_internal_postProcessing.R", local = TRUE)

  descSearch <- "irport" ## made non-capital sensitive to be reusable / avoid using str_to_lower across multiple functions
 
   dt <- dg %>% filter(str_detect(str_to_lower(desc), descSearch))
  
   if (nrow(dt) == 0) { ## conditional to stop simplifyAirports function applied to datasets with no airport items
    
    stop("There is no airport data; simplifyAirports is not needed.")
     
   }
   
  else {
    
    output1 <- dt %>% function_groupAirports(threshold = threshold_distanceBuildings) 
    
    output2 <- 
      output1 %>% ## made more generic
      function_airportBoundaries(threshold = threshold_outline) 
    
    output3 <- 
      function_groupAirportInfrastructure(
        airports = output1, airportsTidy = output2, 
        threshold = threshold_distanceInfrastructure)
    
    output4 <- 
      output3 %>% 
      function_thresholdArea(thresholdArea = threshold_area)
    
    output_pt <-
      dt %>% filter(type == "POINT") ## extract point data
    
    if ((nrow(output_pt) < 1) == FALSE) { ## if any point data exists, bind it for later application of simplifyIntersects
      
      output4 <- output4 %>% rbind(output_pt)
      
    }
    
    output5 <- ## simplifyIntersects to remove extraneous point data
      output4 %>%
      st_as_sf() %>%
      simplifyIntersects(maxIterations = 100) %>%
      mutate(type = st_geometry_type(geometry))
    
    if (rbind == TRUE) {
      
      output <- 
        list(dg %>% filter(!str_detect(desc, descSearch)), output5) %>% 
        .bind_rows_sf()
      
    }
    
    return(output)
    
  }
   
}
