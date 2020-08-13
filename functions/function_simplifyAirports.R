simplifyAirports <- function(dg, 
                             threshold_distanceBuildings = 1000, 
                             threshold_distanceInfrastructure = 2500,
                             threshold_outline = 0.5, 
                             threshold_area = 20000,
                             rbind = TRUE) {
  
  source("functions/functions_internal_postProcessing.R", local = TRUE)
  
  descSearch <- "airport"
  dt <- dg %>% filter(str_detect(str_to_lower(desc), descSearch))
  
  output1 <- 
    dt %>% 
    function_groupAirports(threshold = threshold_distanceBuildings)
  
  output2 <- 
    output1[[2]] %>% 
    function_airportBoundaries(threshold = threshold_outline)
  
  output3 <- 
    function_groupAirportInfrastructure(airports = output1, 
                                        airportsTidy = output2, 
                                        threshold = threshold_distanceInfrastructure)  
  
  output <- 
    output3 %>% 
    function_thresholdArea(thresholdArea = threshold_area) %>% 
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
