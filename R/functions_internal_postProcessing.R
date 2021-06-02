function_thresholdRail <- function(input, threshold = 1000) {

  input %>%
    mutate(area = st_area(geometry) %>% as.numeric,
           desc = ifelse(area > threshold,
                         paste0(desc, " (Major)"),
                         paste0(desc, " (Minor)"))) %>%
    select(desc, geometry)

}

function_groupAirports <- function(input, threshold = 1000) {
  
  # Remove all points and identify unique airports
  
  dl <- 
    input %>% 
    filter(type != "POINT") %>% 
    split(., .$desc)
  
  if (is_empty(dl$`Airport; Aerodrome, terminal and gates`) == TRUE) { ## if empty list element stop further processing
    
    print("In this data there is 1 or fewer string or polygon data entries for 'Airport; Aerodrome, terminal and gates' to simplify.")
    
    return(dl)
    
  }
  
  else {
    
    groups <-
      dl$`Airport; Aerodrome, terminal and gates` %>%
      st_is_within_distance(dist = units::set_units(threshold, "m"))
    
    grouped <-
      dl$`Airport; Aerodrome, terminal and gates` %>%
      mutate(airportGroup = 
               sapply(groups, function(x) x %>% paste0(collapse = "_")))
    output <- 
      list(dl = dl, grouped = grouped)
    
    return(output)
    
  }
  
}

function_airportBoundaries <- function(input, threshold = 0.5) {

  if (is.null(input$grouped) == TRUE) { ## check if grouped list element exists, if not return the input unchanged
    
    if (nrow(input$`Transport infrastructure; Airport apron, runways and taxiways`) < 2) {
      
      print("In this data there is 1 or fewer string or polygon data entries for `Transport infrastructure; Airport apron, runways and taxiways` to simplify.")
      
    }
    
    return(input)
    
  }
  
  # If the airport has more than one row of data
  # Check that the largest area is an order of magnitude greater
  # than the rest of the polygons
  
  else {
    
    dl <- 
      input[[2]] %>% ## changed
      mutate(area = st_area(geometry) %>% as.numeric())
    
    dl <- 
      dl %>% 
      split(., .$airportGroup) 
    
    check <- dl[(sapply(dl, nrow) > 1)]
    
    output <- 
      lapply(check, function(x) {
        x %>% 
          filter(area > 0) %>% 
          mutate(area2 = area/max(area)) %>% 
          mutate(desc = ifelse(area2 >= threshold, 
                               paste0(desc, " (Airport outline)"), paste0(desc, " (Building outline)"))) %>% 
          select(desc, airportGroup, geometry)
    }) %>% 
      c(dl[(sapply(dl, nrow) == 1)] %>% 
          modify(. %>% 
                   select(desc, airportGroup, geometry))) 
    
    return(output)
    
  }
  
}


function_thresholdArea <- function(input, thresholdArea = 20000) 
{
  
  if (nrow(input) < 2) { ## alternate for cases where there is not enough data to group
    
    output <- 
      input %>% 
      filter(str_detect(desc, "irport")) %>%
      mutate(area = st_area(geometry)) %>% 
      as_tibble() %>%
      mutate(prefix = ifelse(area > units::set_units(20000, "m^2"), 
                             "Major", "Minor")) %>%
      mutate(desc = str_replace(desc, "Airport", paste0(prefix, " airport")),
             type = st_geometry_type(geometry)) %>%
      select(desc, type, geometry)
   
     return(output)
    
  }
  
  else {
    
    key <- 
      input %>% 
      filter(str_detect(desc, "Aerodrome, terminal and gates")) %>%
      filter(desc != "Airport; Aerodrome, terminal and gates (Building outline)") %>%
      mutate(area = st_area(geometry)) %>% 
      as_tibble() %>%
      group_by(airportGroup, 
               .groups = "drop_last") %>%
      summarise(area = sum(area) %>% 
                  as.numeric) %>% 
      ungroup %>%
      mutate(prefix = ifelse(area > thresholdArea, 
                             "Major", "Minor")) %>% 
      select(airportGroup, prefix)
    
    output <- 
      input %>% 
      left_join(key, by = "airportGroup") %>%
      mutate(desc = str_replace(desc, "Airport", paste0(prefix, " airport")),
             type = st_geometry_type(geometry)) %>% ## shifted
      select(desc, type, geometry) ## shifted
    
    return(output)
    
  }
  
}

function_groupAirportInfrastructure <- function(airports, airportsTidy, threshold = 2500) {

  # Identify runways, taxiways and aprons that are part of the airport
  # i.e. linestring (predominantly)

  if (length(airports) == length(airportsTidy)) { ## if enough data to group, output 1 will have 2 elements and output 2 will have 1 element; otherwise skip function_groupAirportInfrastructure
   
     print("In this data there is 1 or fewer string or polygon data entries for `Transport infrastructure; Airport apron, runways and taxiways` to simplify.")
   
     output <- 
      airportsTidy %>% 
      .bind_rows_sf() %>% 
      select(desc, geometry)
     
    return(output)
     
  }
  
  else {
    
    dl <- 
      lapply(1:length(airportsTidy), function(x) {
      airportCompare <- 
        airportsTidy[[x]] %>% 
        mutate(area = st_area(geometry)) %>%
        filter(area == max(area))
      
      dt <- 
        airports$dl$`Transport infrastructure; Airport apron, runways and taxiways` %>%
        mutate(distance = st_distance(airportCompare, 
                                      airports$dl$`Transport infrastructure; Airport apron, runways and taxiways`) %>% 
                 unlist %>% 
                 as.vector() %>% 
                 as.numeric) %>% 
        filter(distance < threshold) 
      
      output <- 
        list(airportsTidy[[x]], dt) %>% 
        .bind_rows_sf() %>% 
        mutate(airportGroup = airportCompare$airportGroup[[1]])
      
      return(output)
      
    })
    
    output <- 
      dl %>% 
      .bind_rows_sf() %>% 
      select(desc, airportGroup, geometry)
    return(output) 
    
  }
  
}


# Optional
# If include length as an airport threshold
# Use the following minimum bounding circle to provide an estimate of length in polygons
#
# step9 <-
#   step8 %>%
#   filter(desc != "Airport; Aerodrome, terminal and gates") %>%
#   split(., .$type, drop = TRUE)
# step9
#
# step9b <- tryCatch(
#   step9$POLYGON %>%
#   st_transform(crs = 27700) %>%
#   mutate(circleArea = st_minimum_bounding_circle(geometry) %>%
#            st_area %>%
#            as.numeric,
#          circleRadius = sqrt(circleArea/pi),
#          circleDiameter = circleRadius*2,
#          length = circleDiameter) %>%
#   select(-circleArea, -circleRadius, -circleDiameter),
#   error = function(e) NULL)
# step9b
#
# step9c <-
#   tryCatch(
#     step9$LINESTRING %>%
#       mutate(length = st_length(geometry) %>% as.numeric),
#     error = function(e) NULL)
# step9c
#
# step10 <- list(step9b, step9c) %>% Filter(Negate(is.null), .) %>% .bind_rows_sf()
# step10 %>% as_tibble %>% group_by(airportGroup) %>% summarise(length = sum(length))
