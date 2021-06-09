function_thresholdRail <- 
  function(input, threshold = 1000) {

  input %>%
    mutate(area = st_area(geometry) %>% as.numeric,
           desc = ifelse(area > threshold,
                         paste0(desc, " (Major)"),
                         paste0(desc, " (Minor)"))) %>%
    select(desc, geometry)

}

function_groupAirports <- 
  function(input = dt, threshold = 1000) {
    
    dl <- input %>% dplyr::filter(type != "POINT") %>% split(., .$desc)
    
    if (is_empty(dl$`Airport; Aerodrome, terminal and gates`) == TRUE & 
        is_empty(dl$`Public transport; Helipad`) == TRUE) {
      
      print("In this data there is 1 or fewer string or polygon data entries for 'Airport; Aerodrome, terminal and gates' or 'Public transport; Helipad' to be grouped.")
      
      return(dl)
      
    } else if (is_empty(dl$`Public transport; Helipad`) == TRUE) {
      
      groups <-
        dl$`Airport; Aerodrome, terminal and gates` %>%
        st_is_within_distance(dist = units::set_units(threshold, "m"))
      
      grouped <-
        dl$`Airport; Aerodrome, terminal and gates` %>%
        mutate(airportGroup = sapply(groups, function(x) x %>% paste0(collapse = "_")))
      
      output <- list(dl = dl, grouped = grouped)
      
      return(output)
      
    } else if (is_empty(dl$`Airport; Aerodrome, terminal and gates`) == TRUE) {
      
      groups <-
        dl$`Public transport; Helipad` %>%
        st_is_within_distance(dist = units::set_units(threshold, "m"))
      
      grouped <-
        dl$`Public transport; Helipad` %>%
        mutate(airportGroup = sapply(groups, function(x) x %>% paste0(collapse = "_")))
      
      output <- list(dl = dl, grouped = grouped)
      
      return(output)
      
    } else {
      
      groups <- 
        dl$`Airport; Aerodrome, terminal and gates` %>% 
        rbind(dl$`Public transport; Helipad`) %>% 
        st_is_within_distance(dist = units::set_units(threshold, "m"))
      
      grouped <-
        dl$`Airport; Aerodrome, terminal and gates` %>%
        rbind(dl$`Public transport; Helipad`) %>%
        mutate(airportGroup = sapply(groups, function(x) x %>% paste0(collapse = "_")))
      
      output <- list(dl = dl, grouped = grouped)
      
      return(output)
      
    }
    
  }

function_airportBoundaries <- 
  function(input, threshold = 0.5) {
    
    if (is.null(input$grouped) == TRUE) {
      
      if (nrow(input$`Airport; Aerodrome, terminal and gates`) == 1) {
        print("In this data there is only 1 string or polygon data entry for airports to simplify, in `Airport; Aerodrome, terminal and gates`.")
      }
      
      if (nrow(input$`Public transport; Helipad`) == 1) {
        print("In this data there is only 1 string or polygon data entry for airports to simplify, in `Public transport; Helipad`.")
      }
      
      if (nrow(input$`Transport infrastructure; Airport apron, runways and taxiways`) == 1) {
        print("In this data there is only 1 string or polygon data entry for airports to simplify, in `Transport infrastructure; Airport apron, runways and taxiways`.")
      }
      
      return(input)
      
    }
    
    
    else {
      
      dl <- 
        input[[2]] %>%
        mutate(area = st_area(geometry) %>% as.numeric())
      
      dl <- dl %>% split(., .$airportGroup) 
      
      check <- dl[(sapply(dl, nrow) > 1)]
      
      output <- 
        lapply(check, function(x) {
          x %>% 
            dplyr::filter(area > 0) %>% 
            mutate(area2 = area/max(area)) %>% 
            mutate(desc = ifelse(area2 >= threshold, 
                                 paste0(desc, " (Airport outline)"), 
                                 paste0(desc, " (Building outline)"))) %>% 
            select(desc, airportGroup, geometry)
        }
        ) %>% 
        c(dl[(sapply(dl, nrow) == 1)] %>% 
            modify(. %>% 
                     select(desc, airportGroup, geometry))) 
      
      return(output)
      
    }
    
  }

function_thresholdArea <- 
  function(input, thresholdArea = 20000) {
  
  if (is.null(input$airportGroup) == TRUE) {
    
    descSearch <- "irport|elipad"
    
    output <- 
      input %>% 
      dplyr::filter(str_detect(desc, descSearch)) %>%
      mutate(area = st_area(geometry)) %>% 
      as_tibble() %>%
      mutate(prefix = ifelse(area > units::set_units(20000, "m^2"), "Major", "Minor")) %>%
      mutate(desc = str_replace(desc, "Airport", paste0(prefix, " airport")),
             desc = str_replace(desc, "Helipad", paste0(prefix, " helipad")),
             type = st_geometry_type(geometry)) %>%
      select(desc, type, geometry)
    
    return(output)
    
  } else {
    
    descSearch <- "erodrome|elipad"
    
    key <- 
      input %>% 
      dplyr::filter(str_detect(desc, descSearch)) %>%
      dplyr::filter(!str_detect(desc, "Building outline")) %>%
      mutate(area = st_area(geometry)) %>% 
      as_tibble() %>%
      group_by(airportGroup, .groups = "drop_last") %>%
      summarise(area = sum(area) %>% as.numeric) %>% 
      ungroup %>%
      mutate(prefix = ifelse(area > thresholdArea, 
                             "Major", "Minor")) %>% 
      select(airportGroup, prefix)
    
    output <- 
      input %>% 
      left_join(key, by = "airportGroup") %>%
      mutate(desc = str_replace(desc, "Airport", paste0(prefix, " airport")),
             desc = str_replace(desc, "Helipad", paste0(prefix, " helipad")),
             type = st_geometry_type(geometry)) %>%
      select(desc, type, geometry)
    
    return(output)
    
  }
  
}

function_groupAirportInfrastructure <- 
  function(input, airports, airportsTidy, threshold = 2500) {
    
    check <- input %>% dplyr::filter(type != "POINT") %>% split(., .$desc)
    
    if (is.null(check$`Transport infrastructure; Airport apron, runways and taxiways`) == TRUE) { 
      print("In this data there are no string or polygon data entries to simplify for `Transport infrastructure; Airport apron, runways and taxiways`.")
      output <- airportsTidy %>% .bind_rows_sf() %>% select(desc, geometry)
      return(output)
      
    } else if (length(airportsTidy) == 1) {
      
      airportCompare <- 
        airportsTidy[[1]] %>% 
        mutate(area = st_area(geometry)) %>%
        dplyr::filter(area == max(area))
      
      dt <- 
        airports$dl$`Transport infrastructure; Airport apron, runways and taxiways` %>%
        mutate(distance = 
                 st_distance(airportCompare, 
                             airports$dl$`Transport infrastructure; Airport apron, runways and taxiways`) %>% 
                 unlist %>% 
                 as.vector() %>% 
                 as.numeric) %>% 
        dplyr::filter(distance < threshold) 
      
      dl <- 
        list(airportsTidy[[1]], dt) %>% 
        .bind_rows_sf() %>% 
        mutate(airportGroup = airportCompare$airportGroup[[1]]) %>% 
        select(desc, airportGroup, geometry)
      
      return(dl)
      
      
    } else {
      
      dl <- 
        lapply(1:length(airportsTidy), function(x) {
          
          airportCompare <- 
            airportsTidy[[x]] %>% 
            mutate(area = st_area(geometry)) %>%
            dplyr::filter(area == max(area))
          
          dt <- 
            airports$dl$`Transport infrastructure; Airport apron, runways and taxiways` %>%
            mutate(distance = st_distance(airportCompare, 
                                          airports$dl$`Transport infrastructure; Airport apron, runways and taxiways`) %>% 
                     unlist %>% 
                     as.vector() %>% 
                     as.numeric) %>% 
            dplyr::filter(distance < threshold) 
          
          output <- 
            list(airportsTidy[[x]], dt) %>% 
            .bind_rows_sf() %>% 
            mutate(airportGroup = airportCompare$airportGroup[[1]])
          
          return(output)
          
        }
        
        )
      
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
