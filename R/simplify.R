makeValid <- function(dg) {

  output <-
    tryCatch(
      dg %>%
        st_as_sf(crs = 4326) %>%
        st_make_valid %>%
        mutate(type = st_geometry_type(geometry) %>% as.character) %>%
        select(desc = desc, type, geometry) %>%
        split(., .$type),

      error = function(e) {

        output <- dg %>% rowwise %>% mutate(valid = st_is_valid(geometry))
        output <-
          list(output %>%
                 filter(valid == FALSE) %>%
                 ungroup %>%
                 st_as_sf() %>%
                 st_make_valid,
               output %>%
                 filter(valid == TRUE) %>%
                 st_as_sf %>%
                 st_make_valid()) %>%
          .bind_rows_sf() %>%
          st_as_sf() %>%
          st_make_valid() %>%
          mutate(type = st_geometry_type(geometry) %>% as.character) %>%
          select(desc = desc, type, geometry) %>%
          split(., .$type)

        return(output)

      })

  vec <- c("POINT", "LINESTRING", "POLYGON")

  output <-
    c(lapply(vec, function(x) {
      tryCatch(
        suppressWarnings(
          output[["GEOMETRYCOLLECTION"]] %>%
            st_make_valid %>%
            st_collection_extract(x) %>%
            st_cast(x, warn = FALSE)
        ),
        error = function(e) NULL)
    }),

    lapply(vec, function(x) {
      tryCatch(
        output[[paste0("MULTI", x)]] %>%
          st_make_valid %>%
          st_cast(x, warn = FALSE),
        error = function(e) NULL)
    }),

    lapply(vec, function(x) {
      tryCatch( output[[x]]
                %>% st_make_valid,
                error = function(e) NULL)
    })) %>%
    Filter(Negate(is.null), .) %>%
    .bind_rows_sf() %>%
    mutate(type = st_geometry_type(geometry))

  return(output)

}

simplifyIntersects <- function(input, descNew = NA, maxIterations = 100) {
  
  # ALTERNATE FOR NROW == 1
  
  if ((nrow(input) == 1) == TRUE) {
    
    print("In this data there is 1 or fewer point data entries for 'Airport; Aerodrome, terminal and gates' or 'Transport infrastructure; Airport apron, runways and taxiways' to simplify")
    
    if (any(str_detect(input$desc, "golf") == TRUE)) {
      descN <- "Sports and games; Golf"
    }
    else if (any(str_detect(input$desc, "Major") == TRUE)) {
      descN <- "Major airports (simplified)"
    }
    else if (any(str_detect(input$desc, "Minor") == TRUE)) {
      descN <- "Minor airports (simplified)"
    }
    else if (any(str_detect(input$desc, "Airport") == TRUE)) {
      descN <- "Airports (to validate)"
    }
    else {
      print("No preset descNew has been found for this object type. Specify descNew argument.")
      descN <- descNew
    }
    
    output <- input %>% mutate(desc = descN)
    
  }
  
  
  # SETUP
  iI <- input %>% st_transform(crs = 27700) %>% st_intersects
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf %>% st_make_valid()
  
  
  # LOOP
  for (i in 1:maxIterations) { 
    
    ## AUTOMATED DESCNEW FOR GOLF, AIRPORTS
    output[[i]] <- input %>% slice(iI[[1]]) 
    
    if (any(str_detect(output[[i]]$desc, "golf") == TRUE)) {
      descN <- "Sports and games; Golf"
    }
    else if (any(str_detect(output[[i]]$desc, "Major") == TRUE)) {
      descN <- "Major airports (simplified)"
    }
    else if (any(str_detect(output[[i]]$desc, "Minor") == TRUE)) {
      descN <- "Minor airports (simplified)"
    }
    else if (any(str_detect(output[[i]]$desc, "Airport") == TRUE)) {
      descN <- "Airports (to validate)"
    }
    else {
      print("No preset descNew has been found for this object type. Specify descNew argument.")
      descN <- descNew
    }
    
    output[[i]] <-
      input %>%
      slice(iI[[1]]) %>%
      summarise() %>%
      mutate(desc = descN) ##
    
    # Indices to remove
    remove <-
      which(
        sapply(1:length(iI), function(x) {
          iI[[x]] %in% iI[[1]] %>% sum
        }) > 0
      )
    
    # Update input & index
    input2 <- input2 %>% slice(-iI[[1]])
    iI <- iI[-remove]
    if(length(iI) == 0) break
    
  }
  
  
  # OUTPUT
  output <- output %>% .bind_rows_sf %>% select(desc, geometry)
  return(output)
  
}

simplifyGolf <- function(dg,
                         descSearch = "golf", ## removed descSearch parentheses and descNew argument
                         maxIterations = 100,
                         rbind = TRUE) {
  
  source("functions/functions_internal_simplifyIntersects.R", local = TRUE)
  
  output <-
    dg %>%
    filter(str_detect(str_to_lower(desc), str_to_lower(descSearch))) %>%
    
    simplifyIntersects(maxIterations = maxIterations) %>% ## removed descNew argument
    
    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)
  
  if(rbind == TRUE) {
    output <-
      list(dg %>%
             filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))),
           output) %>%
      .bind_rows_sf
  }
  
  return(output)
  
}

simplifyRail <- function(dg,
                         descSearch = "Rail station",
                         descNew = "Public transport; Rail station",
                         threshold_distance = 1000,
                         threshold_area = 1000,
                         maxIterations = 100,
                         rbind = TRUE) {

  output <-
    dg %>%
    filter(str_detect(desc, descSearch)) %>%

    simplifyPoints(desc = descNew,
                   distance = threshold_distance,
                   maxIterations = maxIterations) %>%

    function_thresholdRail(threshold = threshold_area) %>%

    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)

  if(rbind == TRUE) {
    output <-
      list(dg %>%
             filter(!str_detect(desc, descSearch)),
           output) %>%
      .bind_rows_sf()
  }

  return(output)

}

simplifyPoints <- function(input, 
                           descNew = NA, ## changed desc to descNew argument
                           distance = 1000, 
                           maxIterations = 100) {
  
  distance <- units::set_units(distance, "m")
  
  # SETUP
  iI <- st_is_within_distance(input, input, units::set_units(distance, "m"))
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf() %>% st_make_valid()
  
  # LOOP
  for (i in 1:maxIterations) {
    
    check <- input %>% slice(iI[[1]])
    
    # AUTOMATED DESCNEW
    if (any(str_detect(check$desc, "Exhibition centres; Stadiums and arenas") == TRUE)) {
      descN <- "Exhibition centres; Stadiums and arenas"
    } else if (any(str_detect(check$desc, "Exhibition centres; Conference centres") == TRUE)) {
      descN <- "Exhibition centres; Conference centres"
    } else if (any(str_detect(check$desc, "Exhibition centres; Events venue") == TRUE)) {
      descN <- "Exhibition centres; Events venue"
    } else if (any(str_detect(check$desc, "Exhibition centres; Ice rink") == TRUE)) {
      descN <- "Exhibition centres; Ice rink"
    } else if (any(str_detect(check$desc, "Wastewater") == TRUE)) {
      descN <- "Wastewater; Treatment works and facilities"
    } else if (any(str_detect(check$desc, "Water") == TRUE)) {
      descN <- "Water; Treatment works and facilities"
    } else if (any(str_detect(check$desc, "Pumping station") == TRUE)) {
      descN <- "Buildings; Pumping station"
    } else {
      print("No preset descNew has been found for this object type. Ensure you have specified a descNew argument.")
      descN <- descNew
    }
    
    if (nrow(check) > 1 & 
        sum(str_detect(check$type, "POLYGON")) >= 1 & 
        sum(str_detect(check$type, "POINT")) >= 1) 
    {
      output[[i]] <- 
        input %>% 
        slice(iI[[1]]) %>% 
        filter(!str_detect(type, "POINT")) %>% 
        summarise() %>% 
        mutate(desc = descN) ## changed to descN
      
    } else {
      
      if (sum(str_detect(check$type, "POINT")) == nrow(check)) {
        output[[i]] <- input %>% slice(iI[[1]]) %>% mutate(desc = descN) ## changed to descN
      } else {
        output[[i]] <- input %>% slice(iI[[1]]) %>% summarise() %>% mutate(desc = descN) ## changed to descN
      }
    }
    
    # Indices to remove
    remove <- which(sapply(1:length(iI), function(x) { iI[[x]] %in% iI[[1]] %>% sum }) > 0)
    
    # Update input & index
    input2 <- input2 %>% slice(-iI[[1]])
    iI <- iI[-remove]
    if (length(iI) == 0) 
      break
    
  }
  
  # OUTPUT
  output <- 
    output %>% 
    .bind_rows_sf() %>% 
    select(desc, geometry)
  
  return(output)
  
}

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

exportOSMtidy <- function(dg, locationName, type = c(".RDS", ".csv", ".shp")) {

  prefix <- paste0(locationName, "_postProcessing")

  if(type == ".RDS") {

    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    dg %>% select(-type) %>% saveRDS(filename)

  }

  if(type == ".csv") {

    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    dg %>% select(-type) %>% write_csv(filename)

  }

  if(type == ".shp") {

    vec = c("POINT", "LINESTRING", "POLYGON")

    filename <-

      lapply(vec, function(x) {

        filename <- paste0(prefix, "_", x, "_",
                           format(Sys.time(), "%Y%m%d-%H%M%S"), ".shp")

        dg %>%
          filter(str_detect(type, x)) %>%
          select(desc, geometry) %>%
          st_write(filename, quiet = TRUE)

        return(filename)

      })

  }

  filename <- filename %>% unlist()

  message(paste0("Files saved as: "))
  message(paste0("\n\t", filename))

}
