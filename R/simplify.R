makeValid <- 
  function(dg) {

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

simplifyIntersects <- 
  function(input, descNew = NA, maxIterations = 100) {
    
    if ((nrow(input) == 1) == TRUE) {
      print("In this data there is 1 or fewer point data entries for 'Airport; Aerodrome, terminal and gates' or 'Transport infrastructure; Airport apron, runways and taxiways' to simplify.")
      
      if (any(str_detect(input$desc, "golf") == TRUE)) {
        descN <- "Sports and games; Golf"
      }
      
      else if (any(str_detect(input$desc, "Major airport") == TRUE)) {
        descN <- "Major airports (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Minor airport") == TRUE)) {
        descN <- "Minor airports (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Major helipad") == TRUE)) {
        descN <- "Major helipads (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Minor helipad") == TRUE)) {
        descN <- "Minor helipads (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Airport|Helipad") == TRUE)) {
        descN <- "Airports (to validate)"
      }
      
      else {
        print("No preset descNew has been found for this object type. Specify descNew argument.")
        descN <- descNew
      }
      
      output <- input %>% mutate(desc = descN)
      
    }
    
    iI <- input %>% st_transform(crs = 27700) %>% st_intersects
    
    iL_n <- lengths(iI)
    
    iI <- iI[order(iL_n, decreasing = TRUE)]
    
    output <- list()
    
    input2 <- input %>% st_as_sf %>% st_make_valid()
    
    for (i in 1:maxIterations) {
      
      output[[i]] <- input %>% slice(iI[[1]]) 
      
      if (any(str_detect(output[[i]]$desc, "golf") == TRUE)) {
        descN <- "Sports and games; Golf"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Major airport") == TRUE)) {
        descN <- "Major airports (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Minor airport") == TRUE)) {
        descN <- "Minor airports (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Major helipad") == TRUE)) {
        descN <- "Major helipads (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Minor helipad") == TRUE)) {
        descN <- "Minor helipads (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Airport|Helipad") == TRUE)) {
        descN <- "Airports (to validate)"
      }
      
      else {
        print("No preset descNew has been found for this object type. Specify descNew argument.")
        descN <- descNew
      }
      
      output[[i]] <- 
        output[[i]] %>% 
        summarise() %>% 
        mutate(desc = descN)
      
      remove <- 
        which(sapply(1:length(iI), function(x) {
          iI[[x]] %in% iI[[1]] %>% sum
        }
        ) > 0)
      
      input2 <- input2 %>% slice(-iI[[1]])
      
      iI <- iI[-remove]
      
      if (length(iI) == 0) 
        break
      
    }
    
    output <- output %>% .bind_rows_sf() %>% select(desc, geometry)
    
    return(output)
    
  }

simplifyGolf <-
  function (dg, descSearch = "golf", maxIterations = 100, rbind = TRUE) {
    
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

simplifyRail <- 
  function(dg, descSearch = "Rail station", 
           descNew = "Public transport; Rail station", 
           threshold_distance = 1000, threshold_area = 1000,
           maxIterations = 100, rbind = TRUE) {
    
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

simplifyPoints <- 
  function(input, descNew = NA, distance = 1000, maxIterations = 100) {
    
    distance <- units::set_units(distance, "m")
    
    iI <- st_is_within_distance(input, input, 
                                units::set_units(distance, "m"))
    iL_n <- lengths(iI)
    
    iI <- iI[order(iL_n, decreasing = TRUE)]
    
    output <- list()
    
    input2 <- input %>% st_as_sf() %>% st_make_valid()
    
    for (i in 1:maxIterations) {
      
      check <- input %>% slice(iI[[1]])
      
      if (any(str_detect(check$desc, 
                         "Exhibition centres; Stadiums and arenas") == TRUE)) {
        descN <- "Exhibition centres; Stadiums and arenas"
      }
      
      else if (any(str_detect(check$desc, 
                              "Exhibition centres; Conference centres") == TRUE)) {
        descN <- "Exhibition centres; Conference centres"
      }
      
      else if (any(str_detect(check$desc, 
                              "Exhibition centres; Events venue") == TRUE)) {
        descN <- "Exhibition centres; Events venue"
      }
      
      else if (any(str_detect(check$desc, "Exhibition centres; Ice rink") == TRUE)) {
        descN <- "Exhibition centres; Ice rink"
      }
      
      else if (any(str_detect(check$desc, "Wastewater") == TRUE)) {
        descN <- "Wastewater; Treatment works and facilities"
      }
      
      else if (any(str_detect(check$desc, "Water") == TRUE)) {
        descN <- "Water; Treatment works and facilities"
      }
      
      else if (any(str_detect(check$desc, "Pumping station") == TRUE)) {
        descN <- "Buildings; Pumping station"
      }
      
      else {
        print("No preset descNew has been found for this object type. Ensure you have specified a descNew argument.")
        descN <- descNew
      }
      
      if (nrow(check) > 1 & 
          sum(str_detect(check$type, "POLYGON")) >= 1 & 
          sum(str_detect(check$type, "POINT")) >= 1) {
        output[[i]] <- 
          input %>% slice(iI[[1]]) %>% dplyr::filter(!str_detect(type, "POINT")) %>% 
          summarise() %>% mutate(desc = descN)
      }
      
      else {
        
        if (sum(str_detect(check$type, "POINT")) == nrow(check)) {
          output[[i]] <- 
            input %>% slice(iI[[1]]) %>% mutate(desc = descN)
        }
        
        else {
          output[[i]] <- 
            input %>% slice(iI[[1]]) %>% summarise() %>% mutate(desc = descN)
        }
      }
      
      remove <- 
        which(sapply(1:length(iI), function(x) {
          iI[[x]] %in% iI[[1]] %>% sum
        }
        ) > 0)
      
      input2 <- input2 %>% slice(-iI[[1]])
      
      iI <- iI[-remove]
      
      if (length(iI) == 0) 
        break
      
    }
    
    output <- output %>% .bind_rows_sf() %>% select(desc, geometry)
    
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

simplifyTreatment <- 
  function(dg,  distance = 1000, maxIterations = 100, rbind = TRUE) {
    
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

simplifyVenues <- 
  function(dg, descSearch = "Exhibition", distance = 1, 
           maxIterations = 100, rbind = TRUE) {
    
    output <- 
      dg %>% 
      dplyr::filter(str_detect(desc, descSearch)) %>%
      simplifyPoints(distance = distance, maxIterations = maxIterations) %>%
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

exportOSMtidy <- 
  function(dg, locationName, type = c(".RDS", ".csv", ".shp")) {

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
