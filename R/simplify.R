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
          OSMtidyPackage:::.bind_rows_sf() %>%
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
    OSMtidyPackage:::.bind_rows_sf() %>%
    mutate(type = st_geometry_type(geometry))

  return(output)

}

simplifyIntersects <- function(input, desc, maxIterations = 100) {

  # SETUP
  iI <- input %>% st_transform(crs = 27700) %>% st_intersects
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf %>% st_make_valid()


  # LOOP
  for(i in 1:maxIterations) {

    output[[i]] <-
      input %>%
      slice(iI[[1]]) %>%
      summarise() %>%
      mutate(desc = desc)

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
  output <- output %>% OSMtidyPackage:::.bind_rows_sf() %>% select(desc, geometry)
  return(output)

}

simplifyGolf <- function(dg,
                         descSearch = "(golf)",
                         descNew = "Sports and games; Golf",
                         maxIterations = 100,
                         rbind = TRUE) {

  output <-
    dg %>%
    filter(str_detect(str_to_lower(desc), str_to_lower(descSearch))) %>%

    simplifyIntersects(desc = descNew, maxIterations = maxIterations) %>%

    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)

  if(rbind == TRUE) {
    output <-
      list(dg %>%
             filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))),
           output) %>%
      OSMtidyPackage:::.bind_rows_sf()
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
      OSMtidyPackage:::.bind_rows_sf()
  }

  return(output)

}

simplifyPoints <-
  function(input, desc,
           distance = 1000,
           maxIterations = 100) {

    distance = units::set_units(distance, "m")

    # SETUP
    iI <- st_is_within_distance(input, input, units::set_units(distance, "m"))
    iL_n <- lengths(iI)
    iI <- iI[order(iL_n, decreasing = TRUE)]
    output <- list()
    input2 <- input %>% st_as_sf() %>% st_make_valid()


    # LOOP
    for(i in 1:maxIterations) {

      check <- input %>% slice(iI[[1]])

      if(nrow(check) > 1 &
         sum(str_detect(check$type, "POLYGON")) >= 1 &
         sum(str_detect(check$type, "POINT")) >= 1) {

        output[[i]] <-
          input %>%
          slice(iI[[1]]) %>%
          filter(!str_detect(type, "POINT")) %>%
          summarise() %>%
          mutate(desc = desc)

      } else {

        if(sum(str_detect(check$type, "POINT")) == nrow(check)) {
          output[[i]] <- input %>% slice(iI[[1]]) %>% mutate(desc = desc)
        } else {
          output[[i]] <- input %>% slice(iI[[1]]) %>% summarise() %>% mutate(desc = desc)
        }

      }

      # Indices to remove
      remove <- which(sapply(1:length(iI), function(x) { iI[[x]] %in% iI[[1]] %>% sum }) > 0)

      # Update input & index
      input2 <- input2 %>% slice(-iI[[1]])
      iI <- iI[-remove]
      if(length(iI) == 0) break

    }


    # OUTPUT
    output <- output %>% OSMtidyPackage:::.bind_rows_sf() %>% select(desc, geometry)
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

  dl <- input %>% filter(type != "POINT") %>% split(., .$desc)

  groups <-
    dl$`Airport; Aerodrome, terminal and gates` %>%
    st_is_within_distance(dist = units::set_units(threshold, "m"))

  grouped <-
    dl$`Airport; Aerodrome, terminal and gates` %>%
    mutate(airportGroup = sapply(groups, function(x) x %>% paste0(collapse = "_")))

  output <- list(dl = dl, grouped = grouped)
  return(output)

}

function_airportBoundaries <- function(input, threshold = 0.5) {

  # If the airport has more than one row of data
  # Check that the largest area is an order of magnitude greater
  # than the rest of the polygons

  dl <-
    input %>%
    mutate(area = st_area(geometry) %>% as.numeric()) %>%
    split(., .$airportGroup)

  check <- dl[(sapply(dl, nrow) > 1)]

  output <-

    lapply(check, function(x) {
      x %>%
        filter(area > 0) %>%
        mutate(area2 = area / max(area)) %>%
        mutate(desc = ifelse(area2 >= threshold,
                             paste0(desc, " (Airport outline)"),
                             paste0(desc, " (Building outline)"))) %>%
        select(desc, airportGroup, geometry)
    }) %>%
    c(dl[(sapply(dl, nrow) == 1)] %>%
        modify(. %>% select(desc, airportGroup, geometry)))

  return(output)

}

function_thresholdArea <- function(input, thresholdArea = 20000) {

  key <-
    input %>%
    filter(str_detect(desc, "Aerodrome, terminal and gates")) %>%
    filter(desc != "Airport; Aerodrome, terminal and gates (Building outline)") %>%
    mutate(area = st_area(geometry)) %>%
    as_tibble() %>%
    group_by(airportGroup, .groups = "drop_last") %>%
    summarise(area = sum(area) %>% as.numeric) %>%
    ungroup %>%
    mutate(prefix = ifelse(area > thresholdArea, "Major", "Minor")) %>%
    select(airportGroup, prefix)

  output <-
    input %>%
    left_join(key, by = "airportGroup") %>%
    mutate(desc = str_replace(desc, "Airport", paste0(prefix, " airport")))
  return(output)

}

function_groupAirportInfrastructure <- function(airports, airportsTidy, threshold = 2500) {

  # Identify runways, taxiways and aprons that are part of the airport
  # i.e. linestring (predominantly)

  dl <-
    lapply(1:length(airportsTidy), function(x) {

      airportCompare <-
        airportsTidy[[x]] %>%
        mutate(area = st_area(geometry)) %>%
        filter(area == max(area))

      dt <-
        airports$dl$`Transport infrastructure; Airport apron, runways and taxiways` %>%
        mutate(distance =
                 st_distance(airportCompare,
                             airports$dl$`Transport infrastructure; Airport apron, runways and taxiways`) %>%
                 unlist %>% as.vector() %>% as.numeric) %>%
        filter(distance < threshold)

      output <-
        list(airportsTidy[[x]], dt) %>%
        OSMtidyPackage:::.bind_rows_sf() %>%
        mutate(airportGroup = airportCompare$airportGroup[[1]])
      return(output)
    })

  output <-
    dl %>%
    OSMtidyPackage:::.bind_rows_sf() %>%
    select(desc, airportGroup, geometry)
  return(output)

}

simplifyAirports <- function(dg,
                             threshold_distanceBuildings = 1000,
                             threshold_distanceInfrastructure = 2500,
                             threshold_outline = 0.5,
                             threshold_area = 20000,
                             rbind = TRUE) {

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
      OSMtidyPackage:::.bind_rows_sf()
  }

  return(output)

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
