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
