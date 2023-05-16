dataSummary <- function(data) {

  if(!class(data) %in% c("OSMtidy_filterOverview",
                         "OSMtidy_dataShapefile",
                         "OSMtidy_dataExtract",
                         "OSMtidy_dataCut",
                         "OSMtidy_dataWrangle",
                         "OSMtidy_dataFilter",
                         "OSMtidy_dataTidy") %>% sum) {
    stop("class(data) is not of type OSMtidy")
  }

  require(purrr)


  # -------------------------------------------------------------------------
  # dataInput ---------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataShapefile") %>% sum) {

    shapePlot <-
      data %>%
      ggplot() +
      geom_sf(fill = "lightgrey", colour = "darkgrey") +
      theme_void()

    output <-
      list(class = class(data),
           shapeProjection = st_crs(data),
           shapeArea = st_area(data),
           shapePerimeter = st_length(data),
           shapePlot = shapePlot)

  }


  # -------------------------------------------------------------------------
  # dataExtract -------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataExtract") %>% sum) {

    dt <-

      lapply(data, function(x) {

        lapply(x, function(y) {

        n = y %>% nrow()

      }) %>%
          map_dfr(~ .x %>% as_tibble(), .id = "feature")

    }) %>%
      map_dfr(~ .x %>% as_tibble(), .id = "type")

    output <-
      list(class = class(data),
           byGeometry = dt %>% group_by(type) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop_last") %>% ungroup %>% as.data.frame,
           byFeature = dt %>% group_by(feature) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop_last") %>% ungroup %>% as.data.frame)

  }


  # -------------------------------------------------------------------------
  # dataCut -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataCut") %>% sum) {

    dt <-

      lapply(data, function(x) {

        n = x %>% nrow
        type = x[1,] %>%
          st_geometry_type %>%
          as.vector %>%
          str_to_lower
        feature = x[1,]$feature

        tibble(feature = feature, type = type, total = n)

      }) %>%
      bind_rows()

    output <-
      list(class = class(data),
           byGeometry = dt %>% group_by(type) %>% summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>% ungroup %>% as.data.frame,
           byFeature = dt %>% group_by(feature) %>% summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>% ungroup %>% as.data.frame)

  }


  # -------------------------------------------------------------------------
  # dataWrangle -------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataWrangle") %>% sum) {

    dt1 <-
      data$dataWrangled %>%
      modify(. %>% select(contains("feature"), geometry)) %>%
      .bind_rows_sf() %>%
      as_tibble() %>%
      mutate(type = st_geometry_type(geometry) %>% str_to_lower) %>%
      select(-geometry) %>%
      mutate(data = "dataWrangled")

    dt2 <-
      data$noDetail %>%
      as_tibble() %>%
      mutate(type = st_geometry_type(geometry) %>% str_to_lower) %>%
      select(-geometry, -osm_id) %>%
      mutate(data = "noDetail")

    output <-

      list(class = class(data),
           byGeometry =

             rbind(dt1, dt2) %>% group_by(data, type) %>% summarise(total = n()) %>% ungroup() %>% as.data.frame %>%
             mutate(percent = total / sum(total) * 100,
                    percent = round(percent, digits = 2)),

           byFeature =

             rbind(dt1, dt2) %>% group_by(data, feature) %>% summarise(total = n()) %>% ungroup() %>% as.data.frame %>%
             mutate(percent = total / sum(total) * 100,
                    percent = round(percent, digits = 2))

      )

  }


  # -------------------------------------------------------------------------
  # dataFilter --------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataFilter") %>% sum) {

    dl <- list(filtered1 =
                 data$filtered %>%
                 summarise(total = n()),

               filtered2 =
                 data$filtered %>%
                 group_by(desc) %>%
                 summarise(total = n()),

               unfiltered =
                 data$unfiltered %>%
                 modify(. %>% st_as_sf) %>%
                 .bind_rows_sf() %>%
                 as_tibble() %>%
                 group_by(feature) %>%

                 summarise(total = n()),

               validate =
                 data$validate %>%
                 modify(. %>% rowwise %>% mutate(geometry = st_as_sfc(geometry)) %>% st_as_sf) %>%
                 .bind_rows_sf() %>%
                 as_tibble() %>%
                 group_by(feature) %>%
                 summarise(total = n())
    )

    summary <-

      list(filtered = dl$filtered1,
           unfiltered = dl$unfiltered %>% summarise(total = sum(total)),
           validate = dl$validate %>% summarise(total = sum(total))) %>%
      map_dfr(~ .x %>% as_tibble(), .id = "name") %>%
      rename(data = name) %>%
      mutate(percent = total / sum(total) * 100,
             percent = round(percent, digits = 2))

    summary

    byFeature <-
      dl[c("unfiltered", "validate")] %>%
      map_dfr(~ .x %>% as_tibble(), .id = "name") %>%
      rename(data = name)

    output <-
      list(class = class(data),
           summary = summary %>% as.data.frame,
           summaryFiltered = dl$filtered2,
           byFeature = byFeature %>% as.data.frame)

  }


  # -------------------------------------------------------------------------
  # dataTidy --------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataTidy") %>% sum) {

  summary <-
    lapply(data, nrow) %>%
    map_dfr(~ .x %>% as_tibble(), .id = "name") %>%
    rename(data = name) %>%
    rename(total = value) %>%
    mutate(percent = total / sum(total) * 100,
           percent = round(percent, digits = 2))

  dl <- list()

  if(sum(names(data) == "filtered") == 1) { dl$summaryFiltered <- data$filtered %>% group_by(desc) %>% summarise(total = n()) }

  if(sum(names(data) == "unfiltered") == 1) {

    dl$unfiltered <-
      data$unfiltered %>%
      as_tibble %>%
      select(-geometry) %>%
      group_by(feature) %>%
      summarise(total = n())

  }

  output <- list(class = class(data),
                 summary = summary) %>%
    c(dl) %>%
    .rmNullList

  }

  class(output) <- c(class(output), "OSMtidy_dataSummary")
  return(output)

}