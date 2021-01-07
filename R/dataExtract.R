dataExtract <- function(dataShapefile, features = NULL,
                        timeout = 300, memsize = 1073741824) {

  # -------------------------------------------------------------------------
  # STEP ONE - SETUP --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step one of two"))
  # -------------------------------------------------------------------------

  # Create empty lists
  points <- list()
  lines <- list()
  polygons <- list()
  multilines <- list()
  multipolygons <- list()

  # Prepare features
  if(is.null(features)) { data("features"); features }
  nVec <- length(features)


  # -------------------------------------------------------------------------
  # STEP TWO - EXTRACT IN LOOP ----------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step two of two"))
  # -------------------------------------------------------------------------

  start <- Sys.time()

  # Apply loop to extract the data
  for(i in 1:nVec) {

    tmp <-
      dataShapefile %>%
      opq(timeout = timeout, memsize = memsize) %>%
      add_osm_feature(features[i]) %>%
      osmdata_sf(quiet = TRUE)

    Sys.sleep(0.2)

    progressText <-
      paste0("feature = ", features[[i]], ", ",
             i, " out of ", nVec, " (", round(i / nVec * 100, digits = 2), "%)",
             "; elapsed = ", difftime(Sys.time(), start, units = "mins") %>%
               as.double(units = "mins") %>%
               round(digits = 5), " mins")
    message(progressText)

    points[[features[i]]] <- tryCatch({
      tmp$osm_points %>% as.data.table %>% mutate(feature := features[[i]])
      }, error = function(e) NULL)
    lines[[features[i]]] <- tryCatch({
      tmp$osm_lines %>% as.data.table %>% mutate(feature := features[[i]])
      }, error = function(e) NULL)
    polygons[[features[i]]] <- tryCatch({
      tmp$osm_polygons %>% as.data.table %>% mutate(feature := features[[i]])
      }, error = function(e) NULL)
    multilines[[features[i]]] <- tryCatch({
      tmp$osm_multilines %>% as.data.table %>% mutate(feature := features[[i]])
      }, error = function(e) NULL)
    multipolygons[[features[i]]] <- tryCatch({
      tmp$osm_multipolygons %>% as.data.table %>% mutate(feature := features[[i]])
      }, error = function(e) NULL)

    rm(tmp)

  }

  # -------------------------------------------------------------------------
  # PREPARING OUTPUT --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Complete, preparing output"))
  # -------------------------------------------------------------------------

  # Remove lists with no data
  points <- points %>% .rmEmptyList %>% .rmNullList()
  lines <- lines %>% .rmEmptyList %>% .rmNullList()
  polygons <- polygons %>% .rmEmptyList %>% .rmNullList()
  multilines <- multilines %>% .rmEmptyList %>% .rmNullList()
  multipolygons <- multipolygons %>% .rmEmptyList %>% .rmNullList()

  # Join together to form a master list
  output <- list(points = points,
                 lines = lines,
                 polygons = polygons,
                 multilines = multilines,
                 multipolygons = multipolygons)

  # Assign class and return output
  class(output) <- c(class(output), "OSMtidy_dataExtract")
  return(output)

}
