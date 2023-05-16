dataExport <- function(data, name = NULL, directory = NULL) {

  if(!class(data) %in% c("OSMtidy_filterOverview",
                         "OSMtidy_dataShapefile",
                         "OSMtidy_dataExtract",
                         "OSMtidy_dataCut",
                         "OSMtidy_dataWrangle",
                         "OSMtidy_dataFilter",
                         "OSMtidy_dataTidy") %>% sum) {
    stop("class(data) is not of type OSMtidy")
  }
  
  require(OSMtidy)


  # -------------------------------------------------------------------------
  # filterOverview ----------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_filterOverview") %>% sum) {

    filename <- paste0("filterOverview_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    if(!is.null(directory)) { filename <- paste0(directory, "/", filename) }

    .exportExcel(tibbleList = data, filename = filename)

    message(paste0("File saved as: "))
    message(paste0("\n\t", filename))

  }


  # -------------------------------------------------------------------------
  # dataShapefile, dataExtract, dataCut -----------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataShapefile",
                        "OSMtidy_dataExtract",
                        "OSMtidy_dataCut") %>% sum) {

    dt <- tibble(class = c("OSMtidy_dataShapefile",
                           "OSMtidy_dataExtract",
                           "OSMtidy_dataCut"),
                 no = 1:3)

    classRow <-
      dt %>%
      filter(class %in% class(data)) %>%
      mutate(class = str_replace(class, "OSMtidy_", ""),
             prefix = paste(no, class, sep = "_"))

    filename <- paste0(name, "_", classRow$prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    if(!is.null(directory)) { filename <- paste0(directory, "/", filename) }
    data %>% saveRDS(filename)

    message(paste0("File saved as: ", filename))

  }


  # -------------------------------------------------------------------------
  # dataWrangle -------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% "OSMtidy_dataWrangle" %>% sum) {

    fileWrangled <- paste0(name, "_4_dataWrangle_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    fileNoDetail <- paste0(name, "_4_dataWrangle-noDetail_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")

    if(!is.null(directory)) {
      fileWrangled <- paste0(directory, "/", fileWrangled)
      fileNoDetail <- paste0(directory, "/", fileNoDetail)
    }

    data %>% saveRDS(fileWrangled)

    dataNoDetail <-
      data$noDetail %>%
      as_tibble %>%
      mutate(geometryType = st_geometry_type(geometry),
             geometry = st_as_text(geometry),
             desc = "",
             feature = str_replace(feature, ":", "_")) %>%
      select(osm_id, desc, geometryType, geometry, everything()) %>%
      split(., .$feature) %>%
      modify(. %>% .rmCols())

    .exportExcel(tibbleList = dataNoDetail, filename = fileNoDetail)

    fileNames <-
      lapply(c("fileWrangled", "fileNoDetail"), function(x) {
        tryCatch(get(x), error = function(e) NULL)
      }) %>%
      .rmNullList %>%
      unlist()

    message(paste0("Files saved as: "))
    message(paste0("\n\t", fileNames))


  }


  # -------------------------------------------------------------------------
  # dataFilter --------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% "OSMtidy_dataFilter" %>% sum) {

    require(xlsx)


    # -------------------------------------------------------------------------
    # UNFILTERED --------------------------------------------------------------
    # -------------------------------------------------------------------------
    if(length(data$unfiltered) > 0 & length(data$unfiltered) < 20 ) { # Number can be varied; implemented to speed up export

      fileInput <- paste0(name, "_5_dataFilter-unfiltered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
      if(!is.null(directory)) { fileInput <- paste0(directory, "/", fileInput) }

      dataUnfiltered <-
        data$unfiltered %>%
        modify(. %>%
                 as_tibble %>%
                 mutate(geometry = st_as_text(geometry), desc = "") %>%
                 select(osm_id, desc, geometry, everything())) %>%
        bind_rows() %>%
        split(., .$feature) %>%
        modify(. %>% .rmCols())

      tryCatch(.exportExcel(tibbleList = dataUnfiltered, filename = fileInput), error = function(e) NULL)

    }

    if(length(data$unfiltered) >= 20) {
      fileInput <- "Unfiltered data list longer than 20; please export manually"
    }


    # -------------------------------------------------------------------------
    # FILTERED ----------------------------------------------------------------
    # -------------------------------------------------------------------------
    fileOutput1 = paste0(name, "_5_dataFilter-filtered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    fileOutput2 = paste0(name, "_5_dataFilter-filtered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")

    if(!is.null(directory)) {
      fileOutput1 <- paste0(directory, "/", fileOutput1)
      fileOutput2 <- paste0(directory, "/", fileOutput2)
      }
               
    csv_output = data$filtered %>% mutate(geometry = st_as_text(geometry)) # Potentially very slow

    tryCatch(csv_output %>% write_csv(fileOutput1), error = function(e) NULL)
    tryCatch(data$filtered %>% saveRDS(fileOutput2), error = function(e) NULL)


    # -------------------------------------------------------------------------
    # VALIDATE ----------------------------------------------------------------
    # -------------------------------------------------------------------------
    if(sum(names(data) %in% "validate") == 1) {

      fileVal = paste0(name, "_5_dataFilter-validate_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
      if(!is.null(directory)) { fileVal <- paste0(directory, "/", fileVal) }

      dataValidate <- lapply(data$validate, function(x) {
        tmp <- x %>% as_tibble
        if(sum(names(tmp) %in% "desc") == 0) { tmp <- tmp %>% mutate(desc = NA) }
        tmp %>%
          select(osm_id, filter, desc, geometry, everything()) %>%
          .rmCols()
      }) %>%
        bind_rows %>%
        split(., .$desc) %>%
        modify(. %>% .rmCols())

      vec <- sapply(1:length(dataValidate), function(x) { p1 = ((dataValidate[[x]] %>% nrow()) < 5); p1 == TRUE }) %>% which

      if(sum(vec) > 0) {

        input <-
          c(list(dataValidate[vec] %>% bind_rows() %>% .rmCols()),
            dataValidate[-vec])

      } else {

        input = dataValidate

      }

      tryCatch(.exportExcel(tibbleList = input, filename = fileVal), error = function(e) NULL)

    }

    if(sum(names(data) %in% "validate") == 0) { fileVal = "No physical objects to validate"}


    fileNames <-
      lapply(c("fileInput", "fileOutput1", "fileOutput2", "fileVal"), function(x) {
        tryCatch(get(x), error = function(e) NULL)
      }) %>%
      .rmNullList %>%
      unlist()

    message(paste0("Files saved as: "))
    message(paste0("\n\t", fileNames))

  }

  # -------------------------------------------------------------------------
  # dataTidy ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% "OSMtidy_dataTidy" %>% sum) {

    fileNames <-

      lapply(1:length(data), function(x) {

        if(sum(names(data)[[x]] == "filtered") == 1) {

          fileName1 <- paste0(name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          fileName2 <- paste0(name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")

          if(!is.null(directory)) {
            fileName1 <- paste0(directory, "/", fileName1)
            fileName2 <- paste0(directory, "/", fileName2)
            }

          data[[x]] %>% saveRDS(fileName1)
          data[[x]] %>% write_csv(fileName2)

          filename <- c(fileName1, fileName2)

        }

        if(sum(names(data)[[x]] == "unfiltered") == 1) {

          filename <- paste0(name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          if(!is.null(directory)) { filename <- paste0(directory, "/", filename) }
          data[[x]] %>% saveRDS(filename)

        }

        if(sum(names(data)[[x]] == "remove") == 1) {

          filename <- paste0(name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          if(!is.null(directory)) { filename <- paste0(directory, "/", filename) }
          data[[x]] %>% saveRDS(filename)

        }

        if(sum(names(data)[[x]] == "removeKeywordFilters") == 1) {

          filename <- paste0(name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          if(!is.null(directory)) { filename <- paste0(directory, "/", filename) }
          data[[x]] %>% saveRDS(filename)

        }


        return(filename)


      }) %>%
      purrr::flatten() %>% unlist()

    message(paste0("Files saved as: "))
    message(paste0("\n\t", fileNames))

  }

}
