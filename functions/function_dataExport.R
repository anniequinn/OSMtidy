dataExport <- function(data, name = NULL) { 
  
  source("functions/functions_internal.R", local = TRUE)
  
  if(!class(data) %in% c("OSMtidy_filterOverview", 
                         "OSMtidy_dataInput", 
                         "OSMtidy_dataExtract",
                         "OSMtidy_dataCut", 
                         "OSMtidy_dataWrangle", 
                         "OSMtidy_dataFilter",
                         "OSMtidy_dataTidy") %>% sum) {
    stop("class(data) is not of type OSMtidy")
  }
  
  
  # -------------------------------------------------------------------------
  # filterOverview ----------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_filterOverview") %>% sum) {
    
    fileName <- paste0("outputs/", "filterOverview_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    
    exportExcel(tibbleList = data, filename = fileName)
    
    message(paste0("File saved as: "))
    message(paste0("\n\t", fileName))
    
  }
  
  
  # -------------------------------------------------------------------------
  # dataInput, dataExtract, dataCut -----------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% c("OSMtidy_dataInput", 
                        "OSMtidy_dataExtract",
                        "OSMtidy_dataCut") %>% sum) {
    
    dt <- tibble(class = c("OSMtidy_dataInput", 
                           "OSMtidy_dataExtract",
                           "OSMtidy_dataCut"),
                 no = 1:3)
    
    classRow <- 
      dt %>% 
      filter(class %in% class(data)) %>% 
      mutate(class = str_replace(class, "OSMtidy_", ""), 
             prefix = paste(no, class, sep = "_"))
    
    file = paste0("outputs/", name, "_", classRow$prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    data %>% saveRDS(file)
    
    message(paste0("File saved as: ", file))
    
  }
  
  
  # -------------------------------------------------------------------------
  # dataWrangle -------------------------------------------------------------
  # -------------------------------------------------------------------------
  if(class(data) %in% "OSMtidy_dataWrangle" %>% sum) {
    
    fileWrangled = paste0("outputs/", name, "_4_dataWrangle_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    fileNoDetail = paste0("outputs/", name, "_4_dataWrangle-noDetail_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    
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
      modify(. %>% rmCols)
    
    exportExcel(tibbleList = dataNoDetail, filename = fileNoDetail)
    
    fileNames <- 
      lapply(c("fileWrangled", "fileNoDetail"), function(x) { 
        tryCatch(get(x), error = function(e) NULL) 
      }) %>% 
      rmNullList %>% 
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
      
      fileInput = paste0("outputs/", name, "_5_dataFilter-unfiltered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
      
      dataUnfiltered <-
        data$unfiltered %>%
        modify(. %>% 
                 as_tibble %>% 
                 mutate(geometry = st_as_text(geometry), desc = "") %>% 
                 select(osm_id, desc, geometry, everything())) %>% 
        bind_rows() %>% 
        split(., .$feature) %>% 
        modify(. %>% rmCols)
      
      exportExcel(tibbleList = dataUnfiltered, filename = fileInput)
      
    }
    
    if(length(data$unfiltered) >= 20) {
      fileInput <- "Unfiltered data list longer than 20; please export manually"
    }
    
    
    # -------------------------------------------------------------------------
    # FILTERED ----------------------------------------------------------------
    # -------------------------------------------------------------------------
    fileOutput1 = paste0("outputs/", name, "_5_dataFilter-filtered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    fileOutput2 = paste0("outputs/", name, "_5_dataFilter-filtered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    
    data$filtered %>% write_csv(fileOutput1)
    data$filtered %>% saveRDS(fileOutput2)
    
    
    # -------------------------------------------------------------------------
    # VALIDATE ----------------------------------------------------------------
    # -------------------------------------------------------------------------
    if(sum(names(data) %in% "validate") == 1) {
      
      fileVal = paste0("outputs/", name, "_5_dataFilter-validate_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
      
      dataValidate <- lapply(data$validate, function(x) {
        tmp <- x %>% as_tibble
        if(sum(names(tmp) %in% "desc") == 0) { tmp <- tmp %>% mutate(desc = NA) }
        tmp %>% 
          select(osm_id, filter, desc, geometry, everything()) %>% 
          rmCols
      }) %>% 
        bind_rows %>% 
        split(., .$desc) %>% 
        modify(. %>% rmCols) 
      
      vec <- sapply(1:length(dataValidate), function(x) { p1 = ((dataValidate[[x]] %>% nrow()) < 5); p1 == TRUE }) %>% which
      
      if(sum(vec) > 0) { 
        
        input <- 
          c(list(dataValidate[vec] %>% bind_rows() %>% rmCols()), 
            dataValidate[-vec])
        
      } else {
        
        input = dataValidate
        
      }
      
      exportExcel(tibbleList = input, filename = fileVal)
      
    }
    
    if(sum(names(data) %in% "validate") == 0) { fileVal = "No physical objects to validate"}
    
    
    fileNames <- 
      lapply(c("fileInput", "fileOutput1", "fileOutput2", "fileVal"), function(x) { 
        tryCatch(get(x), error = function(e) NULL) 
      }) %>% 
      rmNullList %>% 
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
          
          fileName1 <- paste0("outputs/", name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          fileName2 <- paste0("outputs/", name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
          
          data[[x]] %>% saveRDS(fileName1)
          data[[x]] %>% write_csv(fileName2)
          
          fileName <- c(fileName1, fileName2)
          
        }
        
        if(sum(names(data)[[x]] == "unfiltered") == 1) { 
          
          fileName <- paste0("outputs/", name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          data[[x]] %>% saveRDS(fileName)
          
        }
        
        if(sum(names(data)[[x]] == "remove") == 1) { 
          
          fileName <- paste0("outputs/", name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          data[[x]] %>% saveRDS(fileName)
          
        }
        
        if(sum(names(data)[[x]] == "removeKeywordFilters") == 1) { 
          
          fileName <- paste0("outputs/", name, "_6_dataTidy-", names(data)[[x]], "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
          data[[x]] %>% saveRDS(fileName)
          
        }
        
        
        return(fileName) 
        
        
        
      }) %>%
      purrr::flatten() %>% unlist()
    
    message(paste0("Files saved as: "))
    message(paste0("\n\t", fileNames))
    
  }
  
}
