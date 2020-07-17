dataCut <- function(dataExtracted, dataShapefile) {
  
  source("functions/functions_internal.R", local = TRUE)

  # -------------------------------------------------------------------------
  # STEP ONE - SETUP --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step one of four"))
  # -------------------------------------------------------------------------

  pboptions(char = "=", type = "timer", style = 1)
  
  dt = dataExtracted %>% purrr::flatten() %>% modify(. %>% as_tibble)
  
  
  # -------------------------------------------------------------------------
  # STEP TWO - st_join ------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step two of four"))
  # -------------------------------------------------------------------------

  dl <- 
    
    pblapply(dt, function(x) { 
      
      suppressMessages(
        suppressWarnings( 
          x %>% 
            as_tibble %>% 
            st_as_sf %>% 
            st_join(dataShapefile %>% as_tibble %>% st_as_sf, left = FALSE)
        )
      ) 
      
    })
  
  dl <- dl %>% setNames(names(dt)) %>% rmEmptyList()
  

  # -------------------------------------------------------------------------
  # STEP THREE - st_contains ------------------------------------------------
  # -------------------------------------------------------------------------  
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step three of four"))
  # -------------------------------------------------------------------------
  
  dl2 <- 
    
    pblapply(dl, function(x) { 
      
      suppressMessages(
        suppressWarnings(
          x %>% 
            mutate(contains = paste0("dl_", st_contains(dataShapefile, x, sparse = FALSE) %>% 
                                       unlist %>% 
                                       as.vector)) %>% 
            split(., .$contains)
        )
      )
      
    }) %>% 
    purrr::flatten()
  
  
  # -------------------------------------------------------------------------
  # STEP FOUR - TIDYING -----------------------------------------------------
  # -------------------------------------------------------------------------  
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step four of four"))
  # -------------------------------------------------------------------------

  st3False <- dl2[ which(str_detect(names(dl2), "FALSE")) ] # Not contained within shapefile
  st3True <- dl2[ which(str_detect(names(dl2), "TRUE")) ] # Contained within shapefile
  
  # Cut those objects which are not contained directly within the shapefile
  st3False <- pblapply(st3False, function(x) { 
    
    suppressMessages(
      suppressWarnings( 
        
        x %>% st_make_valid %>% st_intersection(dataShapefile) 
        
      )) 
    
  })
  
  
  # -------------------------------------------------------------------------
  # PREPARING OUTPUT --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Complete, preparing output"))
  # -------------------------------------------------------------------------

  output <- c(st3False, st3True)
  output <- output %>% modify(. %>% select(-contains)) %>% unname 
  
  class(output) <- c(class(output), "OSMtidy_dataCut")
  return(output)
  
}
