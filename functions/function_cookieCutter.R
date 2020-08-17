<<<<<<< Updated upstream
cookieCutter <- function(dataInput, dataShapefile) { 
  
  source("functions/functions_internal.R", local = TRUE)
  
  # -------------------------------------------------------------------------
  # STEP ONE - SETUP --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step one of three"))
  # -------------------------------------------------------------------------
  
  pboptions(char = "=", type = "timer", style = 1)
  
  dt = dataInput %>% as_tibble %>% st_as_sf()
  dtShp <- dataShapefile %>% summarise()
  
  
  # -------------------------------------------------------------------------
  # STEP TWO - st_join and st_contains --------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step two of three"))
  # -------------------------------------------------------------------------
  
  dtJoin <- suppressMessages(dt %>% st_join(dtShp, left = FALSE))

  dtContains <- suppressMessages(
    dtJoin %>% 
    mutate(contains = paste0("dl_", st_contains(dtShp, dtJoin, sparse = FALSE) %>% 
                                       unlist %>% 
                                       as.vector)) %>% 
            split(., .$contains)
  )
  
  
  # -------------------------------------------------------------------------
  # STEP THREE - TIDYING -----------------------------------------------------
  # -------------------------------------------------------------------------  
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step three of three"))
  # -------------------------------------------------------------------------
  
  st3False <- dtContains[ which(str_detect(names(dtContains), "FALSE")) ] # Not contained within shapefile
  st3True <- dtContains[ which(str_detect(names(dtContains), "TRUE")) ] # Contained within shapefile
  
  # Cut those objects which are not contained directly within the shapefile
  st3False <- pblapply(st3False, function(x) { 
=======
cookieCutter <- function(sf, sfToCut, quiet = TRUE) {
  
  internalFunction <- function(sf, sfToCut) { 
    
    sfToCut <- sfToCut %>% summarise
    
    index <- sf %>% st_intersects(sfToCut) %>% lengths > 0 # Impacted by hazard
    
    cutout <- sf[index,] %>% st_intersection(sfToCut) # Subset (cut)
>>>>>>> Stashed changes
    
    suppressMessages(
      suppressWarnings( 
        
        x %>% st_make_valid %>% st_intersection(dtShp) 
        
      )) 
    
<<<<<<< Updated upstream
  })
  
  
  # -------------------------------------------------------------------------
  # PREPARING OUTPUT --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Complete, preparing output"))
  # -------------------------------------------------------------------------
  
  output <- c(st3False, st3True)
  output <- output %>% modify(. %>% select(-contains)) %>% unname 
  output <- output %>% mapedit:::combine_list_of_sf() %>% as_tibble %>% st_as_sf()
=======
  }
  
  if(quiet == FALSE) { output <- internalFunction(sf, sfToCut) }
  if(quiet == TRUE) { output <- 
    suppressWarnings( 
      suppressMessages(
        internalFunction(
          sf, sfToCut
        )
      )
    )
  }
>>>>>>> Stashed changes
  
  class(output) <- c(class(output), "OSMtidy_cutOut")
  return(output)
  
}