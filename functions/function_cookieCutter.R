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
    
    suppressMessages(
      suppressWarnings( 
        
        x %>% st_make_valid %>% st_intersection(dtShp) 
        
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
  output <- output %>% mapedit:::combine_list_of_sf() %>% as_tibble %>% st_as_sf()
  
  class(output) <- c(class(output), "OSMtidy_cutOut")
  return(output)
  
}