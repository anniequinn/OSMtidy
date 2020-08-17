cookieCutter <- function(sf, sfToCut, quiet = TRUE) {
  
  internalFunction <- function(sf, sfToCut) {
    
    sfToCut <- sfToCut %>% summarise
    index <- sf %>% st_intersects(sfToCut) %>% lengths > 0 # Impacted by hazard
    cutout <- sf[index,] %>% st_intersection(sfToCut) # Subset (cut)
    return(cutout)
    
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
  
  return(output)
  
}