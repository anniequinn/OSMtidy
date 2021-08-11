cookieCutter <- 
  function(sf, 
           sfToCut, 
           quiet = TRUE) {
  
  # https://github.com/r-spatial/sf/issues/1649
  # https://r-spatial.github.io/sf/articles/sf7.html
  # sf::sf_use_s2()
  sf::sf_use_s2(FALSE) # Until review for updated sf/s2 S^2 spherical space, revert to R^2 flat space
  
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