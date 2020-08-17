cookieCutter <- function(dataInput, dataShapefile) {
  
  internalFunction <- function(dataInput, dataShapefile) { 
    
    dataShapefile <- dataShapefile %>% summarise
    
    index <- dataInput %>% st_intersects(dataShapefile) %>% lengths > 0 # Impacted by hazard
    
    cutout <- dataInput[index,] %>% st_intersection(dataShapefile) # Subset (cut)
    
    return(cutout)
    
  }
  
  if(quiet == FALSE) { output <- internalFunction(dataInput, dataShapefile) }
  if(quiet == TRUE) { output <- 
    suppressWarnings( 
      suppressMessages(
        internalFunction(
          dataInput, dataShapefile
        )
      )
    )
  }
  
  return(output)
  
}