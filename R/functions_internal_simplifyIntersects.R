simplifyIntersects <- 
  function(input, descNew = NA, maxIterations = 100) {
    
    if ((nrow(input) == 1) == TRUE) {
      print("In this data there is 1 or fewer point data entries for 'Airport; Aerodrome, terminal and gates' or 'Transport infrastructure; Airport apron, runways and taxiways' to simplify.")
      
      if (any(str_detect(input$desc, "golf") == TRUE)) {
        descN <- "Sports and games; Golf"
      }
      
      else if (any(str_detect(input$desc, "Major airport") == TRUE)) {
        descN <- "Major airports (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Minor airport") == TRUE)) {
        descN <- "Minor airports (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Major helipad") == TRUE)) {
        descN <- "Major helipads (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Minor helipad") == TRUE)) {
        descN <- "Minor helipads (simplified)"
      }
      
      else if (any(str_detect(input$desc, "Airport|Helipad") == TRUE)) {
        descN <- "Airports (to validate)"
      }
      
      else {
        print("No preset descNew has been found for this object type. Specify descNew argument.")
        descN <- descNew
      }
      
      output <- input %>% mutate(desc = descN)
      
    }
    
    iI <- input %>% st_transform(crs = 27700) %>% st_intersects
    
    iL_n <- lengths(iI)
    
    iI <- iI[order(iL_n, decreasing = TRUE)]
    
    output <- list()
    
    input2 <- input %>% st_as_sf %>% st_make_valid()
    
    for (i in 1:maxIterations) {
      
      output[[i]] <- input %>% slice(iI[[1]]) 
      
      if (any(str_detect(output[[i]]$desc, "golf") == TRUE)) {
        descN <- "Sports and games; Golf"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Major airport") == TRUE)) {
        descN <- "Major airports (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Minor airport") == TRUE)) {
        descN <- "Minor airports (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Major helipad") == TRUE)) {
        descN <- "Major helipads (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Minor helipad") == TRUE)) {
        descN <- "Minor helipads (simplified)"
      }
      
      else if (any(str_detect(output[[i]]$desc, "Airport|Helipad") == TRUE)) {
        descN <- "Airports (to validate)"
      }
      
      else {
        print("No preset descNew has been found for this object type. Specify descNew argument.")
        descN <- descNew
      }
      
      output[[i]] <- 
        output[[i]] %>% 
        summarise() %>% 
        mutate(desc = descN)
      
      remove <- 
        which(sapply(1:length(iI), function(x) {
          iI[[x]] %in% iI[[1]] %>% sum
        }
        ) > 0)
      
      input2 <- input2 %>% slice(-iI[[1]])
      
      iI <- iI[-remove]
      
      if (length(iI) == 0) 
        break
      
    }
    
    output <- output %>% .bind_rows_sf() %>% select(desc, geometry)
    
    return(output)
    
  }