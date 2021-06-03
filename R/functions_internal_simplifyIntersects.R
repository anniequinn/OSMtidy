simplifyIntersects <- function(input, descNew = NA, maxIterations = 100) {

  # ALTERNATE FOR NROW == 1
  
  if ((nrow(input) == 1) == TRUE) {
    
    print("In this data there is 1 or fewer point data entries for 'Airport; Aerodrome, terminal and gates' or 'Transport infrastructure; Airport apron, runways and taxiways' to simplify")
    
    if (any(str_detect(input$desc, "golf") == TRUE)) {
      descN <- "Sports and games; Golf"
    }
    else if (any(str_detect(input$desc, "Major") == TRUE)) {
      descN <- "Major airports (simplified)"
    }
    else if (any(str_detect(input$desc, "Minor") == TRUE)) {
      descN <- "Minor airports (simplified)"
    }
    else if (any(str_detect(input$desc, "Airport") == TRUE)) {
      descN <- "Airports (to validate)"
    }
    else {
      print("No preset descNew has been found for this object type. Specify descNew argument.")
      descN <- descNew
    }
    
    output <- input %>% mutate(desc = descN)
    
  }
  
  
  # SETUP
  iI <- input %>% st_transform(crs = 27700) %>% st_intersects
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf %>% st_make_valid()


  # LOOP
  for (i in 1:maxIterations) { 
    
    ## AUTOMATED DESCNEW FOR GOLF, AIRPORTS
    output[[i]] <- input %>% slice(iI[[1]]) 
    
    if (any(str_detect(output[[i]]$desc, "golf") == TRUE)) {
      descN <- "Sports and games; Golf"
    }
    else if (any(str_detect(output[[i]]$desc, "Major") == TRUE)) {
      descN <- "Major airports (simplified)"
    }
    else if (any(str_detect(output[[i]]$desc, "Minor") == TRUE)) {
      descN <- "Minor airports (simplified)"
    }
    else if (any(str_detect(output[[i]]$desc, "Airport") == TRUE)) {
      descN <- "Airports (to validate)"
    }
    else {
      print("No preset descNew has been found for this object type. Specify descNew argument.")
      descN <- descNew
    }

    output[[i]] <-
      input %>%
      slice(iI[[1]]) %>%
      summarise() %>%
      mutate(desc = descN) ##

    # Indices to remove
    remove <-
      which(
        sapply(1:length(iI), function(x) {
          iI[[x]] %in% iI[[1]] %>% sum
        }) > 0
      )

    # Update input & index
    input2 <- input2 %>% slice(-iI[[1]])
    iI <- iI[-remove]
    if(length(iI) == 0) break

  }


  # OUTPUT
  output <- output %>% .bind_rows_sf %>% select(desc, geometry)
  return(output)

}