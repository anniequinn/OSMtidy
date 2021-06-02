simplifyPoints <- function(input, 
                           descNew = NA, ## changed desc to descNew argument
                           distance = 1000, 
                           maxIterations = 100) {

  distance <- units::set_units(distance, "m")
  
  # SETUP
  iI <- st_is_within_distance(input, input, units::set_units(distance, "m"))
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf() %>% st_make_valid()
  
  # LOOP
  for (i in 1:maxIterations) {
    
    check <- input %>% slice(iI[[1]])
    
    # AUTOMATED DESCNEW
    if (any(str_detect(check$desc, "Exhibition centres; Stadiums and arenas") == TRUE)) {
      descN <- "Exhibition centres; Stadiums and arenas"
    } else if (any(str_detect(check$desc, "Exhibition centres; Conference centres") == TRUE)) {
      descN <- "Exhibition centres; Conference centres"
    } else if (any(str_detect(check$desc, "Exhibition centres; Events venue") == TRUE)) {
      descN <- "Exhibition centres; Events venue"
    } else if (any(str_detect(check$desc, "Exhibition centres; Ice rink") == TRUE)) {
      descN <- "Exhibition centres; Ice rink"
    } else if (any(str_detect(check$desc, "Wastewater") == TRUE)) {
      descN <- "Wastewater; Treatment works and facilities"
    } else if (any(str_detect(check$desc, "Water") == TRUE)) {
      descN <- "Water; Treatment works and facilities"
    } else if (any(str_detect(check$desc, "Pumping station") == TRUE)) {
      descN <- "Buildings; Pumping station"
    } else {
      print("No preset descNew has been found for this object type. Ensure you have specified a descNew argument.")
      descN <- descNew
    }
    
    if (nrow(check) > 1 & 
        sum(str_detect(check$type, "POLYGON")) >= 1 & 
        sum(str_detect(check$type, "POINT")) >= 1) 
    {
      output[[i]] <- 
        input %>% 
        slice(iI[[1]]) %>% 
        filter(!str_detect(type, "POINT")) %>% 
        summarise() %>% 
        mutate(desc = descN) ## changed to descN
      
    } else {
      
      if (sum(str_detect(check$type, "POINT")) == nrow(check)) {
        output[[i]] <- input %>% slice(iI[[1]]) %>% mutate(desc = descN) ## changed to descN
      } else {
        output[[i]] <- input %>% slice(iI[[1]]) %>% summarise() %>% mutate(desc = descN) ## changed to descN
      }
    }
    
    # Indices to remove
    remove <- which(sapply(1:length(iI), function(x) { iI[[x]] %in% iI[[1]] %>% sum }) > 0)
    
    # Update input & index
    input2 <- input2 %>% slice(-iI[[1]])
    iI <- iI[-remove]
    if (length(iI) == 0) 
      break
    
  }
  
  # OUTPUT
  output <- 
    output %>% 
    .bind_rows_sf() %>% 
    select(desc, geometry)
  
  return(output)
  
}
