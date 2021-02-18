simplifyIntersects <- function(input, desc, maxIterations = 100) {

  # SETUP
  iI <- input %>% st_transform(crs = 27700) %>% st_intersects
  iL_n <- lengths(iI)
  iI <- iI[order(iL_n, decreasing = TRUE)]
  output <- list()
  input2 <- input %>% st_as_sf %>% st_make_valid()


  # LOOP
  for(i in 1:maxIterations) {

    output[[i]] <-
      input %>%
      slice(iI[[1]]) %>%
      summarise() %>%
      mutate(desc = desc)

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
