countType <- function(dg, 
                      group, 
                      AHgen = FALSE) {
  
  # https://github.com/r-spatial/sf/issues/1649
  # https://r-spatial.github.io/sf/articles/sf7.html
  # sf::sf_use_s2()
  sf::sf_use_s2(FALSE) # Until review for updated sf/s2 S^2 spherical space, revert to R^2 flat space
  
  if(AHgen == FALSE) {
    
    output <-
      dg %>%
      as_tibble() %>%
      mutate(type = st_geometry_type(geometry) %>% as.character,
             area = st_area(geometry) %>% as.numeric,
             length = st_length(geometry) %>% as.numeric) %>%
      group_by(!!group, type) %>%
      summarise(n = n(),
                area = sum(area, na.rm = TRUE),
                length = sum(length, na.rm = TRUE)) %>%
      ungroup
    
  } else if(AHgen == TRUE) {
    
    output <-
      dg %>%
      as_tibble() %>%
      mutate(type = st_geometry_type(geometry) %>% as.character,
             area = st_area(geometry) %>% as.numeric,
             length = st_length(geometry) %>% as.numeric) %>%
      left_join(key, by = "desc") %>%
      group_by(!!group, type) %>%
      summarise(n = n(),
                area = sum(area, na.rm = TRUE),
                length = sum(length, na.rm = TRUE)) %>%
      ungroup
    
  }
  
  return(output)
  
}