countType <- function(dg, 
                      group, 
                      AHgen = FALSE) {
  
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