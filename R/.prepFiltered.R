.prepFiltered <- function(data) {
  
  if(sum(sapply(data, function(x) { x$geometry %>% is.character })) > 0) {
    
    if(sum(names(data[[1]]) %in% "desc") == 1) {
      
      data <-
        data %>%
        modify(. %>% filter(!is.na(desc)))
      
    }
    
    data <-
      data %>%
      modify(. %>%
               rowwise %>%
               mutate(geometry = st_as_sfc(geometry)) %>%
               ungroup)
    
  }
  
  
  output <-
    data %>%
    .rmEmptyList() %>%
    modify(. %>% rowwise %>% st_as_sf %>% ungroup) %>%
    .bind_rows_sf %>%
    as_tibble() %>%
    .rmCols
  if(sum(names(output) %in% "desc") == 0) { output <- output %>% mutate(desc = "") }
  output <- output %>% mutate(desc = ifelse(desc == "", NA, desc))
  return(output)
  
}