.dataImport <- function(vecExcel) {
  
  require(openxlsx)
  require(purrr)
  
  dl <- lapply(1:length(vecExcel), function(i) {
    
    lapply(vecExcel[[i]] %>% getSheetNames(), function(x) {
      
      read.xlsx(vecExcel[[i]], x) %>%
        as_tibble
      
    })
    
  }) %>%
    
    flatten()
  
  return(dl)
  
}