.rmCols <- function(input) { 
  input %>% Filter(function(x) !all(is.na(x)), .) 
  }