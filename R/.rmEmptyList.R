.rmEmptyList <- function(input) { 
  input %>% Filter(function(x) dim(x)[1] > 0, .) 
  }
