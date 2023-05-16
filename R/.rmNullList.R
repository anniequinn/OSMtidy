.rmNullList <- function(input) { 
  input %>% Filter(Negate(is.null), .) 
  }