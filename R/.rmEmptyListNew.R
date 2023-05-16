.rmEmptyListNew <- function(input) { 
  input[ sapply(input, function(x) { !is.null(dim(x)[1]) }) ] 
  }