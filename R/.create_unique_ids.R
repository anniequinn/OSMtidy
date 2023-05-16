.create_unique_ids <- function(n, seed_no = 1, char_len = 5){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}