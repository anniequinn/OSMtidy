# <<<<<<< Updated upstream
# cookieCutter <- function(sf, sfToCut, quiet = TRUE) {
#   
#   internalFunction <- function(sf, sfToCut) {
# =======
# cookieCutter <- function(sf, sf_toCutOut, quiet = TRUE) {
#   
#   internalFunction <- function(sf, sf_toCutOut) { 
#     
#     sf_toCutOut <- sf_toCutOut %>% summarise
#     
#     index <- sf %>% st_intersects(sf_toCutOut) %>% lengths > 0 # Impacted by hazard
#     
#     cutout <- sf[index,] %>% st_intersection(sf_toCutOut) # Subset (cut)
# >>>>>>> Stashed changes
#     
#     sfToCut <- sfToCut %>% summarise
#     index <- sf %>% st_intersects(sfToCut) %>% lengths > 0 # Impacted by hazard
#     cutout <- sf[index,] %>% st_intersection(sfToCut) # Subset (cut)
#     return(cutout)
#     
#   }
#   
# <<<<<<< Updated upstream
#   if(quiet == FALSE) { output <- internalFunction(sf, sfToCut) }
#   
#   if(quiet == TRUE) { output <-
#     suppressWarnings(
#       suppressMessages(
#         internalFunction(
#           sf, sfToCut
# =======
#   if(quiet == FALSE) { output <- internalFunction(sf, sf_toCutOut) }
#   if(quiet == TRUE) { output <- 
#     suppressWarnings( 
#       suppressMessages(
#         internalFunction(
#           sf, sf_toCutOut
# >>>>>>> Stashed changes
#         )
#       )
#     )
#   }
#   
#   return(output)
#   
# }