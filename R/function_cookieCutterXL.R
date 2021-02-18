cookieCutterXL <- function(sf, sfToCut, minusCores = 1) {

  require(sf)
  require(parallel)
  require(doParallel)

  sfToCut <- sfToCut # Needed for embedded parallel functions

  # Split data by geometry type
  print("1) Split data by geometry type")
  types <- st_geometry_type(sf$geometry) %>% as.character()

  iPoint <- which(types == "POINT")
  iPoly <- which(str_detect(types, "POLY"))
  iLine <- which(str_detect(types, "LINE"))
  iOther <- c(iPoint, iPoly, iLine) # Subtract

  sfList <-
    list(sfPoints = sf %>% slice(iPoint),
         sfPolys = sf %>% slice(iPoly),
         sfLines = sf %>% slice(iLine),
         sfOther = sf %>% slice(-iOther))

  # Cut sfLines into chunks based on the number of cores available
  print("2) Cut lines into chunks")
  no_cores <- detectCores(logical = TRUE)
  cut <- cut_number(1:nrow(sfList$sfLines), no_cores-minusCores, labels = FALSE)
  sfList$sfLines <- sfList$sfLines %>% as_tibble %>% mutate(cut = cut) %>% split(., .$cut) %>% modify(. %>% select(-cut) %>% st_as_sf())

  # Prepare first cluster
  print("3a) First cluster")
  cl <- makeCluster(no_cores-minusCores)
  registerDoParallel(cl)
  clusterExport(cl = cl, varlist = list("sfList", "cookieCutter", "sfToCut"), envir = environment())

  # Run first cluster
  print("3b) First cluster")
  cookies <- parLapply(cl, sfList[c("sfPoints", "sfPolys", "sfOther")], function(x) { library(OSMtidy); library(tidyverse); cookieCutter(sf = x, sfToCut = sfToCut) })
  stopCluster(cl)

  # Prepare second cluster
  print("4a) Second cluster")
  cl <- makeCluster(no_cores-minusCores)
  registerDoParallel(cl)
  clusterExport(cl = cl, varlist = list("sfList", "cookieCutter", "sfToCut"), envir = environment())

  # Run second cluster
  print("4b) Second cluster")
  cookies$sfLines <- parLapply(cl, sfList$sfLines, function(x) { library(OSMtidy); library(tidyverse); cookieCutter(sf = x, sfToCut = sfToCut) })
  stopCluster(cl)
  cookies$sfLines <- cookies$sfLines %>% bind_rows()

  # Prepare output
  print("5) Preparing output")
  output <- cookies %>% bind_rows()
  return(output)

}
