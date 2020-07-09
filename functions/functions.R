files <- list.files("functions", pattern = ".R", full.names = TRUE)

files <- files[-which(files == "functions/functions.R")]
files <- files[-which(files == "functions/functions_internal.R")]


files

lapply(files, source)

rm(files)