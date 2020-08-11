files <- list.files("functions", pattern = ".R", full.names = TRUE)

files <- files[-which(files == "functions/functions.R")]
files <- files[!str_detect(files, "internal")]
files <- files[!str_detect(files, "tmp")]

lapply(files, source)

rm(files)
