rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, sf, leaflet)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")

# Post processing

file <- list.files("vignettes/files/extension", 
                   pattern = "Manchester", 
                   full.names = TRUE)
file

input <- readRDS(file)
input

# Reduce down to 3 geometry types & validate geometry
dg <- input %>% makeValid

##### dg %>% saveShapefile

# plot 
# dg %>% filter(str_detect(desc, "(golf)")) 
# dg_golfBefore %>% function_simpleMap()

dg %>% simplifyGolf(rbind = FALSE)
dg2 <- dg %>% simplifyGolf(rbind = TRUE); dg2

dg2 %>% simplifyRail(rbind = FALSE)
dg3 <- dg2 %>% simplifyRail(rbind = TRUE); dg3

dg3 %>% simplifyAirports(rbind = FALSE)
dg4 <- dg3 %>% simplifyAirports(rbind = TRUE); dg4