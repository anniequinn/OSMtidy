# -------------------------------------------------------------------------
# OSMtidy V0.0.41 Post-processing.R
# Dr Annie Visser-Quinn, a.visser-quinn@hw.ac.uk
#
# Created: 2020-08-12
# -------------------------------------------------------------------------

# Prepare the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE) 

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd() 

# Load the required packages
library(tidyverse)
library(sf)

# Read in the functions that make up OSMtidy V0.0.4
source("functions/functions.R")


# -------------------------------------------------------------------------


dg <- readRDS("vignettes/files/postProcessing/Manchester_City_6_dataTidy-filtered_20200712-170959.RDS")
dg

dg <- dg %>% makeValid; dg

# The output could be exported now using exportOSMtidy()
# Or after application of the three simplifying functions

dg %>% simplifyGolf(rbind = FALSE)
dg %>% nrow
dg <- dg %>% simplifyGolf(rbind = TRUE)
dg %>% nrow
dg %>% tail

dg %>% simplifyRail(rbind = FALSE)
dg %>% nrow
dg <- dg %>% simplifyRail(rbind = TRUE)
dg %>% nrow
dg %>% tail

dg %>% simplifyAirports(rbind = FALSE)
dg %>% nrow
dg <- dg %>% simplifyAirports(rbind = TRUE)
dg %>% nrow
dg %>% tail

# Post processing is complete, export the final output from OSMtidy in one of three formats:
  # .RDS
  # .csv
  # .shp

exportOSMtidy(dg, "Manchester_City", ".RDS")
exportOSMtidy(dg, "Manchester_City", ".csv")
exportOSMtidy(dg, "Manchester_City", ".shp")