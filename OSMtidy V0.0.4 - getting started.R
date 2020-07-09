# -------------------------------------------------------------------------
# OSMtidy V0.0.3c - getting started.R
# Annie Visser-Quinn, a.visser-quinn@hw.ac.uk
#
# Created: 2020-07-09
# Last revised: 2020-07-09
# -------------------------------------------------------------------------

# Prepare the environment
rm(list = ls()); cat("/014"); gc() 

# Load the required packages
library(tidyverse)
library(sf)

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd() 


# All shapefile data is made available Open Source by the Ordnance Survey:
# https://www.ordnancesurvey.co.uk/business-government/products/boundaryline
# Please reference accordingly

# Data used in the walkthrough is:
# OS OpenData - Unitary authorities
# district_borough_unitary_ward_region.shp

# Select a single ward to focus on
dt <- 
  st_read("shapefiles/district_borough_unitary_ward_region.shp") %>% 
  filter(str_detect(FILE_NAME, "CITY_OF_EDINBURGH")) %>% 
  filter(str_detect(NAME, "Leith Walk")) %>%
  select(geometry)
dt

# Visualise the ward outline
dt %>% ggplot() + geom_sf()

# Simplify the outline to speed up OSMtidy
# And convert CRS to epsg:4326
dt <- dt %>% st_simplify(dTolerance = 25) %>% st_transform(4326)

# Visualise the ward outline again
dt %>% ggplot() + geom_sf()

# Export shapefile
dt %>% st_write("shapefiles/exampleEdinburgh.shp")