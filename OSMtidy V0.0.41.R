# -------------------------------------------------------------------------
# OSMtidy V0.0.41.R
# Dr Annie Visser-Quinn, a.visser-quinn@hw.ac.uk
#
# Created: 2020-07-13
# -------------------------------------------------------------------------

# Prepare the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE) 

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd() 

# Load the required packages
library(tidyverse)
library(lubridate)
library(osmdata)
library(sf)
library(lwgeom)
library(pbapply)
library(progress)
library(data.table)
library(readxl)
library(openxlsx)
library(xlsx)

# Read in the functions that make up OSMtidy V0.0.41
source("functions/functions.R")


# -------------------------------------------------------------------------


# Prepare exampleEdinburgh shapefile
# See Vignette 1 for details

st_read("shapefiles/district_borough_unitary_ward_region.shp", quiet = TRUE) %>%
  filter(str_detect(FILE_NAME, "CITY_OF_EDINBURGH")) %>% 
  filter(str_detect(NAME, "Leith Walk")) %>%
  select(geometry) %>%
  st_simplify(dTolerance = 25) %>% 
  st_transform(4326) %>%
  st_write("shapefiles/exampleEdinburgh.shp", quiet = TRUE)


# -------------------------------------------------------------------------


# 1. DATA INPUT
locationName <- "exampleEdinburgh"; locationName
shp <- dataShapefile(name = locationName)

shp %>% dataSummary
dataExport(data = shp, name = locationName)


# 2. DATA EXTRACT
dlExtract <- dataExtract(dataShapefile = shp)

dlExtract %>% dataSummary
dataExport(data = dlExtract, name = locationName)


# 3. DATA CUT
dlCut <- dataCut(dataExtracted = dlExtract, dataShapefile = shp)

dlCut %>% dataSummary
dataExport(data = dlCut, name = locationName)


# 4. DATA WRANGLE
dlWrangle <- dataWrangle(dataCut = dlCut)

dlWrangle %>% dataSummary
dataExport(data = dlWrangle, name = locationName)


# 5. DATA FILTER
filterOverview("filters.xlsx")

dlFilter <- dataFilter(dataWrangle = dlWrangle, filters = "filters.xlsx")

dlFilter %>% dataSummary
dataExport(data = dlFilter, name = locationName)


# 6. DATA TIDY
dlTidy <- 
  dataTidy(dataList = # If making changes in Excel files then use file directory
             list(dlWrangle$noDetail,
                  dlFilter$unfiltered,
                  dlFilter$filtered,
                  dlFilter$validate))

dlTidy %>% dataSummary
dlTidy
dataExport(data = dlTidy, name = locationName)