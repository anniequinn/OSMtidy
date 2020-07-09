# -------------------------------------------------------------------------
# OSMtidy V0.0.4 - walkthrough.R
# Annie Visser-Quinn, a.visser-quinn@hw.ac.uk
#
# Created: 2020-05-12
# Last revised: 2020-07-09
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
library(pbapply)
library(progress)
library(data.table)
library(readxl)
library(openxlsx)


# -------------------------------------------------------------------------
# SET-UP ------------------------------------------------------------------
# -------------------------------------------------------------------------
# Read in the functions that make up OSMtidy V0.0.3c
source("functions/functions.R")

# Print the functions in the environment
ls() 

# Overview of filters 
filterOverview("filters.xlsx")

# Export overview of filters as an output
# The filesave location is printed after the function is run
filterOverview("filters.xlsx") %>% dataExport





# -------------------------------------------------------------------------
# 1. DATA -----------------------------------------------------------
# -------------------------------------------------------------------------

# 1.1 OVERVIEW ------------------------------------------------------------
# Read in location shapefile for which data is to be extracted


# 1.2 ARGUMENTS -----------------------------------------------------------
dataShapefile %>% args

# name = Location name. Should be the same as name of the shapefile in the 
# shapefiles folder (without the .shp file extension)
# crs = Coordinate projection, optional. Function converts to epsg:4326


# 1.3 dataShapefile -------------------------------------------------------
locationName <- "exampleEdinburgh"
locationName

shp <- dataShapefile(name = locationName)


# 1.4 OUTPUT & EXPORT -----------------------------------------------------
shp

# Print a summary
shp %>% dataSummary

# Another way to visualise the shapefile, extract the attribute "plot"
attr(shp, "plot")

# Export the shapefile
# The filesave location is printed after the function is run
dataExport(data = shp, name = locationName)





# -------------------------------------------------------------------------
# 2. DATA EXTRACTION ------------------------------------------------------
# -------------------------------------------------------------------------

# 2.1 OVERVIEW ------------------------------------------------------------
# Extract the data from OpenStreetMap using the function dataExtract()
# Timestamps and progress are printed when the function is running

# OSMtidy extracts 47 of 209 available features in osmdata
# Edit functions/features.txt to change the features extracted
# A list of the available features in osmdata is available via the function
# osmdata::available_features()


# 2.2 ARGUMENTS -----------------------------------------------------------
dataExtract %>% args

# dataShapefile = Output from step 1
# timeout = Time in seconds before the query is timed out; see DETAILS
## Default to 300 seconds
# memsize = Memory size for the overpass server; see DETAILS
## Default to 1073741824 Bytes

# DETAILS - From the R package osmdata
## timeout	
## It may be necessary to increase this value for large queries, because 
## the server may time out before all data are delivered.
## memsize	
## The default memory size for the 'overpass' server in bytes; may need to 
## be increased in order to handle large queries.
##
## See https://wiki.openstreetmap.org/wiki/Overpass_API#Resource_management_options_.28osm-script.29 
## for explanation of timeout and memsize (or maxsize in overpass terms). 
## Note in particular the comment that queries with arbitrarily large 
## memsize are likely to be rejected.


# 2.3 dataExtract ---------------------------------------------------------
dlExtract <- dataExtract(dataShapefile = shp)


# 2.4 OUTPUT & EXPORT -----------------------------------------------------
# Print a summary
dlExtract %>% dataSummary

# Export the extracted data
# The filesave location is printed after the function is run
dataExport(data = dlExtract, name = locationName)





# -------------------------------------------------------------------------
# 3. DATA CUT -------------------------------------------------------------
# -------------------------------------------------------------------------

# 3.1 OVERVIEW ------------------------------------------------------------
# In step 2, the data was extracted as a "bounding box" (a rectangle)
# The data is cut to the shapefile using the function dataCut()
# Timestamps and progress are printed when the function is running


# 3.2 ARGUMENTS -----------------------------------------------------------
dataCut %>% args

# dataExtracted = Output from step 2
# dataShapefile = Output from step 1


# 3.3 dataCut -------------------------------------------------------------
dlCut <- dataCut(dataExtracted = dlExtract, dataShapefile = shp)


# 3.4 OUTPUT & EXPORT -----------------------------------------------------
# Print a summary
dlCut %>% dataSummary

# Export the cut data
dataExport(data = dlCut, name = locationName)





# -------------------------------------------------------------------------
# 4. DATA WRANGLING -------------------------------------------------------
# -------------------------------------------------------------------------

# 4.1 OVERVIEW ------------------------------------------------------------
# Tidy up (or wrangle) the data ready for filtering using the function 
# dataWrangle()
# Timestamps and progress are printed when the function is running


# 4.2 ARGUMENTS -----------------------------------------------------------
dataWrangle %>% args

# dataCut = Output from step 3


# 4.3 dataWrangle ---------------------------------------------------------
dlWrangle <- dataWrangle(dataCut = dlCut)


# 4.4 OUTPUT & EXPORT -----------------------------------------------------
# Print a summary
dlWrangle %>% dataSummary

# Export data
dataExport(data = dlWrangle, name = locationName)





# -------------------------------------------------------------------------
# 5. DATA FILTERING -------------------------------------------------------
# -------------------------------------------------------------------------

# 5.1 OVERVIEW ------------------------------------------------------------
# Filter the data using the function dataFilter()
# Timestamps and progress are printed when the function is running


# 5.2 ARGUMENTS -----------------------------------------------------------
dataFilter %>% args

# dataWrangle = Output from step 4
# filters = An editable spreadsheet of the filters to apply
## Default to filters.xlsx
## Use the function filterOverview() for an overview
filterOverview("filters.xlsx")
# rows = Specify filter rows
## Default to NULL, i.e. all filters
## For troubleshooting and adjusting filters


# 5.3 dataFilter ----------------------------------------------------------
dlFilter <- dataFilter(dataWrangle = dlWrangle, filters = "filters.xlsx")


# 5.4 OUTPUT & EXPORT -----------------------------------------------------
dlFilter %>% dataSummary

dataExport(data = dlFilter, name = locationName)





# -------------------------------------------------------------------------
# 6. DATA TIDY ------------------------------------------------------------
# -------------------------------------------------------------------------

# 6.1 OVERVIEW ------------------------------------------------------------
# Generate a single tidied output based on all filtered, validated, 
# unfiltered and no detail data


# 6.2 ARGUMENTS -----------------------------------------------------------
dataTidy %>% args

# List of objects and/or file names to tidy
# Inputs can be from dataWrangle$noDetail or any output from dataFilter
# Inputs can be from objects in R or manually adjusted xlsx files may be 
# imported from the outputs folder


# 6.3 dataTidy ------------------------------------------------------------
dlTidy <- 
  dataTidy(dataList = 
             list(dlWrangle$noDetail,
                  dlFilter$unfiltered,
                  dlFilter$filtered,
                  dlFilter$validate))
dlTidy

# If making changes in Excel files then use file directory

dataTidy(dataList = 
           list("outputs/exampleEdinburgh_4_dataWrangle-noDetail_20200709-082031.xlsx",
                dlFilter$unfiltered,
                dlFilter$filtered,
                "outputs/exampleEdinburgh_5_dataFilter-validate_20200709-082605.xlsx"))


# 6.4 OUTPUT & EXPORT -----------------------------------------------------
dlTidy %>% dataSummary

dataExport(data = dlTidy, name = locationName)


# -------------------------------------------------------------------------
# UTILITY FUNCTION --------------------------------------------------------
# -------------------------------------------------------------------------

# A.1 - OVERVIEW ----------------------------------------------------------

# A function that cuts out spatial data which lies within a given shapefile


# A.2 - ARGUMENTS ---------------------------------------------------------
cookieCutter %>% args

# dataInput = A spatial dataframe to be cut out from
# dataShapefile = A shapefile outlining the area to be cut


# A.3 cookieCutter --------------------------------------------------------
# Example shape to cut out
shpCut <- 
  st_make_grid(shp, cellsize = 0.01) %>% 
  as_tibble %>% 
  st_as_sf %>% 
  mutate(labs = letters[1:n()]) %>%
  filter(labs %in% c("b", "c")) %>% 
  select(-labs)
shpCut

filteredCut <- cookieCutter(dataInput = dlTidy$filtered, dataShapefile = shpCut)
filteredCut

shp %>% ggplot() + geom_sf() + geom_sf(data = filteredCut)