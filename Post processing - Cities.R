# -------------------------------------------------------------------------
# Post-processing - Cities.R
# Dr Annie Visser-Quinn, a.visser-quinn@hw.ac.uk
#
# Created: 2020-08-12
# Last revised: 2020-08-13
# -------------------------------------------------------------------------

rm(list = ls()); cat("/014"); gc(verbose = TRUE) 

setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd() 

pacman::p_load(tidyverse, sf, pbapply)

source("functions/functions.R")


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# READ DATA ---------------------------------------------------------------
# -------------------------------------------------------------------------
dir = file.path("../../../Dropbox (Heriot-Watt University Team)/",
             "RES_EGIS_Water_Resilient_Cities/WRC - Data archive/",
             "OSMtidy-AHgen development (Before github 2020-08-13)/",
             "OSMtidy V0.0.3c", fsep = "") %>% normalizePath
dir

files <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = ".RDS")
files <- files[str_detect(files, "_6_dataTidy-filtered")]
files <- files[c(1,2,4)]
files

cities <- c("Bristol", "Edinburgh", "Manchester")

dl <- lapply(files, readRDS) %>% setNames(cities)
dl


# -------------------------------------------------------------------------
# makeValid() -------------------------------------------------------------
# -------------------------------------------------------------------------
dl2 <- 
  pblapply(dl, function(x) { 
    tryCatch(x %>% makeValid, 
             error = function(e) NULL) 
    })
dl2


# -------------------------------------------------------------------------
# simplify ----------------------------------------------------------------
# -------------------------------------------------------------------------
dl3 <- 
  pblapply(dl2, function(x) { 
  tryCatch(x %>%
             simplifyGolf(rbind = TRUE) %>% 
             simplifyRail(rbind = TRUE) %>% 
             simplifyAirports(rbind = TRUE),
           error = function(e) NULL)
})
dl3

# Error for Bristol because no airports
dl3$Bristol <- 
  dl2$Bristol %>%
  simplifyGolf(rbind = TRUE) %>% 
  simplifyRail(rbind = TRUE)
dl3


# -------------------------------------------------------------------------
# export ------------------------------------------------------------------
# -------------------------------------------------------------------------
cities2 <- paste0(cities, "_City"); cities2

pblapply(1:length(dl3), function(x) {
  
  dl3[[x]] %>% exportOSMtidy(cities2[[x]], ".RDS")
  
})