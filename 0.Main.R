## ---------------------------
##
## Script name: Main script
##
## Project: FUAs
##
## Purpose of script: 
##
## Author: Andrei Wong Espejo
##
## Date Created: 2025-05-08
##
## Email: awonge01@student.bbk.ac.uk
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation

# Create directories
dirs <- c("Data", "Output", "Figures")
lapply(dirs, dir.create)

# Modify gitignore

## Load required packages ----

library("pacman")
library("here")
library("groundhog")

set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2025-01-05"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("dplyr", "tidyverse", "janitor", "sf"
         , "ggplot2","xfun", "remotes", "sp", "spdep"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork"
         , "haven", "openxlsx", "MASS", "reticulate"
         , "future", "furrr", "data.table","leaflet"
         , "jtools", "tidyr", "ggspatial", "raster"
         , "prettymapr", "viridis", "labelled"
         , "writexl", "WDI", "wesanderson", "ggrepel",
         "ggbreak"
)

groundhog.library(pkgs, groundhog.day)

## Runs the following --------
# 1. Load data
# 2. Review data and check spatial and temporal compatability


# 1.Load data and getting to know you ----

efua <- st_read(here("Data"
                    , "GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0"
                    ,"GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg"
                    )
               )
# Consider logical: 1 eFUA with commuting area; 0 eFUA without commuting area => eFUA==UC
# eFUA_name (name of the primary Urban Centre in the eFUA)
# UC_IDs, eFUA_ID

uc <- st_read(here("Data"
                 , "GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2"
                 , "GHS_STAT_UCDB2015MT_GLOBE_R2019A"
                 ,"GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"
                 )
            )
# UC_NM_MN: the main name of the Urban Centre
# UC_NM_LST: full list of assigned names of the Urban Centre
# There are Urban Centres that are cross international borders XBRDR = 1, CTR_MN_ISO main country
# QA2_1V: quality code (0 – false positive, 1 – true positive, >1 uncertain).
# ID_HDC_G0

oefua <- st_read(here("Data", "OE", "OE_FUA_SHAPEFILE.shp"))
# OE_COUNTRY, OE_FUANAME
# OE_FUAID
# 
# NEXT STEPS
# check all the same projections
# st_intersects
# create matching id table
# create dataset for each urban definition



