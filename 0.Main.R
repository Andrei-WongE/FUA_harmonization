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
## Notes: Country or administrative territories aggregation not considered, see GHS-DUC
##   
##
## ---------------------------

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2(TRUE) # Spherical geometry for spatial operations

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
         "ggbreak", "leaflet.extras", "htmlwidgets"
)

groundhog.library(pkgs, groundhog.day)

## Runs the following --------
# 1. Load data
# 2. Review data and check spatial and temporal compatibility
# 3. Create matching ids table
# 4. Select and create variables according urban definitions
# 5. Create database with geom for each urban definition


# 1.Load data and getting to know you ♥ ----

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

# 2. Review data and check spatial and temporal compatibility ----

## Compare projections
all.equal(st_crs(efua), st_crs(oefua)) && all.equal(st_crs(efua), st_crs(uc))
# [1] FALSE

uc <- st_transform(st_make_valid(uc), st_crs(efua))
oefua <- st_transform(st_make_valid(oefua), st_crs(efua))
efua <- st_make_valid(efua)

if (all.equal(st_crs(efua), st_crs(oefua)) && all.equal(st_crs(efua), st_crs(uc))) {
  # Continue with spatial analysis
  message("CRS match confirmed. Continue...")
  
} else {
  
  stop("CRS mismatch detected. Check transformations.")
}
# [1] TRUE

## Identify spatial overlap
# Check if ALL geometries are contained in oefua
dim(efua)
# [1] 9031   13
dim(oefua)
# [1] 900   5

contained <- st_within(efua, oefua, sparse = FALSE)
not_contained_ind <- which(rowSums(contained) == 0)
oefua_not_contained <- efua[not_contained_ind, ]
(result <- oefua_not_contained[, c("eFUA_ID", "UC_IDs", "eFUA_name")])
# sf [8930, 4]

# Hence, 101 efua FULLY contained in oefua

intersects <- st_intersects(efua, oefua, sparse = FALSE)
not_fully_contained <- which(rowSums(intersects) > 0 & rowSums(contained) == 0)
oefua_not_fuly_contained <- efua[not_fully_contained, ]
(result <- oefua_not_fuly_contained[, c("eFUA_ID", "UC_IDs", "eFUA_name")])
# sf [1385, 4]

# Hence, 1385 efua partially intersect but are not fully contained in oefua
# 7545 efua features have no overlap with oefua 

# Convert to WGS 84 for leaflet
oefua_not_fully_contained <- st_transform(result, 4326) %>% 
                              rename(OE_FUANAME = eFUA_name)

oefua_plot <- st_transform(oefua, 4326)
efua_plot <- st_transform(efua, 4326)

centroids <- st_point_on_surface(oefua_plot)
coords <- st_coordinates(centroids)
mean_lng <- mean(coords[,1])
mean_lat <- mean(coords[,2])

# Extract coordinates as a matrix
coords <- st_coordinates(centroids)

comparison_map <-  leaflet() %>%
                  addTiles() %>%
                  addPolygons(
                    data = oefua_plot, 
                    group = "oefua", # Defining layer name
                    fillColor = "red", 
                    fillOpacity = 0.3, 
                    color = "black", 
                    weight = 1,
                    label = ~OE_FUANAME,
                    popup = ~paste("<b>OE_FUAID:</b>", OE_FUAID, "<br>",
                                   "<b>OE_FUANAME:</b>", OE_FUANAME)
                  ) %>%
                  addPolygons(
                    data = oefua_not_fully_contained, 
                    group = "oefua_not_fully_contained", # Defining layer name
                    fillColor = "blue", 
                    fillOpacity = 0.7, 
                    color = "black", 
                    weight = 1,
                    label = ~OE_FUANAME,
                    popup = ~paste("<b>eFUA_ID:</b>", eFUA_ID, "<br>",
                                   "<b>UC_IDs:</b>", UC_IDs, "<br>",
                                   "<b>eFUA_name:</b>", OE_FUANAME)
                  ) %>%
                  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
                  # addSearchFeatures(
                  #   targetGroups = c("oefua", "oefua_not_fully_contained"), # Search across layers, math layer name
                  #   options = searchFeaturesOptions(
                  #     propertyName = "OE_FUANAME",
                  #     initial = FALSE,
                  #     openPopup = TRUE,
                  #     zoom = 10,
                  #     hideMarkerOnCollapse = FALSE,
                  #     textPlaceholder = "Search oefua name",
                  #     moveToLocation = function(latlng, title, map) {
                  #       map.setView(latlng, 10)
                  #     }
                  #     
                  #   )
                  #   ) %>%
                  addControl(html = "<b>© 2025 DO NOT CITE OR RESUSE WITH OUT MY PERMISION. All rights reserved.</b>", 
                             position = "bottomright") %>%
                  setView(lng = mean(st_coordinates(st_centroid(oefua_plot))[,1]), 
                          lat = mean(st_coordinates(st_centroid(oefua_plot))[,2]), 
                          zoom = 6)

saveWidget(comparison_map
           , file = here("Output","urdef_comparison_map.html")
           , selfcontained = TRUE
           )

# NEXT STEPS
# Select variables according simple sum, areal weigthed sum, population weigted sum or other
# Write code and calculate new variable according urban definition
# Create database for each urban definition
# Create key table to match urban definitions and metadata
