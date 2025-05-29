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
         "ggbreak", "leaflet.extras", "htmlwidgets", "terra"
)

groundhog.library(pkgs, groundhog.day)

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2(TRUE) # Spherical geometry for spatial operations
terraOptions(memfrac = 0.9, todisk = TRUE) # Set memory fraction and write to disk for terra 

# Create directories
dirs <- c("Data", "Output", "Figures")
lapply(dirs, dir.create)

# Modify gitignore

## Runs the following --------
# 1. Load data
# 2. Review data and check spatial and temporal compatibility
# 3. Clip population data
# 4. Create matching ids table
# 5. Select and create variables according urban definitions
# 6. Create database with geom for each urban definition


# 1.Load data and getting to know you ♥ ----

efua <- st_read(here("Data"
                    , "GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0"
                    , "GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg"
                    )
               )
# Consider logical: 1 eFUA with commuting area; 0 eFUA without commuting area => eFUA==UC
# eFUA_name (name of the primary Urban Centre in the eFUA)
# UC_IDs, eFUA_ID

# uc <- st_read(here("Data"
#                  , "GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2"
#                  ,"GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"
#                  )
#             )

uc <- st_read(here("Data"
                   , "GHS_UCDB_GLOBE_R2024A_V1_0"
                   , "GHS_UCDB_GLOBE_R2024A.gpkg"
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

pop_2020 <- terra::rast(here("Data"
                         , "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0"
                         , "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0.tif"
                         ))

# 2. Review data and check spatial and temporal compatibility ----

## Compare projections
st_crs(efua)$wkt
st_crs(oefua)$wkt
st_crs(uc)$wkt
st_crs(pop_2020)$wkt

efua <- st_transform(st_make_valid(efua), st_crs(pop_2020))
oefua <- st_transform(st_make_valid(oefua), st_crs(pop_2020))
uc <- st_transform(st_make_valid(uc), st_crs(pop_2020))

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

# 3. Clip population data -----

# ## Convert to raster, oefua 
# # pop_cropped <- terra::crop(pop_2020, vect(oefua))
# 
# # Reduce to ~2km resolution
# pop_agg <- terra::aggregate(pop_2020, fact = 2, fun = "sum")
# oefua_raster <- terra::rasterize(vect(oefua), pop_agg
#                                  , field = "OE_FUAID"
#                                  # , filename = "efua_temp.tif"
#                                  # , overwrite = TRUE
#                                  )
# 
# ## Clip population data to the extent of OE eFUA
# pop_2020_oefua <- terra::mask(pop_agg, oefua_raster
#                               # , filename = here("Output","pop_2020_oefua_.tif")
#                               # , overwrite = TRUE
# )
# 
# ## Convert to raster, uc
# # pop_cropped <- terra::crop(pop_2020, vect(uc))
# 
# # Reduce to ~2km resolution
# # pop_agg <- terra::aggregate(pop_cropped, fact = 2, fun = "sum")
# uc_raster <- terra::rasterize(vect(uc), pop_agg
#                                  , field = "ID_UC_G0"
#                                  # , filename = "efua_temp.tif"
#                                  # , overwrite = TRUE
#                                 )
# ## Clip population data to the extent of uc
# pop_2020_uc <- terra::mask(pop_agg, uc_raster
#                               # , filename = here("Output","pop_2020_oefua_.tif")
#                               # , overwrite = TRUE
# )
# 
# ## Convert to raster, efua 
# # pop_cropped <- terra::crop(pop_2020, vect(efua))
# 
# # Reduce to ~2km resolution
# # pop_agg <- terra::aggregate(pop_cropped, fact = 2, fun = "sum")
# efua_raster <- terra::rasterize(vect(efua), pop_agg
#                               , field = "eFUA_ID"
#                               # , filename = "efua_temp.tif"
#                               # , overwrite = TRUE
# )
# ## Clip population data to the extent of efua
# pop_2020_efua <- terra::mask(pop_agg, efua_raster
#                               # , filename = here("Output","pop_2020_oefua_.tif")
#                               # , overwrite = TRUE
# )
# 
# 
# # 4. Create matching ids table ----
# 
# ## Step 1: Find uc polygons fully within oefua
# within_check <- terra::zonal(pop_2020_oefua, pop_2020_uc, fun = "all", na.rm = TRUE)
# within_ids <- within_check[within_check[,2] == 1, 1]
# 
# ## Step 2: For uc polygons that intersect but aren't fully within
# intersect_raster <- terra::mask(pop_2020_uc, pop_2020_oefua)
# uc_ids <- unique(pop_2020_uc[!is.na(pop_2020_uc)])
# intersecting_ids <- uc_ids[!uc_ids %in% within_ids]
# 
# ## Step 3: Apply 50% rule to intersecting uc polygons
# pop_totals_uc <- terra::zonal(pop_2020_uc, pop_2020_uc, fun = "sum", na.rm = TRUE)
# pop_intersect_uc <- terra::zonal(intersect_raster, pop_2020, fun = "sum", na.rm = TRUE)
# 
# ## Step 4: Calculate ratios for uc polygons
# pop_ratios_uc <- pop_intersect_uc[,2] / pop_totals_uc[,2]
# intersects_condition <- pop_ratios_uc > 0.5
# 
# intersects_ids <- uc_ids[intersects_condition & uc_ids %in% intersecting_ids]
# intersects_raster <- pop_2020_uc %in% intersects_ids


tryCatch({
## Step 1: Create rasters
pop_agg <- terra::aggregate(pop_2020, fact = 2, fun = "sum")

# Verify resolution and extent aliunment
terra::res(oefua_raster)
# [1] 2000 2000
terra::res(uc_raster)
# [1] 2000 2000
terra::res(efua_raster)
# [1] 2000 2000
terra::ext(oefua_raster)
# SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)
terra::ext(uc_raster)
# SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)
terra::ext(efua_raster)
# SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)


# Rasterize sf objects to match pop_agg
oefua_raster <- terra::rasterize(vect(oefua), pop_agg, field = "OE_FUAID")
uc_raster <- terra::rasterize(vect(uc), pop_agg, field = "ID_UC_G0")
efua_raster <- terra::rasterize(vect(efua), pop_agg, field = "eFUA_ID")

## Step 2: Find intersections conditional on pixel with population and spatial overlap 
area_intersections <- function(area_raster1, area_raster2, pop_raster) {
  area_ids <- unique(area_raster1[!is.na(area_raster1)])
  intersections <- data.frame()
  
  for(area_id in area_ids) {
    # Create mask
    area_mask <- area_raster1 == area_id
    
    # Find oefua areas that have population overlap with this area
    overlapping_cells <- area_mask & !is.na(area_raster2) & !is.na(pop_raster)
    
    if(terra::global(overlapping_cells, fun = "sum", na.rm = TRUE)[1,1] > 0) {
      overlapping_oefua <- unique(area_raster2[overlapping_cells])
      overlapping_oefua <- overlapping_oefua[!is.na(overlapping_oefua)]
      
      if(length(overlapping_oefua) > 0) {
        intersections <- rbind(intersections, 
                               data.frame(area_id = area_id, 
                                          oefua_id = overlapping_oefua))
      }
    }
  }
  return(intersections)
}

# Find uc intersections with oefua
uc_intersections <- area_intersections(uc_raster, oefua_raster, pop_agg)
names(uc_intersections) <- c("uc_id", "oefua_id")

# Find efua intersections with oefua
efua_intersections <- area_intersections(efua_raster, oefua_raster, pop_agg)
names(efua_intersections) <- c("efua_id", "oefua_id")

# Check for multiple intersections
uc_multi <- uc_intersections %>% 
  group_by(uc_id) %>% 
  summarise(n_oefua = n()) %>% 
  filter(n_oefua > 1)

efua_multi <- efua_intersections %>% 
  group_by(efua_id) %>% 
  summarise(n_oefua = n()) %>% 
  filter(n_oefua > 1)

if(nrow(uc_multi) > 0) {
  warning(paste("UC areas intersecting multiple OEFUA:", paste(uc_multi$uc_id, collapse = ", ")))
}

if(nrow(efua_multi) > 0) {
  warning(paste("EFUA areas intersecting multiple OEFUA:", paste(efua_multi$efua_id, collapse = ", ")))
}

## Step 3: Apply 50% population rule 

# Function to apply 50% rule
selection_rule <- function(area_raster, area_intersections, area_name) {
  
  # Calculate total population for each area
  total_pop <- terra::zonal(pop_agg, area_raster, fun = "sum", na.rm = TRUE)
  names(total_pop) <- c("area_id", "total_pop")
  
  qualifying_areas <- data.frame()
  
  for(i in 1:nrow(area_intersections)) {
    area_id <- area_intersections[i, 1]
    oefua_id <- area_intersections[i, 2]
    
    # Create intersection mask
    area_mask <- area_raster == area_id
    oefua_mask <- oefua_raster == oefua_id
    intersection_mask <- area_mask & oefua_mask
    
    # Calculate intersection population
    intersect_pop <- terra::global(pop_agg * intersection_mask, fun = "sum", na.rm = TRUE)[1,1]
    
    # Get total population for this area
    total_pop_area <- total_pop[total_pop$area_id == area_id, "total_pop"]
    
    # Apply 50% rule
    if(length(total_pop_area) > 0 && total_pop_area > 0) {
      ratio <- intersect_pop / total_pop_area
      if(ratio >= 0.5) {
        qualifying_areas <- rbind(qualifying_areas, 
                                  data.frame(area_id = area_id, 
                                             oefua_id = oefua_id,
                                             population = intersect_pop))
      }
    }
  }
  
  return(qualifying_areas)
}

# Apply rule to areas
uc_qualifying <- selection_rule(uc_raster, uc_intersections, "UC")
efua_qualifying <- selection_rule(efua_raster, efua_intersections, "EFUA")

# Check for OEFUA with no qualifying areas
# oefua_ids <- unique(oefua$OE_FUAID)
# uc_oefua_covered <- unique(uc_qualifying$oefua_id)
# efua_oefua_covered <- unique(efua_qualifying$oefua_id)
# 
# uc_missing <- setdiff(oefua_ids, uc_oefua_covered)
# efua_missing <- setdiff(oefua_ids, efua_oefua_covered)
# 
# if(length(uc_missing) > 0) {
#   warning(paste("OEFUA with no qualifying UC areas:", paste(uc_missing, collapse = ", ")))
# }
# 
# if(length(efua_missing) > 0) {
#   warning(paste("OEFUA with no qualifying EFUA areas:", paste(efua_missing, collapse = ", ")))
# }

## Step 4: Identify main city centers 

# UC main centers
uc_main_centers <- uc_qualifying %>%
  group_by(oefua_id) %>%
  mutate(Main_city_center = ifelse(population == max(population), 1, 0)) %>%
  select(Id = area_id, Main_city_center, oefua_id) %>%
  ungroup()

# EFUA main centers
efua_main_centers <- efua_qualifying %>%
  group_by(oefua_id) %>%
  mutate(Main_city_center = ifelse(population == max(population), 1, 0)) %>%
  select(Id = area_id, Main_city_center, oefua_id) %>%
  ungroup()

## Step 5: Create output tables 

write.csv(uc_main_centers, "uc_table.csv", row.names = FALSE)
write.csv(efua_main_centers, "efua_table.csv", row.names = FALSE)

cat("UC qualifying areas:", nrow(uc_main_centers), "\n")
cat("EFUA qualifying areas:", nrow(efua_main_centers), "\n")
cat("Analysis complete!\n")

body = "Your R script has finished running!"
source(here("Data","pushsaver.R"))

}, error = function(e) {
  
  body = paste("Error in script:", e$message)
  source(here("Data","pushsaver.R"))
  
})


# NEXT STEPS
# Select variables according simple sum, areal weigthed sum, population weigted sum or other
# Write code and calculate new variable according urban definition
# Create database for each urban definition
# Create key table to match urban definitions and metadata


