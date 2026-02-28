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
groundhog.day = "2025-09-22"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("tidyverse", "janitor", "sf"
         , "ggplot2","xfun", "remotes", "sp", "spdep"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork"
         , "haven", "openxlsx", "MASS", "reticulate"
         , "future", "furrr", "data.table","leaflet"
         , "jtools", "tidyr", "ggspatial", "raster"
         , "prettymapr", "viridis", "labelled"
         , "writexl", "WDI", "wesanderson", "ggrepel"
         , "ggbreak", "leaflet.extras", "htmlwidgets", "terra"
         , "httr"
)

groundhog.library(pkgs, groundhog.day)

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2(TRUE) # Spherical geometry for spatial operations
terraOptions(memfrac = 0.9, todisk = TRUE) # Set memory fraction and write to disk for terra 

# Create directories
dirs <- c("Data", "Output", "Figures")
lapply(dirs, dir.create)
source("Utils.R")

# Modify gitignore

## Runs the following --------
# 1. Load data
# 2. Review data and check spatial and temporal compatibility
# 3. Clip population data and Create matching ids table and 
# 4. Select and create variables according urban definitions
# 5. Create database with geom for each urban definition


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

uc_all <- read_gpkg_layers(here("Data", "GHS_UCDB_GLOBE_R2024A_V1_0", "GHS_UCDB_GLOBE_R2024A.gpkg")
                 , selected_layers = NULL
                 , quiet = FALSE)

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

uc_2019 <- st_read(here("Data"
                   , "GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2"
                   , "GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"
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

# 3. Clip population data and matching id table-----

# Using vector approach instead of raster, due to the size of pop raster
  pop_agg <- terra::aggregate(pop_2020, fact = 2, fun = "sum") 
  
  names(pop_agg) <- "total_pop"
  
  ## Step 1: Calculate total population for each area using vector-raster extraction

  # Extract total population for each urban area
  uc_pop <- terra::extract(pop_agg, terra::vect(uc), fun = "sum", na.rm = TRUE, ID = TRUE) # maps to original polygon, preserves order
  efua_pop <- terra::extract(pop_agg, terra::vect(efua), fun = "sum", na.rm = TRUE, ID = TRUE)
  
  # As extract returns separte data frames, merge back to original data
  uc$total_pop <- uc_pop$total_pop
  efua$total_pop <- efua_pop$total_pop
  
  gc()
  
  ## Step 2: Find spatial intersections using oefua based cluster selection

  # Set up parallel processing
  plan(multisession, workers = parallel::detectCores() - 1)
  
  # Use oefua as spatial reference for chunking
  oefua_bbox <- sf::st_bbox(oefua)
  
  # Adaptive grid based on OEFUA density
  oefua_density <- nrow(oefua) / 1000
  grid_n <- if(oefua_density > 50) c(8, 4) else c(6, 3)
  
  world_grid <- sf::st_make_grid(oefua_bbox, n = grid_n)
  
  # Process each spatial tile for both uc and efua
  process_tile <- function(tile) {
    # Filter OEFUA (primary reference)
    oefua_tile <- sf::st_filter(oefua, tile)
    
    if(nrow(oefua_tile) == 0) return(list(uc = NULL, efua = NULL))
    
    # Filter uc and efua by tile
    uc_tile <- sf::st_filter(uc, tile)
    efua_tile <- sf::st_filter(efua, tile)
    
    # UC intersections
    uc_result <- NULL
    if(nrow(uc_tile) > 0) {
      uc_result <- sf::st_intersection(uc_tile, oefua_tile)
    }
    
    # EFUA intersections
    efua_result <- NULL
    if(nrow(efua_tile) > 0) {
      efua_result <- sf::st_intersection(efua_tile, oefua_tile)
    }
    
    return(list(uc = uc_result, efua = efua_result))
    gc()
  }
  
  # Intersection in parallel by geographic tiles
  tile_results <- future_map(world_grid, process_tile)
  
  uc_intersections <- map_dfr(tile_results, ~.x$uc)
  efua_intersections <- map_dfr(tile_results, ~.x$efua)
  
  ## Step 3: Extract population for intersection areas

  # Extract population for intersection polygons
  uc_intersect_pop <- terra::extract(pop_agg, terra::vect(uc_intersections), 
                                     fun = "sum", na.rm = TRUE, ID = TRUE)
  efua_intersect_pop <- terra::extract(pop_agg, terra::vect(efua_intersections), 
                                       fun = "sum", na.rm = TRUE, ID = TRUE)
  
  # Add intersection population
  uc_intersections$intersect_pop <- replace_na(uc_intersect_pop$total_pop, 0)
  efua_intersections$intersect_pop <- replace_na(efua_intersect_pop$total_pop, 0)
  
  ## Step 4: Apply 50% population rule

  # Calculate population ratios with NA handling
  uc_intersections$pop_ratio <- ifelse(uc_intersections$total_pop > 0,
                                       uc_intersections$intersect_pop / uc_intersections$total_pop, 
                                       0)
  efua_intersections$pop_ratio <- ifelse(efua_intersections$total_pop > 0,
                                         efua_intersections$intersect_pop / efua_intersections$total_pop, 
                                         0)
  
  # Filter qualifying areas (≥50% population overlap)
  uc_qualifying <- uc_intersections[uc_intersections$pop_ratio >= 0.5, ]
  efua_qualifying <- efua_intersections[efua_intersections$pop_ratio >= 0.5, ]
  
  ## Step 5: Identify main city centers

  # Create data frames for processing
  uc_centers_df <- data.frame(
    Id = uc_qualifying$ID_UC_G0,
    oefua_id = uc_qualifying$OE_FUAID,
    population = uc_qualifying$intersect_pop
  )
  
  efua_centers_df <- data.frame(
    Id = efua_qualifying$eFUA_ID,
    oefua_id = efua_qualifying$OE_FUAID,
    population = efua_qualifying$intersect_pop
  )
  
  # Identify main centers (highest population per OEFUA)
  uc_main_centers <- uc_centers_df %>%
    group_by(oefua_id) %>%
    mutate(Main_city_center = ifelse(population == max(population, na.rm = TRUE), 1, 0)) %>%
    dplyr::select(Id, Main_city_center, oefua_id) %>%
    ungroup()
  
  efua_main_centers <- efua_centers_df %>%
    group_by(oefua_id) %>%
    mutate(Main_city_center = ifelse(population == max(population, na.rm = TRUE), 1, 0)) %>%
    dplyr::select(Id, Main_city_center, oefua_id) %>%
    ungroup()
  
  ## Step 6: Quality checks

  # Check for multiple intersections
  uc_multi <- uc_centers_df %>% 
    group_by(Id) %>% 
    summarise(n_oefua = n(), .groups = 'drop') %>% 
    filter(n_oefua > 1)
  
  efua_multi <- efua_centers_df %>% 
    group_by(Id) %>% 
    summarise(n_oefua = n(), .groups = 'drop') %>% 
    filter(n_oefua > 1)
  
  if(nrow(uc_multi) > 0) {
    warning(paste("UC areas intersecting multiple OEFUA:", nrow(uc_multi)))
    write_csv(uc_multi, here("Output", "uc_multi_intersections.csv"))
  }
  
  if(nrow(efua_multi) > 0) {
    warning(paste("EFUA areas intersecting multiple OEFUA:", nrow(efua_multi)))
    write_csv(efua_multi, here("Output", "efua_multi_intersections.csv"))
  }
  
  ## Step 7: Export results

  write_csv(uc_main_centers, here("Output", "uc_OE_table.csv"))
  write_csv(efua_main_centers, here("Output", "efua_OE_table.csv"))
  
  # Summary statistics
  cat("Analysis Complete!\n")
  cat("UC qualifying areas:", nrow(uc_main_centers), "\n")
  cat("EFUA qualifying areas:", nrow(efua_main_centers), "\n")
  cat("UC areas with multiple OEFUA intersections:", nrow(uc_multi), "\n")
  cat("EFUA areas with multiple OEFUA intersections:", nrow(efua_multi), "\n")

  gc()

# NEXT
# Mapping problematic intersections, check intersection conditions, its ucs and efuas with multiple oefuas!



# tryCatch({
#   
# message("Starting process...")
#   
# ## Step 1: Create rasters
# pop_agg <- terra::aggregate(pop_2020, fact = 2, fun = "sum")
# 
# # Verify resolution and extent aliunment
# terra::res(oefua_raster)
# # [1] 2000 2000
# terra::res(uc_raster)
# # [1] 2000 2000
# terra::res(efua_raster)
# # [1] 2000 2000
# terra::ext(oefua_raster)
# # SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)
# terra::ext(uc_raster)
# # SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)
# terra::ext(efua_raster)
# # SpatExtent : -18041000, 18041000, -9000000, 9000000 (xmin, xmax, ymin, ymax)
# 
# 
# # Rasterize sf objects to match pop_agg
# oefua_raster <- terra::rasterize(vect(oefua), pop_agg, field = "OE_FUAID")
# uc_raster <- terra::rasterize(vect(uc), pop_agg, field = "ID_UC_G0")
# efua_raster <- terra::rasterize(vect(efua), pop_agg, field = "eFUA_ID")
# 
# ## Step 2: Find intersections conditional on pixel with population and spatial overlap 
# area_intersections <- function(area_raster1, area_raster2, pop_raster) {
#   area_ids <- unique(area_raster1[!is.na(area_raster1)])
#   intersections <- data.frame()
#   
#   for(area_id in area_ids) {
#     # Create mask
#     area_mask <- area_raster1 == area_id
#     
#     # Find oefua areas that have population overlap with this area
#     overlapping_cells <- area_mask & !is.na(area_raster2) & !is.na(pop_raster)
#     
#     if(terra::global(overlapping_cells, fun = "sum", na.rm = TRUE)[1,1] > 0) {
#       overlapping_oefua <- unique(area_raster2[overlapping_cells])
#       overlapping_oefua <- overlapping_oefua[!is.na(overlapping_oefua)]
#       
#       if(length(overlapping_oefua) > 0) {
#         intersections <- rbind(intersections, 
#                                data.frame(area_id = area_id, 
#                                           oefua_id = overlapping_oefua))
#       }
#     }
#   }
#   return(intersections)
# }
# 
# # Find uc intersections with oefua
# uc_intersections <- area_intersections(uc_raster, oefua_raster, pop_agg)
# names(uc_intersections) <- c("uc_id", "oefua_id")
# 
# # Find efua intersections with oefua
# efua_intersections <- area_intersections(efua_raster, oefua_raster, pop_agg)
# names(efua_intersections) <- c("efua_id", "oefua_id")
# 
# message("intersections process...")
# 
# # Check for multiple intersections
# uc_multi <- uc_intersections %>% 
#   group_by(uc_id) %>% 
#   summarise(n_oefua = n()) %>% 
#   filter(n_oefua > 1)
# 
# efua_multi <- efua_intersections %>% 
#   group_by(efua_id) %>% 
#   summarise(n_oefua = n()) %>% 
#   filter(n_oefua > 1)
# 
# if(nrow(uc_multi) > 0) {
#   warning(paste("UC areas intersecting multiple OEFUA:", paste(uc_multi$uc_id, collapse = ", ")))
# }
# 
# if(nrow(efua_multi) > 0) {
#   warning(paste("EFUA areas intersecting multiple OEFUA:", paste(efua_multi$efua_id, collapse = ", ")))
# }
# 
# ## Step 3: Apply 50% population rule 
# 
# # Function to apply 50% rule
# selection_rule <- function(area_raster, area_intersections, area_name) {
#   
#   # Calculate total population for each area
#   total_pop <- terra::zonal(pop_agg, area_raster, fun = "sum", na.rm = TRUE)
#   names(total_pop) <- c("area_id", "total_pop")
#   
#   qualifying_areas <- data.frame()
#   
#   for(i in 1:nrow(area_intersections)) {
#     area_id <- area_intersections[i, 1]
#     oefua_id <- area_intersections[i, 2]
#     
#     # Create intersection mask
#     area_mask <- area_raster == area_id
#     oefua_mask <- oefua_raster == oefua_id
#     intersection_mask <- area_mask & oefua_mask
#     
#     # Calculate intersection population
#     intersect_pop <- terra::global(pop_agg * intersection_mask, fun = "sum", na.rm = TRUE)[1,1]
#     
#     # Get total population for this area
#     total_pop_area <- total_pop[total_pop$area_id == area_id, "total_pop"]
#     
#     # Apply 50% rule
#     if(length(total_pop_area) > 0 && total_pop_area > 0) {
#       ratio <- intersect_pop / total_pop_area
#       if(ratio >= 0.5) {
#         qualifying_areas <- rbind(qualifying_areas, 
#                                   data.frame(area_id = area_id, 
#                                              oefua_id = oefua_id,
#                                              population = intersect_pop))
#       }
#     }
#   }
#   
#   return(qualifying_areas)
# }
# 
# # Apply rule to areas
# uc_qualifying <- selection_rule(uc_raster, uc_intersections, "UC")
# efua_qualifying <- selection_rule(efua_raster, efua_intersections, "EFUA")
# 
# message("Apply rule process...")
# 
# # Check for OEFUA with no qualifying areas
# # oefua_ids <- unique(oefua$OE_FUAID)
# # uc_oefua_covered <- unique(uc_qualifying$oefua_id)
# # efua_oefua_covered <- unique(efua_qualifying$oefua_id)
# # 
# # uc_missing <- setdiff(oefua_ids, uc_oefua_covered)
# # efua_missing <- setdiff(oefua_ids, efua_oefua_covered)
# # 
# # if(length(uc_missing) > 0) {
# #   warning(paste("OEFUA with no qualifying UC areas:", paste(uc_missing, collapse = ", ")))
# # }
# # 
# # if(length(efua_missing) > 0) {
# #   warning(paste("OEFUA with no qualifying EFUA areas:", paste(efua_missing, collapse = ", ")))
# # }
# 
# ## Step 4: Identify main city centers 
# 
# # UC main centers
# uc_main_centers <- uc_qualifying %>%
#   group_by(oefua_id) %>%
#   mutate(Main_city_center = ifelse(population == max(population), 1, 0)) %>%
#   select(Id = area_id, Main_city_center, oefua_id) %>%
#   ungroup()
# 
# # EFUA main centers
# efua_main_centers <- efua_qualifying %>%
#   group_by(oefua_id) %>%
#   mutate(Main_city_center = ifelse(population == max(population), 1, 0)) %>%
#   select(Id = area_id, Main_city_center, oefua_id) %>%
#   ungroup()
# 
# message("main city centers  process...")
# 
# ## Step 5: Create output tables 
# 
# write.csv(uc_main_centers, here("Output","uc_table.csv"), row.names = FALSE)
# write.csv(efua_main_centers, here("Output", "efua_table.csv"), row.names = FALSE)
# 
# cat("UC qualifying areas:", nrow(uc_main_centers), "\n")
# cat("EFUA qualifying areas:", nrow(efua_main_centers), "\n")
# cat("Analysis complete!\n")
# 
# body = "Your R script has finished running!"
# source(here("Data","pushsaver.R"))
# 
# }, error = function(e) {
#   
#   body = paste("Error in script:", e$message)
#   source(here("Data","pushsaver.R"))
#   
# })

  
# 4. Select and create variables according urban definitions -----
## For efua
  efua_dataset <- efua %>%
                  separate_longer_delim(UC_IDs, delim = ";") %>%
                  mutate(UC_IDs = as.numeric(trimws(UC_IDs))) %>% 
                  left_join(st_drop_geometry(uc_2019), by = c("UC_IDs" = "ID_HDC_G0"), keep = TRUE) %>%  #with efua geometry!!
                  rename(area = AREA) %>% 
                  group_by(ID_HDC_G0) %>%
                  mutate(main_uc = as.integer(P15 == max(P15, na.rm = TRUE))) %>% # Main city center 
                  ungroup() %>% 
                  st_as_sf()

                
  ## Operations mapping with variables and geometries
  # Area weighted sum  
  # Average rate of growth  
  # Population weighted sum  
  # Simple sum  
  # Total % change  
  # Value of main  
  
  # Variable	Operation	Geometry
  # B00	Simple sum	all
  # B15	Simple sum	all
  # Annual average rate of build up growth (2010-2015)	Average rate pf growth	all
  # P00	Simple sum	all
  # P15	Simple sum	all
  # Annual average rate of pop growth (2010-2015)	Average rate pf growth	all
  # BUCAP15	population weighted sum	All
  # NTL_AV	Area weighted sum	all
  # GDP00_SM	Simple sum	all
  # GDP15_SM	Simple sum	all
  # Average GDP growth (2000-2015)	Average rate pf growth	all
  # GDP per cap 2015	population weighted sum	all
  # GDP per cap 2010	population weighted sum	all
  # GDP per cap average growth rate (2010-2015)	Average rate pf growth	all
  # TT2CC	Value of main	main uc
  # E_GR_AT00	Area weighted sum	all
  # E_GR_AT14	Area weighted sum	all
  # Change in green	Total % change	all
  # E_EPM2_E00	Simple sum	all
  # E_EPM2_E15	Simple sum	all
  # E_EPM2_R00	Simple sum	all
  # E_EPM2_R15	Simple sum	all
  # E_EPM2_I00	Simple sum	all
  # E_EPM2_I15	Simple sum	all
  # E_EPM2_T00	Simple sum	all
  # E_EPM2_T15	Simple sum	all
  # E_EPM2_A00	Simple sum	all
  # E_EPM2_A15	Simple sum	all
  # ADD GRWTH FOR EACH OF THE CATEGORIES	Total % change	all
  # E_CPM2_T00	Area weighted sum	all
  # E_CPM2_T14	Area weighted sum	all
  # ADD GRWTH FOR EACH OF THE CATEGORIES	Total % change	all
  # EX_FD_B00	simple sum	all
  # EX_FD_B15	simple sum	all
  # ADD GRWTH FOR EACH OF THE CATEGORIES	simple sum	all
  # EX_FD_P00	simple sum	all
  # EX_FD_P15	Simple sum	all
  # ADD GRWTH FOR EACH OF THE CATEGORIES	Total % change	all
  # EX_SS_B00	simple sum	all
  # EX_SS_B15	simple sum	all
  # ADD GRWTH FOR EACH OF THE CATEGORIES	Total % change	all
  # EX_SS_P00	Area weighted sum	all
  # EX_SS_P15	Simple sum	all
  # SDG_LUE9015	Value of main	main uc
  # SDG_OS15MX	Value of main	main uc
  # 
  
  # SIMPLE SUM (sum across all ucs in efua)
  simple_sum_vars <- c(
    "B00", "B15",                           # Built-up area
    "P00", "P15",                           # Population  
    "GDP00_SM", "GDP15_SM",                 # GDP PPP totals
    "E_EPM2_E00", "E_EPM2_E15",             # Energy emissions
    "E_EPM2_R00", "E_EPM2_R15",             # Residential emissions
    "E_EPM2_I00", "E_EPM2_I15",             # Industrial emissions
    "E_EPM2_T00", "E_EPM2_T15",             # Transport emissions
    "E_EPM2_A00", "E_EPM2_A15",             # Agriculture emissions
    "EX_FD_B00", "EX_FD_B15",               # Flood exposure built-up
    "EX_FD_P00", "EX_FD_P15",               # Flood exposure population
    "EX_SS_B00", "EX_SS_B15",               # Sea level exposure built-up
    "EX_SS_P15"                             # Sea level exposure population
  )  
  

  # AREA WEIGHTED SUM (weighted by area) vs FUA_area VERIFY!!
  area_weighted_vars <- c(
    "NTL_AV",                             # Night time lights
    "E_GR_AT00", "E_GR_AT14",             # Green areas
    "E_CPM2_T00", "E_CPM2_T14",           # PM2.5 concentrations
    "EX_SS_P00"                           # Sea level exposure population (area weighted)
  )
  
  # VALUE OF MAIN UC (take value from uc with highest P15)
  main_uc_vars <- c(
    "TT2CC",                               # Travel time to city center
    "SDG_LUE9015",                         # SDG Land Use Efficiency
    "SDG_OS15MX"                           # SDG Open Space
  )
  
  # Aggregate uc_2019 data to efua level, FUA_p_2015/P15 amd FUA_area/B15
  efua_stats <- efua_dataset %>%
    group_by(eFUA_ID) %>%
    summarise(
        
        # Simple sums
        across(all_of(simple_sum_vars), ~sum(.x, na.rm = TRUE)),
        
        # Population weighted averages, ISSUE with summarise and weighted.mean() in dplyr operations!!!
        BUCAP15 = sum(BUCAP15 * FUA_p_2015, na.rm = TRUE) / sum(FUA_p_2015, na.rm = TRUE),
        
        # Area weighted averages  
        across(all_of(area_weighted_vars), 
                ~sum(.x * FUA_area, na.rm = TRUE) / sum(B15, na.rm = TRUE)),
        
        # Main UC values (from UC with highest P15)
        across(all_of(main_uc_vars), ~.x[which.max(FUA_p_2015)]),
        
        # efua metadata
        total_area = sum(B15, na.rm = TRUE),
        total_population = sum(P15, na.rm = TRUE),
        n_urban_centers = n(),
        area_distortion = area/FUA_area,
        pop_distortion = P15/FUA_p_2015,
        
        # Taking first as its at efua level, WARNING ignored
        geom = st_geometry(.)[1],
          
        .groups = 'drop'
      ) %>%
      
      # POST-aggregation calculations
      mutate(
        # Growth rates (compound annual)
        buildup_growth_rate = (B15 / B00)^(1/15) - 1,     # 2000-2015
        pop_growth_rate = (P15 / P00)^(1/15) - 1,         # 2000-2015
        gdp_growth_rate = (GDP15_SM / GDP00_SM)^(1/15) - 1, # 2000-2015
        # NTL_AV, only 2015
        
        # GDP per capita, 2000-2015
        gdp_per_cap_00 = GDP00_SM / P00,
        gdp_per_cap_15 = GDP15_SM / P15,
        gdp_per_cap_growth_rate = (gdp_per_cap_15 / gdp_per_cap_00)^(1/15) - 1,
        
        # Percentage changes, 2000-2015
        green_change_pct = (E_GR_AT14 - E_GR_AT00) / E_GR_AT00 * 100,
        pm25_change_pct = (E_CPM2_T14 - E_CPM2_T00) / E_CPM2_T00 * 100,
        flood_buildup_change_pct = (EX_FD_B15 - EX_FD_B00) / EX_FD_B00 * 100,
        flood_pop_change_pct = (EX_FD_P15 - EX_FD_P00) / EX_FD_P00 * 100,
        sealevel_buildup_change_pct = (EX_SS_B15 - EX_SS_B00) / EX_SS_B00 * 100,
        sealevel_pop_change_pct = (EX_SS_P15 - EX_SS_P00) / EX_SS_P00 * 100,
        
        # Emissions growth by category, 2000-2015
        energy_emissions_growth = (E_EPM2_E15 - E_EPM2_E00) / E_EPM2_E00 * 100,
        residential_emissions_growth = (E_EPM2_R15 - E_EPM2_R00) / E_EPM2_R00 * 100,
        industrial_emissions_growth = (E_EPM2_I15 - E_EPM2_I00) / E_EPM2_I00 * 100,
        transport_emissions_growth = (E_EPM2_T15 - E_EPM2_T00) / E_EPM2_T00 * 100,
        agriculture_emissions_growth = (E_EPM2_A15 - E_EPM2_A00) / E_EPM2_A00 * 100
      )
  
  # ISSUE uc do not sum to efua as this include commuting zones that are structurally different from uc
  # using efua area and population is an heroic assumption VERIFY!!!!!
  
  write_csv(efua_stats, here("Output", "efua_stats.csv"))
  saveRDS(efua_stats, here("Output", "efua_stats.rds"))
  
## For uc, 2024
  
  # Indicator Name		Attribute ID	Unit	Operation
  # Urban Centre Area		GC_UCA_KM2_2020	km2	Simple sum
  # Urban Centre population		GC_POP_TOT_2020	number of people	Simple sum
  # Total Built-up surface		GH_BUS_TOT_2020	m2	Simple sum
  # build up SERFICE GROWTH rate 2010-2019				Average rate of growth 
  # Total Population		GH_POP_TOT_2020	num inhabitants	Simple sum
  # Compound Annual Growth Rate		GH_POP_CAG_2020		Average rate of growth 
  # GDP		SC_SEC_GDP_2020	PPP	Simple sum
  # GDP average annual growth				Average rate of growth 
  # Expected years of schooling		SC_SEC_SET_2020	years	Population weighted average
  # Mean years of schooling		SC_SEC_SYT_2020	years	Population weighted average
  # Emissions per capita 	Emissions of  CO2 per capita	EM_CO2_PEC_2020	ton/(year x person)	Population weighted average
  # Emissions of  GHG per capita	EM_GHG_PEC_2020	ton/(year x person)	Population weighted average
  # Emissions of  NOx per capita	EM_NOX_PEC_2020	ton/(year x person)	Population weighted average
  # Emissions of  PM2.5 per capita	EM_PM2_PEC_2020	ton/(year x person)	Population weighted average
  # Share of population living in areas exposed to floods 	Share of population living in areas exposed to floods (100 yrp)	EX_010_SHP_2020	%	Population weighted average
  # Share of population living in areas exposed to floods (10 yrp)	EX_100_SHP_2020	%	Population weighted average
  # Share of population exposed	Share of Population exposed to coastal floods with a return period of 20 years	EX_CF2_SHP_2020	%	Population weighted average
  # Share of Population exposed to coastal floods with a return period of 100 years	EX_CF1_SHP_2020	%	Population weighted average
  # Total number of events		HZ_CON_TOT_2020	num events	Simple sum
  # Share of green area in built-up area		GR_SHB_GRN_2020	%	Area weighted sum 
  # increase in annual mean temperature 2000-2019				Calculate using aveages of urban centers  for each of the year
  # Share of the urban centre population living within 1 km buffer from a hospital		HL_SHP_HOS_2020	%	Area weighted average
  # Road network density		IN_ROA_DEN_2020	m/m2	Area weignted average
  # CISI (all sectors)		IN_CIS_ALL_2020	-	Population weighted average
  # 
  
  # A. Define variables by type for extracting from sub datasets and create dataset with relevant vars
  
  # SIMPLE SUM variables
  simple_sum_vars <- c(
    "GC_POP_TOT_2025",     # Urban Centre population  
    "GH_BUS_TOT_2020",     # Total Built-up surface
    "GH_POP_TOT_2020",     # Total Population
    "SC_SEC_GDP_2020",     # GDP
    "HZ_CON_TOT_2020"      # Total number of events
  )
  
  # POPULATION WEIGHTED AVERAGE variables
  pop_weighted_vars <- c(
    "SC_SEC_SET_2020",     # Expected years of schooling
    "SC_SEC_SYT_2020",     # Mean years of schooling
    "EM_CO2_PEC_2020",     # CO2 emissions per capita
    "EM_GHG_PEC_2020",     # GHG emissions per capita
    "EM_NOX_PEC_2020",     # NOX emissions per capita
    "EM_PM2_PEC_2020",     # PM2.5 emissions per capita
    "EX_010_SHP_2020",     # Share pop exposed to 10yr floods
    "EX_100_SHP_2020",     # Share pop exposed to 100yr floods
    "EX_CF2_SHP_2020",     # Share pop exposed to climate factor 2
    "EX_CF1_SHP_2020",     # Share pop exposed to climate factor 1
    "IN_CIS_ALL_2020"      # CISI (all sectors)
  )
  
  # AREA WEIGHTED AVERAGE variables
  area_weighted_vars <- c(
    "GR_SHB_GRN_2020",     # Share of green area in built-up
    "HL_SHP_HOS_2025",     # Share pop within 1km of hospital
    "IN_ROA_DEN_2024"      # Road network density, only for 2024
  )
  
  other_vars <- c(
    "GC_UCA_KM2_2025"     # Urban Centre Area, in all subdatasets
  )
  
  # GROWTH RATE variables (after aggregation)
  growth_rate_vars <- c(
    "buildup_growth_rate",      # From built-up data
    "pop_growth_rate",          # From GH_POP_CAG_2020 or calculate
    "gdp_growth_rate",          # From GDP data
    "temp_increase_rate"        # From temperature data
  )
  
  
  all_vars <- c(simple_sum_vars, pop_weighted_vars, area_weighted_vars, other_vars)
  
  # Create an index of variable names to their respective layer names
  var_df_mapping <- imap(uc_all, function(df, name) {
                      variable_names <- setdiff(names(df), c("ID_UC_G0", "geom"))
                      setNames(rep(name, length(variable_names)), variable_names)
                    }) %>% 
                    flatten_chr()
  
  vars_by_layer <- split(names(var_df_mapping), var_df_mapping)
  selected_layers <- intersect(names(vars_by_layer), names(uc_all))
  
  uc_merged <- reduce(
    selected_layers,
    function(x, layer_name) {
      vars <- intersect(vars_by_layer[[layer_name]], all_vars)
      df <- uc_all[[layer_name]] %>%
            dplyr::select(any_of(c("ID_UC_G0", vars, "geom")))
      
      if (is.null(x)) { #Requried for initialization
        return(df)
      } else {
        return(left_join(x, st_drop_geometry(df), by = "ID_UC_G0"))
      }
    },
    .init = NULL
  ) %>%
    st_as_sf()
  
  # Clean duplicates
  uc_merged <- uc_merged %>%
    rename(
      GC_POP_TOT_2025 = GC_POP_TOT_2025,      
      GC_UCA_KM2_2025 = GC_UCA_KM2_2025        
    ) %>%
    dplyr::select(-matches("^GC_POP_TOT_2025\\.")) %>%
    dplyr::select(-matches("^GC_UCA_KM2_2025\\."))
  
  # B. Geographically matching, as uc and efua are not compatible:
  
  # Intersection between uc and efua
  intersections <- st_intersection(uc_merged, efua)
  
  # Area of each intersection
  intersections$intersection_area <- st_area(intersections)
  
  # Proportion intersection, crs in meters
  intersections$area_percentage <- as.numeric(intersections$intersection_area) / 1000000 / intersections$GC_UCA_KM2_2025
  
  # Population in each intersection
  intersections$pop_in_intersection <- intersections$area_percentage * intersections$GC_POP_TOT_2025
  
  # Group by uc and calculate total population and percentage in each efua
  uc_efua_match <- intersections %>%
    st_drop_geometry() %>%
    group_by(ID_UC_G0) %>%
    mutate(
      total_uc_pop = sum(pop_in_intersection),
      pop_percentage = pop_in_intersection / total_uc_pop * 100
    ) %>%
    ungroup()
  
  # Filter for uc where >50% population is in an efua
  uc_assigned <- uc_efua_match %>%
    filter(pop_percentage > 50) %>%
    dplyr::select(ID_UC_G0, eFUA_ID, pop_percentage)
  
  # Join to original uc data
  ghsl_efua <- uc_merged %>%
    left_join(uc_assigned, by = "ID_UC_G0") %>%
    left_join(efua %>% st_drop_geometry(), by = "eFUA_ID")
  
  # Identify main uc for each efua
  main_uc_lookup <- ghsl_efua %>%
    st_drop_geometry() %>%
    filter(!is.na(eFUA_ID)) %>%
    group_by(eFUA_ID) %>%
    slice_max(GC_POP_TOT_2025, n = 1, with_ties = FALSE) %>%
    dplyr::select(eFUA_ID, main_uc_id = ID_UC_G0)
  
  # Add main_uc 
  ghsl_efua <- ghsl_efua %>%
    left_join(main_uc_lookup, by = "eFUA_ID") %>%
    mutate(main_uc = ifelse(ID_UC_G0 == main_uc_id, TRUE, FALSE)) %>%
    dplyr::select(-main_uc_id)
  
  # View results
  print(paste("Total UCs:", nrow(ghsl_efua)))
  print(paste("UCs assigned to eFUA:", sum(!is.na(ghsl_efua$eFUA_ID))))
  print(paste("Main UCs identified:", sum(ghsl_efua$main_uc, na.rm = TRUE)))
  
  # [1] "Total UCs: 11422"
  # [1] "UCs assigned to eFUA: 9554"
  # [1] "Main UCs identified: 7824"

  # Check for multiple assignments (shouldn't happen with >50% rule)
  multiple_assignments <- uc_efua_match %>%
    filter(pop_percentage > 50) %>%
    group_by(ID_UC_G0) %>%
    summarise(count = n()) %>%
    filter(count > 1)
  
  if(nrow(multiple_assignments) > 0) {
    warning("Some UCs assigned to multiple eFUAs - check population calculation")
  }
    
    # separate_longer_delim(UC_IDs, delim = ";") %>%
    # mutate(UC_IDs = as.numeric(trimws(UC_IDs))) %>% 
    # left_join(st_drop_geometry(uc), by = c("UC_IDs" = "ID_UC_G0"), keep = TRUE) %>%  #with efua geometry!!
    # rename(area = GC_UCA_KM2_2025) %>% 
    # group_by(ID_UC_G0) %>%
    # mutate(main_uc = as.integer(GC_POP_TOT_2025 == max(GC_POP_TOT_2025, na.rm = TRUE))) %>% # Main city center 
    # ungroup() %>% 
    # st_as_sf()

  # C. Aggregate GHSL data to eFUA level
  ghsl_dataset <- ghsl_efua %>%
    st_drop_geometry() %>%
    drop_na(eFUA_ID) %>% # 1868 NA's, this is, do not comply with matching rule
    group_by(eFUA_ID) %>%
    summarise(
      # Simple sums
      across(all_of(simple_sum_vars), ~sum(.x, na.rm = TRUE)),
      
      # Population weighted averages
      across(all_of(pop_weighted_vars), 
             ~sum(.x * GH_POP_TOT_2020, na.rm = TRUE) / sum(GH_POP_TOT_2020, na.rm = TRUE)),
      
      # Area weighted averages (weighted by built-up area)
      across(all_of(area_weighted_vars),
             ~sum(.x * GH_BUS_TOT_2020, na.rm = TRUE) / sum(GH_BUS_TOT_2020, na.rm = TRUE)),
      
      # Metadata
      total_area_km2 = sum(GC_UCA_KM2_2025, na.rm = TRUE),
      total_population = sum(GH_POP_TOT_2020, na.rm = TRUE),
      total_buildup_m2 = sum(GH_BUS_TOT_2020, na.rm = TRUE),
      total_gdp_ppp = sum(SC_SEC_GDP_2020, na.rm = TRUE),
      n_urban_centers = n(),
      area_distortion = total_area_km2/FUA_area,
      pop_distortion = total_population/FUA_p_2015,
      
      .groups = 'drop'
    ) %>%
    # POST-aggregation calculations
    mutate(
      # Growth rates (if base year data available)
      # buildup_growth_rate = calculate from time series data
      # pop_growth_rate = use GH_POP_CAG_XXXX if available
      # gdp_growth_rate = calculate from GDP time series
      
      # Derived indicators
      gdp_per_capita = total_gdp_ppp / total_population,
      buildup_per_capita = total_buildup_m2 / total_population,
      population_density = total_population / total_area_km2
    )
  
  # Join geometry back
  ghsl_efua_final <- ghsl_dataset %>%
                     distinct() %>% 
                      left_join(
                        ghsl_efua %>% drop_na(eFUA_ID) %>% dplyr::select(eFUA_ID, geom) %>% distinct(),
                        by = "eFUA_ID"
                      ) %>%
                      st_sf()
  
  if(!dim(ghsl_efua_final)[1] == sum(!is.na(ghsl_efua$eFUA_ID))) {
    warning("Output does not match eFUA number")
  }
  
  write_csv(ghsl_efua_final, here("Output", "ghsl_efua.csv"))
  saveRDS(ghsl_efua_final, here("Output", "ghsl_efua.rds"))

  # NEXT STEPS
  # Select variables according simple sum, areal weigthed sum, population weigted sum or other
  # Write code and calculate new variable according urban definition
  # Create database for each urban definition
  # Create key table to match urban definitions and metadata
  
  # First the list of deliverables I expect in the folder (clearly arranged in the folder)
  #   Now the list of graphs:
  #   
  #   First of all, I expect to have  ability to select one main city, and 8 comparators: 4 direct comparators, and 4 aspirational. Both groups should be identified by color – E.g. green for aspirational, blue for direct.
  # 
  # Comparison of economic growth – GVA OE 15 years line chart till 2021.
  # Comparison of employment growth: OE 15 years line chart till 2021.
  # Comparison of growth of Night lights: total of eFUAs (15 years) – line chart
  # Comparison of growth of total build up area eFUA (25 years) – line chart
  # Comparison of gdp growth using GDP from UCDB – sum of all centers, (not sure what the time range is for it  - so make a call)
  # Comparison of the structure of GVA : OE, % bar charts. 2019
  # Comparison of structure of employment: OE, % bar charts 2019
  # Timeseries bar charts for shifts of employment structure for each city (hopefully you can stack them on one page). OE – 15 years.til 2021
  # Timeseries bar charts for shifts of GVA structure for each city. Till 2021
  # Comparison structure bar charts for high – low skill employment using the data for eFUAs from bens dataset, use lates year available.
  # GHG emissions per capita over time comparison  - line chart. (for as long as available) – UCBD
  # Share of population living  in areas in areas exposed to 10 year floods.  – USDB, a simple bar chart.
  # 
  # Bonus
  # 
  # Comparison of gdp growth using GDP from UCDB – oonly for the main urban center. , line chart,
  # Share of population exposed to 20 year coastal floods  - bar chart, UDSB
  # Share of green in build up areas – bar chart.UCDB
  # Increase of annual mean temperatures – bar chart UCDB
  # Road network density – bar chart  - UCDB
  # CISI index for all sectors.  – UCDB – population average for all urban centers.
  # 
  
