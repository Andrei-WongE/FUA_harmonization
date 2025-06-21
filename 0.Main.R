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

  write_csv(uc_main_centers, here("Output", "uc_table.csv"))
  write_csv(efua_main_centers, here("Output", "efua_table.csv"))
  
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
  
  # Aggregate uc_2019 data to efua level
  efua_stats <- efua_dataset %>%
    group_by(eFUA_ID) %>%
    summarise(
        
        # Simple sums
        across(all_of(simple_sum_vars), ~sum(.x, na.rm = TRUE)),
        
        # Population weighted averages, ISSUE with summarise and weighted.mean() in dplyr operations!!!
        BUCAP15 = sum(BUCAP15 * P15, na.rm = TRUE) / sum(P15, na.rm = TRUE),
        
        # Area weighted averages  
        across(all_of(area_weighted_vars), 
                ~sum(.x * B15, na.rm = TRUE) / sum(B15, na.rm = TRUE)),
        
        # Main UC values (from UC with highest P15)
        across(all_of(main_uc_vars), ~.x[which.max(P15)]),
        
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
        
        # GDP per capita
        gdp_per_cap_00 = GDP00_SM / P00,
        gdp_per_cap_15 = GDP15_SM / P15,
        gdp_per_cap_growth_rate = (gdp_per_cap_15 / gdp_per_cap_00)^(1/15) - 1,
        
        # Percentage changes
        green_change_pct = (E_GR_AT14 - E_GR_AT00) / E_GR_AT00 * 100,
        pm25_change_pct = (E_CPM2_T14 - E_CPM2_T00) / E_CPM2_T00 * 100,
        flood_buildup_change_pct = (EX_FD_B15 - EX_FD_B00) / EX_FD_B00 * 100,
        flood_pop_change_pct = (EX_FD_P15 - EX_FD_P00) / EX_FD_P00 * 100,
        sealevel_buildup_change_pct = (EX_SS_B15 - EX_SS_B00) / EX_SS_B00 * 100,
        sealevel_pop_change_pct = (EX_SS_P15 - EX_SS_P00) / EX_SS_P00 * 100,
        
        # Emissions growth by category
        energy_emissions_growth = (E_EPM2_E15 - E_EPM2_E00) / E_EPM2_E00 * 100,
        residential_emissions_growth = (E_EPM2_R15 - E_EPM2_R00) / E_EPM2_R00 * 100,
        industrial_emissions_growth = (E_EPM2_I15 - E_EPM2_I00) / E_EPM2_I00 * 100,
        transport_emissions_growth = (E_EPM2_T15 - E_EPM2_T00) / E_EPM2_T00 * 100,
        agriculture_emissions_growth = (E_EPM2_A15 - E_EPM2_A00) / E_EPM2_A00 * 100
      )
  
  # ISSUE uc do not sum to efua as this include commuting zones that are structurally different from uc
  # using efua area and population is an heroic assumption VERIFY!!!!!
              
## For uc, 2024
  
  
  
  
  
  
  
  
  
  
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
  