# Coastal Carbon Research Coordination Network
# Global Blue Carbon Inventorying

# This script seeks to unify country and EEZ shapefiles 
# Contact: Jaxine Wolfe, wolfejax@si.edu

## Prepare workspace ####
library(tidyverse)
library(rgdal)
library(rgeos)
# library(sp)
library(sf)
library(raster)
# library(rnaturalearth)

## Read in data
# Load  shapefiles from rdata
load("data/input_shapefiles/assign_geography.rdata")

## Wrangle data #### 

# Add field that can be used to join with the country spatial dataframe
eez@data <- eez@data %>% 
  mutate(COUNTRY = case_when(TERRITORY1 == "Democratic Republic of the Congo" ~ "Congo DRC",
                             TERRITORY1 == "Republic of the Congo" ~ "Congo",
                             TRUE ~ TERRITORY1))

# try to combine
combined <- raster::bind(countries, eez)

combined@data <- combined@data %>% 
  dplyr::select(-c(contains("ISO_"), contains("UN_"), contains("MRGID")))

# try to dissolve internal boundaries
# combined_union <- gUnaryUnion(combined)

# Other attempts
# try to intersect
# inters$flag <- sapply(split(inters, inters$var), function(x) st_intersects(x)) %>% unlist(recursive = FALSE)

# diffs <- gDifference(countries, eez)
# plot(diffs, col = "blue")


# Write Resulting Shapefile ####
writeOGR(obj = combined, dsn = ".", layer = "world_and_eez", driver = "ESRI Shapefile") 

# read in global mangrove watch data from 2020
# gmw <- readOGR(dsn = "./data/input_shapefiles/GMW_v3_2020/00_Data/", layer = "gmw_v3_2020") # takes way too much time

## Ping-pong with ArcGis (Sigh)
worldeez_unified <- readOGR(dsn = "./data/input_shapefiles/WorldEEZUnion//",
                            layer = "WorldEEZUniont")

# Remove duplicate polygons from unified world EEZ
worldeez_unified@data <- worldeez_unified@data %>% 
  add_count(COUNTRY, Shape_Area) %>% 
  mutate(drop_polygon = ifelse(n == 2 & Shape_Leng == 0, T, F))
  # filter(!(n == 2 & Shape_Leng == 0)) %>% 
  dplyr::select(-n)
  # arrange(COUNTRY)
  # add_count(COUNTRY, Shape_Area) %>% 
  # arrange(desc(n)) %>%
  # dplyr::select(n, Shape_Area, everything())


# write
writeOGR(obj = subset(worldeez_unified, drop_polygon == F), dsn = ".", 
         layer = "world_and_eez_unified", driver = "ESRI Shapefile") 


