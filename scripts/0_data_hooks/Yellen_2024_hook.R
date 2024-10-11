## CCN Data Library ####

## Soil core data curation script for Yellen 2024
## contact: wolfejax@si.edu

library(tidyverse)
library(RefManageR)
library(leaflet)

## Read files ####
dbd <- read_csv("data/primary_studies/Yellen_2024/original/UMass_BlueCarbonCores_BD.csv")
loi <- read_csv("data/primary_studies/Yellen_2024/original/UMass_BlueCarbonCores_LOI.csv", na = "NaN")
cores_orig <- read_csv("data/primary_studies/Yellen_2024/original/UMass_sediment_core_locations.csv")
surface_samples <- read_csv("data/primary_studies/Yellen_2024/original/UMass-NRCS_blue_carbon_surface_samples.csv") %>% 
  mutate(depth_min = 0, depth_max = 10) %>% mutate(core_id = Samp_ID, site_id = Site)

## Depthseries ####

# convert from wide to long format 
dbd_long <- dbd %>% 
  pivot_longer(cols = -c(dstart, dend), names_to = "core_id", values_to = "dry_bulk_density", values_drop_na = T) %>% 
  arrange(core_id, dstart) %>% rename(depth_min = dstart, depth_max = dend) %>% 
  mutate(core_id = gsub("_BD", "", core_id))

loi_long <- loi %>% 
  pivot_longer(cols = -c(dstart, dend), names_to = "core_id", values_to = "percent_organic_matter", values_drop_na = T) %>% 
  arrange(core_id, dstart) %>% rename(depth_min = dstart, depth_max = dend) %>% 
  mutate(core_id = gsub("_LOI", "", core_id))

# join intervals with dbd and loi
# need to take a second look at the README - these may be representative depths instead
deep_cores <- full_join(dbd_long, loi_long)

ggplot(deep_cores, aes(dry_bulk_density, percent_organic_matter)) + geom_point()

# combine all
depthseries <- bind_rows(surface_samples, deep_cores)
# investigate HSB_002_110m - only core ID present in both

## Core level ####

surface_cores <- surface_samples %>% select(site_id, core_id, Lat, Lon, Elev_mNAVD88)

cores <- cores_orig %>% rename(site_id = Site, core_id = CoreName, Elev_mNAVD88 = m_NAVD88) %>% 
  bind_rows(surface_cores) %>% 
  rename(latitude = Lat, longitude = Lon, elevation = Elev_mNAVD88) %>% 
  mutate(elevation_datum = "NAVD88",
         habitat = "marsh")

## Methods ####

# needs doing
# methods <- data.frame()

## QAQC ####

## map cores

leaflet(cores) %>% addTiles() %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 2, label = ~site_id)


# Create bibliography
# data: https://doi.org/10.15482/USDA.ADC/1529276


## Write files ####
write_csv(cores, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_cores.csv")
write_csv(depthseries, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_depthseries.csv")
write_csv(methods, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_methods.csv")
write_csv(study_citations, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_study_citations.csv")

