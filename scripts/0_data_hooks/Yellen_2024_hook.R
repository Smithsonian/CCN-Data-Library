## CCN Data Library ####

## Soil core data curation script for Yellen 2024
## contact: wolfejax@si.edu

library(tidyverse)
library(RefManageR)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html


## Read files ####
dbd <- read_csv("data/primary_studies/Yellen_2024/original/UMass_BlueCarbonCores_BD.csv")
loi <- read_csv("data/primary_studies/Yellen_2024/original/UMass_BlueCarbonCores_LOI.csv", na = "NaN")
cores_orig <- read_csv("data/primary_studies/Yellen_2024/original/UMass_sediment_core_locations.csv")
yellen_surface_samples <- read_csv("data/primary_studies/Yellen_2024/original/UMass-NRCS_blue_carbon_surface_samples.csv")

id <- "Yellen_2024"

## Depthseries ####

site_lookup <- cores_orig %>% rename(core_id = CoreName, site_id = Site) %>% 
  select(site_id, core_id)

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
deep_cores <- full_join(dbd_long, loi_long) %>% 
  full_join(site_lookup) %>% 
  mutate(method_id = "deep_core",
         core_id = paste0(core_id, "_core"),
         fraction_organic_matter = percent_organic_matter/100) %>% 
  select(-percent_organic_matter)

# now the surface samples
surface_samples <- yellen_surface_samples %>% 
  mutate(depth_min = 0, depth_max = 10,
         method_id = "surface_sample") %>% 
  rename(core_id = Samp_ID,
         site_id = Site, 
         dry_bulk_density = BD_g_cm3,
         fraction_organic_matter = loi) %>% 
  select(-c(perC, CD_gC_cm3, Mineral_g_cm3, Vol_cm3))

# ggplot(surface_samples, aes(fraction_organic_matter, perC)) + geom_point()

# combine all
depthseries <- bind_rows(surface_samples, deep_cores) %>% 
  mutate(study_id = id) %>% 
  select(-c(Lat, Lon, dry_mass_g, Elev_mNAVD88)) %>% 
  reorderColumns("depthseries", .)
# investigate HSB_002_110m - only core ID present in both
# HSB_002_40m has a bunch of DBD = 0
# SPG_003_10m has a negative DBD

# unique(depthseries$depth_max - depthseries$depth_min)

ggplot(depthseries, aes(dry_bulk_density, fraction_organic_matter, col = method_id)) + 
  geom_point()

## Core level ####

# surface_cores <- surface_samples %>% select(site_id, core_id, Lat, Lon, Elev_mNAVD88)

cores <- cores_orig %>% rename(site_id = Site, core_id = CoreName, Elev_mNAVD88 = m_NAVD88) %>% 
  mutate(core_id = paste0(core_id, "_core")) %>% 
  bind_rows(surface_samples %>% select(site_id, core_id, Lat, Lon, Elev_mNAVD88)) %>% 
  rename(latitude = Lat, longitude = Lon, elevation = Elev_mNAVD88) %>% 
  mutate(study_id = id,
         year = 2021,
         month = 2,
         latitude = round(latitude, 5), longitude = round(longitude, 5), 
         core_notes = "Field samples were collected between 2020-06-01 and 2021-08-31",
         core_length_flag = "core depth limited by length of corer",
         elevation_datum = ifelse(!is.na(elevation), "NAVD88", NA),
         elevation_method = ifelse(!is.na(elevation), "RTK", NA),
         position_method = ifelse(!is.na(GPS_method), GPS_method, "RTK"),
         vegetation_class = "emergent",
         habitat = "marsh") %>% 
  select(-c(State, GPS_method)) %>% 
  reorderColumns("cores",.)

## Methods ####

# shallow core methods
methods <- data.frame(study_id = id,
                      method_id = "surface_sample", 
                      coring_method = "push core",
                      compaction_flag = "no obvious compaction",
                      dry_bulk_density_sample_volume = 418,
                      loss_on_ignition_sample_mass = 20) %>%
  # deep core methods
  bind_rows(data.frame(study_id = id, 
                       method_id = "deep_core", 
                       coring_method = "gouge auger",
                       compaction_flag = "corer limits compaction"
  
  )) %>% 
  # shared methods
  mutate(sediment_sieved_flag = "sediment not sieved",
         roots_flag = "roots and rhizomes included",
         dry_bulk_density_temperature = 100,
         loss_on_ignition_time = 4,
         loss_on_ignition_temperature = 550) %>% 
  reorderColumns("methods", .)

## Bibliography

library(RefManageR)

# read in data and article citations
study_citations <- as.data.frame(GetBibEntryWithDOI("10.15482/USDA.ADC/1529276")) %>% 
  mutate(study_id = id,
         bibliography_id = "Yellen_2024_data", 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, bibtype, everything())

## QAQC ####

## map cores

leaflet(cores) %>% addTiles() %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 2, label = ~site_id)

## Table testing
table_names <- c("methods", "cores", "depthseries")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores) 

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
#testNumericCols(depthseries)
test <- testNumericCols(depthseries) ##testNumericCols producing error message 
# testNumericCols(depthseries)


# Create bibliography
# data: https://doi.org/10.15482/USDA.ADC/1529276


## Write files ####
write_csv(cores, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_cores.csv")
write_csv(depthseries, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_depthseries.csv")
write_csv(methods, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_methods.csv")
write_csv(study_citations, "./data/primary_studies/Yellen_2024/derivative/Yellen_2024_study_citations.csv")

