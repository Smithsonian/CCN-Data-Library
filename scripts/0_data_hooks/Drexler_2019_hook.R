# Curation script for Drexler et al. 2019 
# Drexler, Judith Z., et al. 
# "Carbon accumulation and vertical accretion in a restored versus historic salt marsh 
# in southern Puget Sound, Washington, United States." Restoration Ecology.

# Contact: Michael Lonneman, lonnemanM@si.edu

library(tidyverse)

raw_cores <- read_csv("./data/primary_studies/drexler_2019/original/coordinates.csv")
raw_depthseries <- read_csv("./data/primary_studies/drexler_2019/original/nisqually_depthseries_manual_edits.csv")

## Curate data #########
study_id_value <- "Drexler_et_al_2019"

## ... depthseries #####
depthseries <- raw_depthseries %>%
  mutate(core_id = gsub("Core ", "", core_id)) %>%
  mutate(study_id = study_id_value,
         depth_min = depth_max - 2,
         site_id = ifelse(grepl("SG", core_id), "Six_Gill_Slough", "Animal_Slough")) %>%
  # Modify carbon stock variables: 
  mutate(fraction_carbon = percent_carbon / 100,
         # Dry bulk density was in grams per cubic meter
         dry_bulk_density = dry_bulk_density * 100) %>%
  select(study_id, site_id, core_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon, 
         total_pb210_activity, total_pb210_sd,  
         ra226_activity, ra226_activity_sd, 
         excess_pb210_activity, excess_pb210_activity_sd)

## ... core data #######
coordinates <- raw_cores %>%
  filter(!is.na(Latitude)) %>%
  rename(core_latitude = Latitude,
         core_longitude = Longitude, 
         core_id = `Core abbreviations`,
         core_notes = `Full core ID`, 
         core_date = `Date of collection`) %>%
  mutate(study_id = study_id_value)

cores <- depthseries %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id)) %>%
  merge(coordinates, by="core_id", all.x=TRUE, all.y=TRUE) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, core_notes)

## ... site data #######
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- cores %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) 

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

sites <- cores %>%
  group_by(site_id) %>%
  summarize(study_id = study_id_value) %>%
  merge(site_boundaries, by="site_id") %>%
  select(study_id, site_id, everything())

# ## ... impact data #####
# impacts <- impacts_raw %>%
#   select(study_id, site_id, core_id, impact_class)
# 
# ## ... species data ####
# species <- species_raw %>%
#   select(study_id, site_id, core_id, species_code) %>%
#   filter(!is.na(species_code))

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores)
test_colnames("site_level", sites) 
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("impact", impacts)

test_variable_names(cores)
test_variable_names(sites)
test_variable_names(impacts)
test_variable_names(depthseries)
test_variable_names(species)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

study_uncontrolled <- read_csv("./data_releases/drexler_2019/data/intermediate/drexler_2019_user_defined_attributes.csv")
test_numeric_vars(depthseries)

## Export curated data ###########
write_csv(depthseries, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_depthseries.csv")
write_csv(cores, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_cores.csv")
write_csv(sites, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_sites.csv")
#write_csv(impacts, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_impacts.csv")
#write_csv(species, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_species.csv")
