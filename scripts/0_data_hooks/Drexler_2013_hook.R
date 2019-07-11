# Curation script for Drexler et al 2013
# Waccamaw National Wildlife refuge

# Contact: Michael Lonneman, lonnemanM@si.edu

library(tidyverse)

raw_cores <- read_csv("./data/primary_studies/drexler_2013/original/drexler_2019_initial_sites.csv")
raw_depthseries <- read_csv("./data/primary_studies/drexler_2013/original/drexler_2019_initial_depthseries.csv")
raw_species_table <- read_csv("./data/primary_studies/drexler_2013/original/drexler_2019_study_species_table.csv")

## Curate data #########
study_id_value <- "Drexler_et_al_2013"

## ... core data #######
cores <- raw_cores %>%
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  mutate(study_id = study_id_value,
         core_id = paste(site_id, core_number, sep = "_"))

sites_raw <- cores
impacts_raw <- cores
species_raw <- cores

cores <- cores %>%
  separate(core_position, into=c("core_latitude", "core_longitude"), sep=",") %>%
  mutate(core_latitude = as.numeric(core_latitude), 
         core_longitude = as.numeric(core_longitude)) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude)

## ... site data #######
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- cores %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) %>%
  filter(is.na(core_latitude) == FALSE)

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

sites <- sites_raw %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id),
            site_description = first(site_description)) %>%
  merge(site_boundaries, by="site_id") %>%
  select(study_id, site_id, everything())

## ... impact data #####
impacts <- impacts_raw %>%
  select(study_id, site_id, core_id, impact_class)

## ... species data ####
species <- species_raw %>%
  select(study_id, site_id, core_id, species_code) %>%
  filter(!is.na(species_code))

## ... study level species table #####
study_species <- raw_species_table %>%
  mutate(study_id = study_id_value) %>%
  select(study_id, everything())

## ... depthseries #####
depthseries <- raw_depthseries %>%
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  mutate(study_id = study_id_value,
         core_id = paste(site_id, `Core Number`, sep = "_")) %>%
  rename(dry_bulk_density = `Bulk Density (g/cm3)`, 
         core_notes = Notes, 
         total_carbon = `Total Carbon (%)`,
         organic_carbon = `Organic Carbon (%)`, 
         depth_max = `Section bottom depth from surface (cm)`,
         section_thickness = `section thickness`) %>%
  mutate(fraction_organic_carbon = organic_carbon / 100, 
         fraction_total_carbon = total_carbon / 100, 
         depth_max = gsub("~", "", depth_max), 
         section_thickness = gsub("~", "", section_thickness)) %>%
  mutate(depth_max = as.numeric(depth_max), 
         depth_min = as.numeric(depth_max) - as.numeric(section_thickness)) %>%
  select(study_id, site_id, core_id, depth_max, depth_min, 
         dry_bulk_density, fraction_organic_carbon, fraction_total_carbon) 


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
#write_csv(study_information, "./data_releases/drexler_2019/data/intermediate/drexler_2019_study_information.csv")
#write_csv(keywords, "./data_releases/drexler_2019/data/intermediate/drexler_2019_keywords.csv")
#write_csv(authors, "./data_releases/drexler_2019/data/intermediate/drexler_2019_authors.csv")
#write_csv(associated_publications, "./data_releases/drexler_2019/data/intermediate/drexler_2019_associated_publications.csv")
#write_csv(funding_sources, "./data_releases/drexler_2019/data/intermediate/drexler_2019_funding_sources.csv")
#write_csv(materials_and_methods, "./data_releases/drexler_2019/data/intermediate/drexler_2019_materials_and_methods.csv")

