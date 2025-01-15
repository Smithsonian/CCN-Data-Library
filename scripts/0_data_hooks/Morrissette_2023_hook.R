## CCN Data Library ########

## Data hook script for Morrissette et al 2023
## contact: Jaxine Wolfe; wolfejax@si.edu 

# load necessary libraries
library(tidyverse)
# library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
# library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# load in data 
methods_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_methods.csv")
plots_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_plots.csv")
depthseries_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_depthseries.csv")
biomass_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_biomass.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Morrissette_et_al_2023"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

methods <-  methods_raw %>% 
  mutate(fraction_carbon_type = "organic carbon",
         carbonates_removed = TRUE,
         method_id = "single set of methods",
         carbon_profile_notes = paste("Samples dried over 36-48hrs for DBD.", carbon_profile_notes)) %>% 
  select(-dry_bulk_density_time_min, -dry_bulk_density_time_max, -ground_or_sieved_flag)

#reorder columns 
methods <- reorderColumns("methods", methods)

## ... Plot and Core Level ####
core_plot <- plots_raw %>% 
  rename(plot_notes = site_description) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         ecotype = tolower(ecotype),
         salinity_class = assignSalinityClass(salinity),
         salinity_method = ifelse(!is.na(salinity_class), "measurement", NA),
         vegetation_class = ifelse(habitat == "mangrove", "forested", "seagrass")) 
  
# divvy into core and plot level tables
cores <- core_plot %>% 
  select(-c(transect_id, plot_id, section_n, max_depth, pH, ORP, tree_count, dominant_species, 
            salinity, protection_status, protection_notes, ecosystem_health, ecotype,
            inundation_notes, plot_notes, contains("_carbon")))

# plot summary
plots <- core_plot %>%
  rename(plant_count = tree_count,
         environmental_setting = ecotype,
         ecosystem_condition = ecosystem_health,
         
         # plot_center_latitude = latitude,
         # plot_center_longitude = longitude,
         max_soil_depth_reporting = max_depth,
         plot_soil_carbon_total = plot_sediment_carbon,
         plot_soil_carbon_100cm = plot_sediment_carbon_1m,
         plot_plant_carbon = plot_biomass_carbon) %>% 
  mutate(plot_shape = "circular",
         # coordinates_obscured_flag = "not obscured",
         field_or_manipulation_code = "field",
         salinity_class = assignSalinityClass(salinity),
         plant_allometry_present = TRUE,
         soil_core_present = TRUE) %>% 
  select(study_id, site_id, plot_id, year, month, day, everything()) %>% 
  select(-c(core_id, section_n, pH, ORP, total_ecosystem_carbon,
            # protection_status, protection_notes, 
            inundation_notes, total_ecosystem_carbon_1m, transect_id))

# some of these would go into to plant_plot_detail table, and merge
# some info from the biomass allometry table as well

## ... Plant Allometry ####

plant <- biomass_raw %>% 
  filter(biomass_flag != "debris") %>% 
  rename(species = species_code, 
         basal_width = diameter_base,
         height = tree_height,
         alive_or_dead = biomass_flag,
         debris_count = debris_number,
         plant_aboveground_mass = biomass_aboveground, # kg
         # plant_organic_matter_above = biomass_aboveground_scaled, # MgC ha-1
         plant_aboveground_carbon = biomass_aboveground_carbon, # MgC ha-1
         plant_belowground_mass = biomass_belowground, # kg
         # plant_organic_matter_below = biomass_belowground_scaled, # MgC ha-1
         plant_belowground_carbon = biomass_belowground_carbon, # MgC ha-1
         plant_organic_matter_total = biomass_total, # MgC ha-1
         plant_organic_carbon_total = biomass_total_carbon) %>% # MgC ha-1
  # separate_wider_delim(species_code, names = c("genus", "species"), delim = " ") %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         plot_area = pi*(plot_radius^2),
         alive_or_dead = recode(alive_or_dead, "live" = "alive"),
         # plant_mass_unit = "kilogram",
         diameter_flag = case_when(!is.na(diameter_qmd) ~ "QMD",
                                   !is.na(diameter_dbh) ~ "DBH"),
         diameter = coalesce(diameter_dbh, diameter_qmd),
         # diameter_unit = ifelse(!is.na(diameter), "centimeter", NA),
         aboveground_carbon_conversion = 0.48,
         belowground_carbon_conversion = 0.39
           # case_when(biomass_flag == "debris" ~ 0.5,
         #                                      # Using an aboveground carbon conversion factor of 0.48
         #                                      # Using a belowground carbon conversion factor of 0.39
         #                                      T ~ NA_real_),
         # canopy_width_unit = ifelse(!is.na(canopy_width), "centimeter", NA)
         ) %>% 
  select_if(function(x) {!all(is.na(x))}) %>%
  select(-c(transect_id, diameter_dbh, contains("scaled"), contains("decay_3"), plant_organic_carbon_total))

names(plant)
# which attributes to drop/retain?

## ... Debris ####

debris <- biomass_raw %>% 
  filter(biomass_flag == "debris")

## ... Species ####

species <- plots_raw %>% 
  rename(species_code = dominant_species) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         species_code = strsplit(species_code, split = ", ")) %>% 
  distinct(study_id, site_id, habitat, species_code) %>% 
  unnest(species_code) %>% 
  mutate(code_type = "Genus species")

sort(unique(species$species_code))

## ... Impacts ####

# create impacts table from plot level
# impacts <- plots %>%
#   select(contains("_id"), ecosystem_health) %>%
#   distinct()
# unique impacts at the site and transect level
# this is more related to the aboveground

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(method_id = "single set of methods",
         fraction_organic_matter = soil_organic_matter/100,
         fraction_carbon = soil_organic_carbon/100) %>% 
  select(-c(transect_id, plot_id, contains("carbon_density"), contains("carbon_stock"), contains("soil_"))) %>% 
  select(study_id, site_id, method_id, everything())

# depthseries <- reorderColumns("depthseries", depthseries)



## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores) # there are two, the seagrass cores share coordinates with mangrove sites

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
      #testNumericCols(depthseries)
test <- test_numeric_vars(depthseries) ##testNumericCols producing error message 
# testNumericCols(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_methods.csv")
write_csv(plots, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_plots.csv")
write_csv(cores, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_species.csv")
write_csv(plant, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_plants.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####
  
# read in data and article citations
release_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.21298338")) %>% 
  mutate(bibliography_id = "Morrissette_et_al_2023_data", publication_type = "primary dataset")
pub_bib <- as.data.frame(ReadBib("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_associated_publication.bib")) %>% 
  mutate(bibliography_id = "Morrissette_et_al_2023_article", publication_type = "primary source")

study_citation <- bind_rows(release_bib, pub_bib) %>% 
  mutate(study_id = id) %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, bibtype, everything())
  
# Morrissette_bib <- study_citation %>% select(-study_id, -publication_type) %>%   
#                   column_to_rownames("bibliography_id")

write_csv(study_citation, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_study_citations.csv")
# WriteBib(as.BibEntry(Morrissette_bib), "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
