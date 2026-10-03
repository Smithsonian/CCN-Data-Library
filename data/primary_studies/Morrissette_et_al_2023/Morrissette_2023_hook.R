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

cores <- plots_raw %>% 
  rename(plot_notes = site_description) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         ecotype = tolower(ecotype),
         salinity_class = assignSalinityClass(salinity),
         salinity_method = ifelse(!is.na(salinity_class), "measurement", NA),
         vegetation_class = ifelse(habitat == "mangrove", "forested", "seagrass")) %>% 
  select(-c(transect_id, plot_id, section_n, pH, ORP, tree_count, dominant_species, 
            salinity, protection_status, protection_notes, ecosystem_health, ecotype,
            inundation_notes, plot_notes, contains("_carbon")))

# plot summary
plots <- plots_raw %>% 
  rename(plot_notes = site_description,
         geomorphic_setting = ecotype) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         geomorphic_setting = case_when(geomorphic_setting == "Caye" ~ "open coast", 
                                        T ~ tolower(geomorphic_setting)),
         salinity_class = assignSalinityClass(salinity),
         salinity_method = ifelse(!is.na(salinity_class), "measurement", NA),
         vegetation_class = ifelse(habitat == "mangrove", "forested", "seagrass"),
         impact_class = case_when(grepl("massively disturbed", plot_notes) ~ "disturbed", 
                                  ecosystem_health == "healthy" ~ "natural", 
                                  T ~ ecosystem_health),
         salinity_class = assignSalinityClass(salinity)
    # plot_shape = "circular",
    # coordinates_obscured_flag = "not obscured",
    # field_or_manipulation_code = "field",
         # plant_allometry_present = TRUE,
         # soil_core_present = TRUE
         ) %>% 
  select(study_id, site_id, plot_id, year, month, day, everything()) %>% 
  select(-c(core_id, section_n, pH, ORP, contains("_carbon"), dominant_species, max_depth,
            protection_status, protection_notes, tree_count, ecosystem_health, 
            inundation_notes, total_ecosystem_carbon_1m, transect_id))

## ... Plant Allometry ####

# need to merge plot-level information to the plants table

plant <- biomass_raw %>% 
  filter(biomass_flag != "debris") %>% 
  rename(bsd_cm = diameter_base,
         dbh_cm = diameter_dbh,
         condition_live_or_dead = biomass_flag,
         debris_count = debris_number,
         dead_decay_class = decay_class,
         plant_aboveground_mass = biomass_aboveground, # kg
         # plant_organic_matter_above = biomass_aboveground_scaled, # MgC ha-1
         plant_aboveground_carbon = biomass_aboveground_carbon, # MgC ha-1
         plant_belowground_mass = biomass_belowground, # kg
         # plant_organic_matter_below = biomass_belowground_scaled, # MgC ha-1
         plant_belowground_carbon = biomass_belowground_carbon, # MgC ha-1
         plant_organic_matter_total = biomass_total, # MgC ha-1
         plant_organic_carbon_total = biomass_total_carbon) %>% # MgC ha-1
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         plot_area_ha = pi*(plot_radius^2),
         tree_height_m = tree_height/100,
         canopy_width_d1_m = canopy_width/100
         # aboveground_carbon_conversion = 0.48,
         # belowground_carbon_conversion = 0.39
         # canopy_width_unit = ifelse(!is.na(canopy_width), "centimeter", NA)
         ) %>% 
  select_if(function(x) {!all(is.na(x))}) %>%
  select(-c(tree_height, transect_id, plot_radius, canopy_width, contains("scaled"), contains("decay_3"), plant_organic_carbon_total,
            plot_density, plant_aboveground_mass, plant_aboveground_carbon, plant_belowground_mass,
            plant_organic_matter_total, plant_belowground_carbon, biomass_decay_corrected)) %>% 
  # join plot-level information to plant table
  left_join(plots)

# names(plant)

## ... Debris ####

# Leaving out of the synthesis for now
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
# write_csv(plots, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_plots.csv")
write_csv(cores, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_depthseries.csv")
# write_csv(species, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_species.csv")
write_csv(plant, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_forest_structure.csv")

l## 4. Bibliography ####
  
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
