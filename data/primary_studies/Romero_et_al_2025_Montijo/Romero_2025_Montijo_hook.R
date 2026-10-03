## CCN Data Library ########

## Soil core data curation script for Romero et al 2025 Montijo
## contact: Jaxine Wolfe, wolfejax@si.edu

## Notes about the dataset 
## Link to the data release and associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# read data
orig_sites <- read_csv("data/primary_studies/Romero_et_al_2025_Montijo/original/Romero_et_al_2025_sites.csv")
orig_ds <- read_csv("data/primary_studies/Romero_et_al_2025_Montijo/original/Romero_et_al_2025_depthseries.csv") 
orig_methods <- read_csv("data/primary_studies/Romero_et_al_2025_Montijo/original/Romero_et_al_2025_methods.csv")

# not curating these tables for the time being
# orig_debris <- read_csv("data/primary_studies/Romero_et_al_2025_Montijo/original/Romero_et_al_2025_debris.csv")
# orig_veg <- read_csv("data/primary_studies/Romero_et_al_2025_Montijo/original/Romero_et_al_2025_plants.csv", guess_max = 1500)

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Romero_et_al_2025_Montijo"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- orig_methods %>% 
  mutate(method_id = "single set of methods")

## ... Cores ####

# curate core-level data table
cores <- orig_ds %>% 
  distinct(study_id, site_id, core_id, year, month, day) %>% 
  full_join(orig_sites %>% select(study_id, site_id, latitude, longitude, position_notes)) %>%
  mutate(position_method = "other low resolution",
         habitat = "mangrove",
         vegetation_class = "forested",
         vegetation_method = "measurement")

## ... Depthseries ####

# curate core depthseries data table
depthseries <- orig_ds %>% 
  mutate(method_id = "single set of methods") %>% 
  select(-c(plot_id, soil_color, soil_carbon_density, interval_carbon_stock, fraction_h2o, sediment_dry_weight,
            year, month, day, representative_depth_max)) %>%
  reorderColumns("depthseries", .)

## ... Species ####

species <- orig_veg %>%
  distinct(study_id, site_id, species) %>% 
  rename(species_code = species) %>% 
  mutate(code_type = "Genus species")

## ... Plants ####


## ... Plots ####


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, label = ~core_id)

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
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")
testIDs(cores, species, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
test_numeric_vars(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_excel_csv(methods, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_methods.csv")
# write_excel_csv(plants, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_plants.csv")
write_excel_csv(cores, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_species.csv")
# write_excel_csv(impacts, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_impacts.csv")

## 4. Bibliography ####

library(RefManageR)

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.29873261")) %>%
  mutate(study_id = id,
         bibliography_id = "Romero_et_al_2025_Montijo_data",
         publication_type = "primary dataset") %>%
  select(-keywords)

# orig_bib <- as.data.frame(ReadBib("data/primary_studies/Romero_et_al_2025/original/Romero_et_al_2025_associated_publications.bib")) %>%
#   mutate(study_id = "Romero_et_al_2025",
#          bibliography_id = "Rogers_et_al_2019_article",
#          publication_type = "associated source")

study_citations <-  orig_datapub %>% 
  # bind_rows(orig_bib, orig_datapub) %>%
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

write_csv(study_citations, "data/primary_studies/Romero_et_al_2025_Montijo/derivative/Romero_et_al_2025_study_citations.csv")
