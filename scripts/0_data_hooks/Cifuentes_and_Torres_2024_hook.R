## CCN Data Library ########

## Soil core data curation script for Cifuentes and Torres 2024
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
orig_methods <- read_csv("data/primary_studies/Cifuentes_and_Torres_2024/original/Cifuentes_and_Torres_2024_methods.csv")
orig_plots <- read_csv("data/primary_studies/Cifuentes_and_Torres_2024/original/Cifuentes_and_Torres_2024_plots.csv")
orig_ds <- read_csv("data/primary_studies/Cifuentes_and_Torres_2024/original/Cifuentes_and_Torres_2024_depthseries.csv")
orig_plants <- read_csv("data/primary_studies/Cifuentes_and_Torres_2024/original/Cifuentes_and_Torres_2024_plants.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Cifuentes_and_Torres_2024"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- orig_methods %>% mutate(method_id = "single set of methods")

## ... Cores ####

# curate core-level data table
cores <- orig_plots %>% 
  select(-c(impact_class:TECK)) %>% 
  mutate(core_id = paste(site_id, transect_id, plot_id, sep = "_"), 
         habitat = "mangrove", 
         vegetation_class = "forested") %>% 
  select(-c(transect_id, plot_id)) %>% 
  select(study_id, site_id, core_id, everything())

## ... Depthseries ####

# curate core depthseries data table
depthseries <- orig_ds %>% 
  mutate(method_id = "single set of methods") %>% 
  select(-c(transect_id, plot_id, cn_ratio, fraction_nitrogen, soil_carbon, dry_weight))

## ... Species ####

species <- orig_plants %>%  
  distinct(study_id, site_id, transect_id, plot_id, species) %>% 
  rename(species_code = species) %>% 
  mutate(code_type = case_when(grepl("[.]", species_code) ~ "Genus",
                               species_code == "None" ~ NA, 
                               T ~ "Genus species")) %>% 
  mutate(core_id = paste(site_id, transect_id, plot_id, sep = "_")) %>% 
  select(-c(transect_id, plot_id)) %>% 
  select(study_id, site_id, core_id, everything())

## ... Impacts ####

impacts <- orig_plots %>%  
  mutate(core_id = paste(site_id, transect_id, plot_id, sep = "_")) %>% 
  select(-c(transect_id, plot_id)) %>% 
  select(study_id, site_id, core_id, everything()) %>% 
  distinct(study_id, site_id, core_id, impact_class)

## ... Plants ####
plants <- orig_plants %>% 
  mutate(plot_id = paste(transect_id, plot_id, sep = "_")) %>% 
  select(-transect_id)

## ... Plots ####


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species", "impacts")

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
testIDs(cores, impacts, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
test_numeric_vars(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_excel_csv(methods, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_methods.csv")
write_excel_csv(plants, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_plants.csv")
write_excel_csv(cores, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_species.csv")
write_excel_csv(impacts, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_impacts.csv")

## 4. Bibliography ####

library(RefManageR)

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.26999278")) %>%
  mutate(study_id = "Cifuentes_and_Torres_2024",
         bibliography_id = "Cifuentes_and_Torres_2024_data",
         publication_type = "primary dataset") %>%
  select(-keywords)

# orig_bib <- as.data.frame(ReadBib("data/primary_studies/Cifuentes_and_Torres_2024/original/Cifuentes_and_Torres_2024_associated_publications.bib")) %>%
#   mutate(study_id = "Cifuentes_and_Torres_2024",
#          bibliography_id = "Rogers_et_al_2019_article",
#          publication_type = "associated source")

study_citations <-  orig_datapub %>% 
  # bind_rows(orig_bib, orig_datapub) %>%
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

write_csv(study_citations, "data/primary_studies/Cifuentes_and_Torres_2024/derivative/Cifuentes_and_Torres_2024_study_citations.csv")
