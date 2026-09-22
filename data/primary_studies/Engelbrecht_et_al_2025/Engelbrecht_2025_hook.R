## CCN Data Library ########

## Soil core data curation script for Engelbrecht et al 2025
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
orig_sites <- read_csv("data/primary_studies/Engelbrecht_et_al_2025/original/engelbrecht_et_al_2025_sites.csv")
orig_ds <- read_csv("data/primary_studies/Engelbrecht_et_al_2025/original/engelbrecht_et_al_2025_depthseries.csv") 
orig_methods <- read_csv("data/primary_studies/Engelbrecht_et_al_2025/original/engelbrecht_et_al_2025_methods.csv") 
orig_species <- read_csv("data/primary_studies/Engelbrecht_et_al_2025/original/engelbrecht_et_al_2025_species.csv") 
orig_cores <- read_csv("data/primary_studies/Engelbrecht_et_al_2025/original/engelbrecht_et_al_2025_cores.csv") 

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Engelbrecht_et_al_2025"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- orig_methods

## ... Cores ####

# curate core-level data table
cores <- orig_cores

## ... Depthseries ####

# curate core depthseries data table
depthseries <- orig_ds %>% 
  mutate(fraction_organic_matter = fraction_organic_matter/100) # this needs to be fixed in the data release as well

## ... Species ####

species <- orig_species

## ... Plants ####
# plants <- orig_plants %>% 
#   rename(plant_id = tree_id) %>% 
#   separate(species, into = c("genus", "species"), sep = " ") %>% 
#   mutate(plot_id = paste(transect_id, plot_id, sep = "_")) %>% 
#   select(-transect_id, -family)

## ... Plots ####


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2)

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

# test numeric attribute ranges
fractionNotPercent(depthseries)
test_numeric_vars(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_excel_csv(methods, "data/primary_studies/Engelbrecht_et_al_2025/derivative/Engelbrecht_et_al_2025_methods.csv")
write_excel_csv(cores, "data/primary_studies/Engelbrecht_et_al_2025/derivative/Engelbrecht_et_al_2025_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Engelbrecht_et_al_2025/derivative/Engelbrecht_et_al_2025_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Engelbrecht_et_al_2025/derivative/Engelbrecht_et_al_2025_species.csv")

## 4. Bibliography ####

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.28541594")) %>%
  mutate(study_id = "Engelbrecht_et_al_2025",
         bibliography_id = "Engelbrecht_et_al_2025_data",
         publication_type = "primary dataset") %>%
  select(-keywords)

# no associated paper
# paperbib <- as.data.frame(GetBibEntryWithDOI("10.1016/j.scitotenv.2025.180000")) %>%
#   mutate(study_id = "Engelbrecht_et_al_2025",
#          bibliography_id = "Engelbrecht_et_al_2025_article",
#          publication_type = "associated source")

study_citations <-  orig_datapub %>% 
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

write_csv(study_citations, "data/primary_studies/Engelbrecht_et_al_2025/derivative/Engelbrecht_et_al_2025_study_citations.csv")
