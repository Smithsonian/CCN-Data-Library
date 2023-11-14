## CCN Data Library ########

## Soil core data curation script for <insert dataset name>
## contact: Your Name, your email

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
beers_methods <- read_csv("data/primary_studies/Beers_et_al_2023/original/Beers_et_al_2023_material_and_methods.csv")
beers_cores <- read_csv("data/primary_studies/Beers_et_al_2023/original/Beers_et_al_2023_cores.csv")
beers_ds <- read_csv("data/primary_studies/Beers_et_al_2023/original/Beers_et_al_2023_depthseries.csv")
beers_sites <- read_csv("data/primary_studies/Beers_et_al_2023/original/Beers_et_al_2023_sites.csv")
beers_species <- read_csv("data/primary_studies/Beers_et_al_2023/original/Beers_et_al_2023_species.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Beers_et_al_2023"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- beers_methods

## ... Cores ####

# curate core-level data table
cores <- beers_cores %>% 
  mutate(habitat = case_when(vegetation_class == "emergent" ~ "marsh",
                             vegetation_class == "forested" ~ "mangrove",
                             vegetation_class == "seagrass" ~ "seagrass",
                             TRUE ~ "scrub/shrub"))

## ... Depthseries ####

# curate core depthseries data table
depthseries <- beers_ds %>% select(-fraction_carbon_method)

## ... Species ####

species <- beers_species

## ... Sites ####

sites <- beers_sites

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "sites", "cores", "depthseries", "species")

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
testNumericCols(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_methods.csv")
write_csv(sites, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_sites.csv")
write_csv(cores, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_species.csv")
# write_csv(impacts, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_impacts.csv")

## 4. Bibliography ####

library(RefManageR)

Beers_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.11971527")) %>% 
  mutate(study_id = "Beers_et_al_2023",
         bibliography_id = "Beers_et_al_2023_data", 
         publication_type = "primary dataset") %>% 
  select(-keywords)

Beers_bib <- as.data.frame(ReadBib("data/primary_studies/Beers_et_al_2023/original/beers_et_al_2023_associated_publications.bib")) %>% 
  mutate(study_id = "Beers_et_al_2023",
         bibliography_id = "Rogers_et_al_2019_article", 
         publication_type = "associated source")

study_citations <-  bind_rows(Beers_bib, Beers_datapub) %>% 
  select(study_id, bibliography_id, everything()) %>% 
  remove_rownames() 

write_csv(study_citations, "data/primary_studies/Beers_et_al_2023/derivative/Beers_et_al_2023_study_citations.csv")
