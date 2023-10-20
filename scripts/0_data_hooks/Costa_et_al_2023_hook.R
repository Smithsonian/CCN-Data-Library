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

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# read data
costa_methods <- read_csv("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_methods.csv")
costa_cores <- read_csv("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_cores.csv")
costa_ds <- read_csv("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_depthseries.csv")
costa_impacts <- read_csv("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_impacts.csv")
costa_species <- read_csv("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_species.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Costa_et_al_2023"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- costa_methods

## ... Cores ####

# curate core-level data table
cores <- costa_cores

## ... Depthseries ####

# curate core depthseries data table
depthseries <- costa_ds

## ... Species ####

species <- costa_species

## ... Impacts ####

impacts <- costa_impacts

## 2. QAQC ####

## Mapping
leaflet(core_data) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

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

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_methods.csv")
# write_csv(sites, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_sites.csv")
write_csv(cores, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_species.csv")
write_csv(impacts, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_impacts.csv")

## 4. Bibliography ####

library(RefManageR)

costa_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.21295716")) %>% 
  mutate(study_id = "Costa_et_al_2023",
         bibliography_id = "Costa_et_al_2023_data", 
         publication_type = "primary dataset") %>% 
  select(-keywords)

costa_bib <- as.data.frame(ReadBib("data/primary_studies/Costa_et_al_2023/original/costa_et_al_2023_associated_publication.bib")) %>% 
  mutate(study_id = "Costa_et_al_2023",
         bibliography_id = "Costa_et_al_2022_article", 
         publication_type = "associated source") %>% 
  select(-abstract)

study_citations <-  bind_rows(costa_bib, costa_datapub) %>% 
  select(study_id, bibliography_id, everything()) %>% 
  remove_rownames() 

write_csv(study_citations, "data/primary_studies/Costa_et_al_2023/derivative/Costa_et_al_2023_study_citations.csv")
