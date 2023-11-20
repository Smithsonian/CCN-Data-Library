## CCN Data Library ########

## Soil core data curation script for Shaw 
## contact: James Holmquist 2023-11-14

## Notes about the dataset 
## Link to the data release and associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# read data
shaw_methods <- read_csv("data/primary_studies/Shaw_et_al_2023/original/shaw_et_al_2023_materials_and_methods.csv")
shaw_cores <- read_csv("data/primary_studies/Shaw_et_al_2023/original/shaw_et_al_2023_core.csv")
shaw_ds <- read_csv("data/primary_studies/Shaw_et_al_2023/original/shaw_et_al_2023_depthseries.csv")
shaw_species <- read_csv("data/primary_studies/Shaw_et_al_2023/original/shaw_et_al_2023_species.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Shaw_et_al_2020"

# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- shaw_methods %>% 
  mutate(method_id = "single set of methods",
         compaction_flag = "compaction quantified") %>% 
  select(-compaction_notes)

## ... Cores ####

# curate core-level data table
cores <- shaw_cores %>% 
  mutate(habitat = "marsh") %>% 
  mutate(core_date = mdy(core_date),
         year = year(core_date),
         month = month(core_date),
         day = day(core_date),
         core_elevation_method = "RTK") %>% 
  select(-c(core_date))

## ... Depthseries ####

# curate core depthseries data table
depthseries <- shaw_ds %>% 
  mutate(method_id = "single set of methods") %>% 
  select(-c(k40_activity, k40_activity_se))  %>% 
  select(-c(pb210_age)) %>% 
  rename(marker_date_se = marker_age_se) %>% 
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA),
         
         pb210_unit = ifelse(!is.na(excess_pb210_activity), "becquerelsPerKilogram", NA),
         c14_material = ifelse(!is.na(c14_age), "plant macrofossils", NA))


## ... Species ####

species <- shaw_species %>% 
  mutate(species_code = paste(genus, species),
         code_type = "Genus species")  %>% 
  select(-c("genus", "species"))
  
# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)
  
methods <- updated$methods
cores <- updated$cores
depthseries <- updated$depthseries
species <- updated$species
  
## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods","cores", "depthseries", "species")

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
write_csv(methods, "data/primary_studies/Shaw_et_al_2023/derivative/Shaw_et_al_2021_methods.csv")
write_csv(cores, "data/primary_studies/Shaw_et_al_2023/derivative/Shaw_et_al_2021_cores.csv")
write_csv(depthseries, "data/primary_studies/Shaw_et_al_2023/derivative/Shaw_et_al_2021_depthseries.csv")
write_csv(species, "data/primary_studies/Shaw_et_al_2023/derivative/Shaw_et_al_2021_species.csv")


## 4. Bibliography ####

library(RefManageR)

Shaw_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.24526066")) %>% 
  mutate(study_id = "Shaw_et_al_2020",
         bibliography_id = "Shaw_et_al_2020_data", 
         publication_type = "primary dataset") %>% 
  select(-keywords)

Shaw_bib <- as.data.frame(ReadBib("data/primary_studies/Shaw_et_al_2023/original/shaw_et_al_2023_associated_publication.bib")) %>% 
  mutate(study_id = "Shaw_et_al_2020",
         bibliography_id = "Christie_et_al_2021_article", 
         publication_type = "associated source")

study_citations <-  bind_rows(Shaw_datapub, Shaw_bib) %>% 
  select(study_id, bibliography_id, everything()) %>% 
  remove_rownames() 

write_csv(study_citations, "data/primary_studies/Shaw_et_al_2023/derivative/Shaw_et_al_2021_study_citations.csv")
