## CCRNC Data Release Formatting ########

## Soil core data curation script for <insert dataset name>
## contact: Your Name, your email

## Notes about the dataset 
## Link to associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in  original data by inserting the path to the dataset

cores <- read_csv("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_cores.csv") %>% 
  select_if(function(x) {!all(is.na(x))})
depthseries <- read_xls("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_depthseries.xls", na = "NA") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% drop_na(study_id)
impacts <- read_csv("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_impacts.csv") %>% 
  select_if(function(x) {!all(is.na(x))})
species <- read_csv("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_species.csv") %>% 
  select_if(function(x) {!all(is.na(x))})
methods <- read_csv("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_methods.csv") %>% 
  select_if(function(x) {!all(is.na(x))})

raw_bib <- read_csv("data/primary_studies/Darienzo_and_Peterson_1990/original/Darienzo 1990_bibliography.csv") %>% 
  select_if(function(x) {!all(is.na(x))})

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

## Update Tables ####

# the templates used are outdated, going to use the verioning fnx to update col headers
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

## 1. Curation ####

# id <- "Darienzo_and_Peterson_1990"

## ... Cores ####

cores <- updated$cores %>% 
  rename(elevation_notes = elevation_method) %>% 
  mutate(position_method = "other low resolution",
         site_id = "Netarts",
         core_id = paste0(site_id, "_", core_id),
         habitat = "marsh")
# core date needs resolving, maybe check the pub
# cores elevation_method = "Estimated from predicted tides" should be a note somewhere

# map locations
cores %>%
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 2, label = ~core_id)

## ... Depthseries ####

depthseries <- updated$depthseries %>% 
  mutate(core_id = paste0("Netarts_", core_id))

## ... Methods ####

methods <- updated$methods

# to resolve
# 1 methods publication_type      peer reviewed publication
# 2 methods    coring_method                   Plastic pipe
# 3 methods       roots_flag            Large roots removed
# 4 methods  compaction_flag                    Average 10%

## ... Species ####

species <- updated$species %>% 
  mutate(code_type = "Genus species",
         core_id = paste0("Netarts_", core_id))

## ... Impacts ####

impacts <- updated$impacts %>% 
  mutate(core_id = paste0("Netarts_", core_id))

## 2. QAQC ####

## ... Standard QA Tests ####

# test alignment to database structure
# if there are uncontrolled attributes or variables
# create tables to define them
testTableCols(table_names) 
testTableVars(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational databases
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numerical values and ranges
# this function requires library(skimr)
test_depth <- test_numeric_vars(depthseries, study_uncontrolled = NULL)
test_cores <- test_numeric_vars(cores, study_uncontrolled = NULL)
test_methods <- test_numeric_vars(methods, study_uncontrolled = NULL)
fraction_not_percent(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_methods.csv")
# write_csv(sites, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_sites.csv")
write_csv(cores, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_cores.csv")
write_csv(depthseries, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_depthseries.csv")
write_csv(species, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_species.csv")
write_csv(impacts, "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_impacts.csv")

## 4. Bibliography ####
library(RefManageR)

# read in bib file
bib <- as.data.frame(GetBibEntryWithDOI("10.1029/TC009i001p00001"))



# There are three ways to approach this:
# 1) download the article citation directly to the data release folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in the metadata folder, read it in and output bib file to data release folder

# example study citation creation:
# study_citation <- data.frame(bibliography_id = "Spera_et_al_2020",
#                              title = "Spatial and temporal changes to a hydrologically-reconnected coastal wetland: Implications for restoration",
#                              author = "Alina C. Spera and John R. White and Ron Corstanje",
#                              bibtype = "Article",
#                              doi = "10.1016/j.ecss.2020.106728",
#                              url = "https://doi.org/10.1016/j.ecss.2020.106728", 
#                              journal = "Estuarine, Coastal and Shelf Science",
#                              year = "2020") %>% 
#     column_to_rownames("bibliography_id")
# 
# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_associated_publications.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
