## CCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu 

## Soil core data curation script for Bossert et al 2025

## Dataset: 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html


#load in data 
methods_raw <- read_csv("data/primary_studies/Bossert_et_al_2025_Langebaan/original/Bossert_et_al_2025_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Bossert_et_al_2025_Langebaan/original/Bossert_et_al_2025_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Bossert_et_al_2025_Langebaan/original/Bossert_et_al_2025_depthseries.csv")
sites_raw <- read_csv("data/primary_studies/Bossert_et_al_2025_Langebaan/original/Bossert_et_al_2025_sites.csv")
species_raw <- read_csv("data/primary_studies/Bossert_et_al_2025_Langebaan/original/Bossert_et_al_2025_species.csv")

## 1. Curation ####

id <- "Bossert_et_al_2025_Langebaan"

## ... Methods ####

methods <- methods_raw

## ... Cores ####

cores <- cores_raw 

cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw 

depthseries <- reorderColumns("depthseries", depthseries)

# sites <- sites_raw

species <- species_raw

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
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
#testNumericCols(depthseries)
testNumericCols(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_excel_csv(methods, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_methods.csv")
write_excel_csv(cores, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_depthseries.csv")
# write_csv(sites, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_sites.csv")
write_excel_csv(species, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_species.csv")


## 4. Bibliography ####

# There are three ways to approach this:
# 1) download the article citation directly to the study's folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.29868626")) %>%
  mutate(study_id = id,
         bibliography_id = paste0(id, "_data"),
         publication_type = "primary dataset") %>%
  select(-keywords)


study_citations <-  orig_datapub %>%
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Bossert_et_al_2025/derivative/Bossert_et_al_2025_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Bossert_et_al_2025_Langebaan/derivative/Bossert_et_al_2025_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
