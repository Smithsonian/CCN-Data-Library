## CCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu 

## Soil core data curation script for Bossert et al 2025

## Dataset: https://doi.org/10.25573/serc.29847683

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
methods_raw <- read_csv("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_depthseries.csv")
sites_raw <- read_csv("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_sites.csv")
species_raw <- read_csv("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_species.csv")

## 1. Curation ####

id <- "Ndhlovu_et_al_2024"

## ... Methods ####

methods <- methods_raw

## ... Cores ####

cores <- cores_raw %>% 
  mutate(site_id = paste0(site_id, plot_id), 
         month = as.numeric(month), 
         day = as.numeric(day)) %>% 
  select(-plot_id)

cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(site_id = paste0(site_id, plot_id)) %>% 
  select(-plot_id)

depthseries <- reorderColumns("depthseries", depthseries)

# sites <- sites_raw

species <- species_raw %>% 
  mutate(site_id = paste0(site_id, plot_id)) %>% 
  select(-plot_id)

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
#testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_methods.csv")
write_csv(cores, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_cores.csv")
write_csv(depthseries, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_depthseries.csv")
# write_csv(sites, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_sites.csv")
write_csv(species, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_species.csv")


## 4. Bibliography ####

# There are three ways to approach this:
# 1) download the article citation directly to the study's folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.29847683")) %>%
  mutate(study_id = id,
         bibliography_id = paste0(id, "_data"),
         publication_type = "primary dataset") %>%
  select(-keywords)

orig_bib <- as.data.frame(ReadBib("data/primary_studies/Ndhlovu_et_al_2025/original/Ndhlovu_et_al_2025_associated_publications.bib")) %>%
  mutate(study_id = id,
         bibliography_id = paste("Ndhlovu_et_al", year, "article", sep = "_"),
         publication_type = "associated source")

study_citations <-  bind_rows(orig_bib, orig_datapub) %>%
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Ndhlovu_et_al_2025/derivative/Ndhlovu_et_al_2025_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
