## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Vincent 2023 dataset: Sediment Carbon Content from three Maine Salt Marshes 1993

## Dataset: https://doi.org/10.25573/serc.23960793.v1

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
methods_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_depthseries.csv")
impacts_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_impacts.csv")
sites_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_sites.csv")
species_raw <- read_csv("data/primary_studies/Vincent_2023/original/Vincent_and_Dionne_2023_species.csv")
  
  
## 1. Curation ####

id <- "Vincent_and_Dionne_2023"


## ... Methods ####

methods <- methods_raw 
methods <- reorderColumns("methods", methods)


## ... Sites ####

sites <- sites_raw


## ... Cores ####

cores <- cores_raw
cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

species <- species_raw

## ... Impacts ####

impacts <- impacts_raw

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "sites", "species", "impacts")

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
write_csv(methods, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_methods.csv")
write_csv(sites, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_sites.csv")
write_csv(cores, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_species.csv")
write_csv(impacts, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_impacts.csv")

## 4. Bibliography ####

# There are three ways to approach this:
    # 1) download the article citation directly to the study's folder
    # 2) create the study citation in the curation script and output it to the data release folder
    # 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

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
# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_associated_publications.bib")

study_citation <- data.frame(study_id = id, 
                             bibliography_id = id,
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Sediment Carbon Content from three Maine Salt Marshes 1993",
                             author = "Robert Vincent, Michele Dionne",
                             doi = "https://doi.org/10.25573/serc.23960793.v2",
                             url = "https://smithsonian.figshare.com/articles/dataset/Dataset_Sediment_Carbon_Content_from_three_Maine_Salt_Marshes_1993/23960793/2",
                             year = "2023")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023.bib")
write_csv(study_citation, "data/primary_studies/Vincent_2023/derivative/Vincent_and_Dionne_2023_study_citation.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
