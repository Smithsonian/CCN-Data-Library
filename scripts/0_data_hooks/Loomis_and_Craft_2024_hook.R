## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Loomis and Craft 2024 dataset: Carbon Sequestration and Nutrient (Nitrogen, Phosphorus) Accumulation in River-Dominated Tidal Marshes, Georgia, USA.

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
methods_raw <- read_csv("data/primary_studies/Loomis_and_Craft_2024/original/Loomis_and_Craft_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Loomis_and_Craft_2024/original/Loomis_and_Craft_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Loomis_and_Craft_2024/original/Loomis_and_Craft_2024_depthseries.csv")

  
## 1. Curation ####

id <- "Loomis_and_Craft_2024"


## ... Methods ####

methods <- methods_raw %>% 
  select(- carbon_profile_notes) #this is only specifying type of EA used 

methods <- reorderColumns("methods", methods)


## ... Sites ####

#sites <- sites_raw


## ... Cores ####

cores <- cores_raw 
cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  select(-P, -fraction_nitrogen) %>% 
  mutate(pb210_unit = ifelse(is.na(total_pb210_activity), NA, "disintegrationsPerMinutePerGram"))

depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

#species <- species_raw

## ... Impacts ####

#impacts <- impacts_raw

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries")

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

## 3. Write datavis report ####
writeDataVizReport(id)

## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Loomis_and_Craft_2024/derivative/Loomis_and_Craft_2024_materials_and_methods.csv")
write_csv(cores, "data/primary_studies/Loomis_and_Craft_2024/derivative/Loomis_and_Craft_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Loomis_and_Craft_2024/derivative/Loomis_and_Craft_2024_depthseries.csv")

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
                             bibliography_id = "Loomis_and_Craft_2024",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Carbon Sequestration and Nutrient (Nitrogen, Phosphorus) Accumulation in River-Dominated Tidal Marshes, Georgia, USA.",
                             author = "Mark J. Loomis, Christopher Craft",
                             doi = "10.25573/serc.24518755",
                             url = "https://doi.org/10.25573/serc.24518755.v1",
                             year = "2024")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Loomis_and_Craft_2024/derivative/Loomis_and_Craft_2024.bib")
write_csv(study_citation, "data/primary_studies/Loomis_and_Craft_2024/derivative/Loomis_and_Craft_associated_publication.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
