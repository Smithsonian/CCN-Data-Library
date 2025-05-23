## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Stahl et al 2024 dataset: 

## Dataset: Tidal freshwater forests: Sentinels for climate change

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
methods_raw <- read_csv("data/primary_studies/Stahl_et_al_2024/original/Stahl_et_al_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Stahl_et_al_2024/original/Stahl_et_al_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Stahl_et_al_2024/original/Stahl_et_al_2024_depthseries.csv")

  
## 1. Curation ####

id <- "Stahl_et_al_2024"


## ... Methods ####

methods <- methods_raw %>% 
  select(-mineral_matter_method, -fraction_nitrogen_method, -fraction_phosphorus_type, 
         -fraction_phosphorus_method, -fraction_nitrogen_type)

methods <- reorderColumns("methods", methods)


## ... Sites ####

#sites <- sites_raw


## ... Cores ####

cores <- cores_raw 
cores <- reorderColumns("cores", cores)


## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  select(-sample_wt, -`C:N`, -fraction_nitrogen, -fraction_phosphorus)

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

## 3. Write data vis report ####
writeDataVizReport(id)
# same error in pb210 depth profiles 

## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Stahl_et_al_2024/derivative/Stahl_et_al_2024_materials_and_methods.csv")
write_csv(cores, "data/primary_studies/Stahl_et_al_2024/derivative/Stahl_et_al_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Stahl_et_al_2024/derivative/Stahl_et_al_2024_depthseries.csv")

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
paper_citation <- dataset_citation <- data.frame(study_id = id,
                                                 bibliography_id = "Stahl_et_al_2018",
                                                 publication_type = "article",
                                                 bibtype = "Article", 
                                                 title = "Tidal freshwater forests: Sentinels for climate change",
                                                 author = "McKenna Stahl, Sarah Widney and Christopher Craft",
                                                 doi = "10.1016/j.ecoleng.2018.03.002",
                                                 url = "{https://doi.org/10.1016/j.ecoleng.2018.03.002",
                                                 journal = "Ecological Engineering",
                                                 year = "2018")



dataset_citation <- data.frame(study_id = id,
                               bibliography_id = "Stahl_et_al_2024",
                               publication_type = "primary dataset",
                               bibtype = "Misc", 
                               title = "Dataset: Tidal freshwater forests: Sentinels for climate change",
                               author = "McKenna Stahl, Sarah Widney and Christopher Craft",
                               doi = "10.25573/serc.24886155",
                               url = "https://doi.org/10.25573/serc.24886155",
                               year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Stahl_et_al_2024/derivative/Stahl_et_al_2024.bib")
write_csv(study_citations, "data/primary_studies/Stahl_et_al_2024/derivative/Stahl_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
