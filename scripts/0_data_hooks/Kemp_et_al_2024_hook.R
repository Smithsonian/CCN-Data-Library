## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Kemp et al  2024 <-  Dataset: Timing and magnitude of recent accelerated sea-level rise (North Carolina, United States)

## Dataset:https://doi.org/10.25573/serc.24910587.v1

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
methods_raw <- read_csv("data/primary_studies/Kemp_et_al_2024/original/Kemp_et_al_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Kemp_et_al_2024/original/Kemp_et_al_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Kemp_et_al_2024/original/Kemp_et_al_2024_depthseries.csv")
impacts_raw <- read_csv("data/primary_studies/Kemp_et_al_2024/original/Kemp_et_al_2024_impacts.csv")
species_raw <- read_csv("data/primary_studies/Kemp_et_al_2024/original/Kemp_et_al_2024_species.csv")

  
## 1. Curation ####

id <- "Kemp_et_al_2024"


## ... Methods ####

methods <- methods_raw 
methods <- reorderColumns("methods", methods)


## ... Sites ####

#sites <- sites_raw


## ... Cores ####

cores <- cores_raw %>% 
  mutate(study_id = id)
cores <- reorderColumns("cores", cores)


## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  janitor::remove_empty(which = "cols")
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

species <- species_raw
species <- reorderColumns("species", species)

## ... Impacts ####

impacts <- impacts_raw
impacts <- reorderColumns("impacts", impacts)

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "impacts", "species")

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
# error message -> Quitting from lines 152-192 [210Pb depth profiles] (data_visualization_report.Rmd)
#Error in `if (...) NULL`:
 # ! the condition has length > 1

## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_methods.csv")
write_csv(cores, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_depthseries.csv")
write_csv(impacts, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_impacts.csv")
write_csv(species, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_species.csv")

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
paper_citation <- data.frame(study_id = id,
                             bibliography_id = "Kemp_et_al_2009",
                             publication_type = "article",
                             bibtype = "Article", 
                             title = "Timing and magnitude of recent accelerated sea-level rise (North Carolina, United States)",
                             author = "Andrew C. Kemp, Benjamin P. Horton, Stephen J. Culver, D. Reide Corbett, Orson van de Plassche, W. Roland Gehrels, Bruce C. Douglas, Andrew C. Parnell",
                             doi = "https://doi.org/10.1130/G30352A.1",
                             url = "https://doi.org/10.1130/G30352A.1",
                             journal = "Geology",
                             year = "2009")



dataset_citation <- data.frame(study_id = id,
                               bibliography_id = id,
                               publication_type = "primary dataset",
                               bibtype = "Misc", 
                               title = "Dataset: Timing and magnitude of recent accelerated sea-level rise (North Carolina, United States)",
                               author = "Andrew C. Kemp, Benjamin P. Horton, Stephen J. Culver, D. Reide Corbett, Orson van de Plassche, W. Roland Gehrels, Bruce C. Douglas, Andrew C. Parnell",
                               doi = "10.25573/serc.24910587",
                               url = "https://doi.org/10.25573/serc.24910587",
                               year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024.bib")
write_csv(study_citations, "data/primary_studies/Kemp_et_al_2024/derivative/Kemp_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
