## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Craft 2024 dataset: Tidal freshwater forest accretion does not keep pace with sea level rise

## Dataset: https://doi.org/10.25573/serc.24895293

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
methods_raw <- read_csv("data/primary_studies/Craft_2024/original/Craft_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Craft_2024/original/Craft_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Craft_2024/original/Craft_2024_depthseries.csv")

  
## 1. Curation ####

id <- "Craft_2024"


## ... Methods ####

methods <- methods_raw 
methods <- reorderColumns("methods", methods)


## ... Sites ####

#sites <- sites_raw


## ... Cores ####

cores <- cores_raw %>% 
  janitor::remove_empty(which = "cols") %>% 
  select(-cs137_accretion_rate, -pb210_crs_accretion_rate, -cs137_accretion_rate_bd) # attributes not included in current library structure

cores <- reorderColumns("cores", cores)


## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  select(-total_phosphorus, -fraction_nitrogen)


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
# error message -> Quitting from lines 152-192 [210Pb depth profiles] (data_visualization_report.Rmd)
#Error in `if (...) NULL`:
 # ! the condition has length > 1

## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Craft_2024/derivative/Craft_2024_materials_and_methods.csv")
write_csv(cores, "data/primary_studies/Craft_2024/derivative/Craft_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Craft_2024/derivative/Craft_2024_depthseries.csv")

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
paper_citation <- dataset_citation <- data.frame(bibliography_id = "Craft_2012",
                                                 publication_type = "article",
                                                 bibtype = "Article", 
                                                 title = "Tidal freshwater forest accretion does not keep pace with sea level rise",
                                                 author = "Christopher Craft",
                                                 doi = "",
                                                 url = "https://doi.org/10.1016/j.ecoleng.2018.03.002",
                                                 journal = "Ecological Engineering",
                                                 year = "2012")



dataset_citation <- data.frame(bibliography_id = "Craft_2024",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Tidal freshwater forest accretion does not keep pace with sea level rise",
                             author = "Christopher Craft",
                             doi = "10.25573/serc.24895293",
                             url = "https://doi.org/10.25573/serc.24895293",
                             year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Craft_2024/derivative/Craft_2024.bib")
write_csv(study_citations, "data/primary_studies/Craft_2024/derivative/Craft_2024_associated_publication.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
