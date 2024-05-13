## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Gundersen et al 2024 <-  Dataset: Long-Term Sediment, Carbon, and Nitrogen Accumulation Rates in Coastal Wetlands Impacted by Sea Level Rise
## Dataset: https://doi.org/10.25573/serc.25021361.v1

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
methods_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_cores.csv")
sites_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_sites.csv")
depthseries_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_depthseries.csv")
impacts_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_impacts.csv")
species_raw <- read_csv("data/primary_studies/Gundersen_et_al_2024/original/Gundersen_et_al_2024_species.csv")

  
## 1. Curation ####

id <- "Gundersen_et_al_2024"


## ... Methods ####

methods <- methods_raw 
methods <- reorderColumns("methods", methods)


## ... Sites ####

sites <- sites_raw


## ... Cores ####

cores <- cores_raw %>% #removing currently uncontrolled variables 
  select(-mass_accumulation_rate, -mass_accumulation_rate_error, -carbon_accumulation_rate, 
         -carbon_accumulation_rate_error, -nitrogen_accumulation_rate, -nitrogen_accumulation_rate_error, 
         -organic_matter_accumulation_rate, -organic_matter_accumulation_rate_error, 
         -inorganic_matter_accumulation_rate, -inorganic_matter_accumulation_rate_error,
         -accumulation_rate_unit)

cores <- reorderColumns("cores", cores)


## ... Depthseries ####

depthseries <- depthseries_raw 
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
table_names <- c("methods", "cores","sites", "depthseries", "impacts", "species")

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
testNumericCols(depthseries) ##testNumericCols producing error message 
test_numeric_vars(depthseries)  


## 3. Write data vis report ####
writeDataVizReport(id)


## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_methods.csv")
write_csv(cores, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_cores.csv")
write_csv(sites, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_sites.csv")
write_csv(depthseries, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_depthseries.csv")
write_csv(impacts, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_impacts.csv")
write_csv(species, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_species.csv")

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
                             bibliography_id = "Gundersen_et_al_2021",
                             publication_type = "article",
                             bibtype = "Article", 
                             title = "Long-Term Sediment, Carbon, and Nitrogen Accumulation Rates in Coastal Wetlands Impacted by Sea Level Rise",
                             author = "Gillian Gundersen, D. Reide Corbett, Austyn Long, Melinda Martinez & Marcelo Ardón",
                             doi = "https://link.springer.com/article/10.1007/s12237-021-00928-z",
                             url = "https://link.springer.com/article/10.1007/s12237-021-00928-z",
                             journal = "Estuaries and Coasts",
                             year = "2021")



dataset_citation <- data.frame(study_id = id,
                               bibliography_id = id,
                               publication_type = "primary dataset",
                               bibtype = "Misc", 
                               title = "Dataset: Long-Term Sediment, Carbon, and Nitrogen Accumulation Rates in Coastal Wetlands Impacted by Sea Level Rise",
                               author = "Gillian Gundersen, D. Reide Corbett, Austyn Long, Melinda Martinez, Marcelo Ardón",
                               doi = "10.25573/serc.25021361",
                               url = "https://doi.org/10.25573/serc.25021361",
                               year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024.bib")
write_csv(study_citations, "data/primary_studies/Gundersen_et_al_2024/derivative/Gundersen_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
