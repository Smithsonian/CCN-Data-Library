## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Rovai et al 2022 dataset: Rovai, Andre; Twilley, Robert; Castaneda-Moya, Edward; Riul, Pablo; Cifuentes-Jara, Miguel;
  # Manrow-Villalobos, Marilyn; et al. (2023): Dataset: Global controls on carbon storage in mangrove soils. Smithsonian Environmental Research Center.

## Dataset: https://smithsonian.figshare.com/articles/dataset/Dataset_Global_controls_on_carbon_storage_in_mangrove_soils/21295713
# Assoc. publication: https://www.nature.com/articles/s41558-018-0162-5


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
methods_raw <- read_csv("data/primary_studies/Rovai_et_al_2022/original/Rovai_et_al_2022_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Rovai_et_al_2022/original/Rovai_et_al_2022_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Rovai_et_al_2022/original/Rovai_et_al_2022_depthseries.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Rovai_et_al_2022"


## ... Methods ####

methods <- methods_raw %>% mutate(study_id = id)
methods <- reorderColumns("methods", methods)


## ... Sites ####


## ... Cores ####

cores <- cores_raw %>% mutate(study_id = id,
                              salinity_class = "estuarine",
                              salinity_method = "field observation")
  
cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw %>% mutate(study_id = id)
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####


## ... Impacts ####


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

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Rovai_et_al_2022/derivative/Rovai_et_al_2022_methods.csv")
  #write_csv(sites, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Rovai_et_al_2022/derivative/Rovai_et_al_2022_cores.csv")
write_csv(depthseries, "data/primary_studies/Rovai_et_al_2022/derivative/Rovai_et_al_depthseries.csv")
  #write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

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
                             bibliography_id = "Rovai_et_al_2022",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Global controls on carbon storage in mangrove soils",
                             author = "Andre Rovai, Robert Twilley, Edward Castaneda-Moya, Pablo Riul, Miguel Cifuentes-Jara,
                             Marilyn Manrow-Villalobos, Paulo A. Horta, Jose C. Simonassi, Alessandra L. Fonseca, 
                             Paulo R. Pagliosa, Danillo Torres",
                             doi = "https://doi.org/10.25573/serc.21295713.v1",
                             url = "https://smithsonian.figshare.com/articles/dataset/Dataset_Global_controls_on_carbon_storage_in_mangrove_soils/21295713",
                             year = "2022")



study_citation_article <- data.frame(bibliography_id = "Rovai_et_al_2018",
                                     title = "Global controls on carbon storage in mangrove soils",
                                     author = "André S. Rovai, Robert R. Twilley, Edward Castañeda-Moya, Pablo Riul, Miguel Cifuentes-Jara, 
                                     Marilyn Manrow-Villalobos, Paulo A. Horta, José C. Simonassi, Alessandra L. Fonseca & Paulo R. Pagliosa", 
                                     bibtype = "Article",
                                     doi = "https://doi.org/10.1038/s41558-018-0162-5",
                                     url = "https://www.nature.com/articles/s41558-018-0162-5#Abs1",
                                     journal = "Nature Climate Change",
                                     publication_type = "associated source",
                                     year = "2018") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("Rovai_et_al_2022", "Rovai_et_al_2018"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Rovai_et_al_2022/derivative/Rovai_et_al_2022.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
