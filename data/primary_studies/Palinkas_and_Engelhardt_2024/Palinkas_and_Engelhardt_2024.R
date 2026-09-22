## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Palinkas and Engelhardt 2024 dataset: Spatial and temporal patterns of modern sedimentation in a tidal freshwater marsh

## Dataset: https://doi.org/10.25573/serc.24470152

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
methods_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_methods.csv")
cores_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_depthseries.csv")
sites_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_sites.csv")
species_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_species.csv")
impacts_raw <- read_csv("data/primary_studies/Palinkas_and_Engelhardt_2024/original/Palinkas_and_Engelhardt_2024_impacts.csv")


  
## 1. Curation ####

id <- "Palinkas_and_Engelhardt_2024"


## ... Methods ####

methods <- methods_raw %>% 
  mutate(excess_pb210_rate = "cumulative mass")

methods <- reorderColumns("methods", methods)


## ... Sites ####

# there's no information here that isn't present at core-leve resolution
# probly a result of the author trying to fill out all the templates
# sites <- sites_raw

## ... Cores ####

cores <- cores_raw %>% 
  mutate(salinity_method = "field observation",
         vegetation_method = "field observation") %>% 
  select(-pb210_profile_max_age)# uncontrolled attribute


cores <- reorderColumns("cores", cores) %>% 
  mutate(core_notes = unique(impacts_raw$impact_notes))

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(method_id = "single set of methods")
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

species <- species_raw
species <- reorderColumns("species", species)

## ... Impacts ####

impacts <- impacts_raw %>% mutate(impact_class = "natural") %>% 
  select(-impact_notes)
impacts <- reorderColumns("impacts", impacts)

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


### 3. Write Data vis report ####
writeDataVizReport(id)
  #no carbon depth profiles, function quits 

## 4. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_materials_and_methods.csv")
write_csv(cores, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_cores.csv")
write_csv(sites,"data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_sites.csv")
write_csv(depthseries, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_depthseries.csv")
write_csv(impacts, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_impacts.csv")
write_csv(species, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_species.csv")

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
paper_citation <- dataset_citation <- data.frame(study_id = "Palinkas_and_Engelhardt_2024",
                                                 bibliography_id = "Palinkas_and_Engelhardt_2015",
                                                 publication_type = "article",
                                                 bibtype = "Article", 
                                                 title = "Spatial and temporal patterns of modern (~100 yr) sedimentation in a tidal freshwater marsh: Implications for future sustainability",
                                                 author = "Cindy M. Palinkas, Katharina A. M. Engelhardt",
                                                 doi = "10.1002/lno.10202",
                                                 url = "https://doi.org/10.1002/lno.10202",
                                                 journal = "Limnology and Oceanography",
                                                 year = "2015")



dataset_citation <- data.frame(study_id = "Palinkas_and_Engelhardt_2024",
                               bibliography_id = "Palinkas_and_Engelhardt_2024",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Spatial and temporal patterns of modern (~100 yr) sedimentation in a tidal freshwater marsh: Implications for future sustainability",
                             author = "Cindy M. Palinkas, Katharina A. M. Engelhardt",
                             doi = "10.25573/serc.24470152",
                             url = "https://doi.org/10.25573/serc.24470152.v1",
                             year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024.bib")
write_csv(study_citations, "data/primary_studies/Palinkas_and_Engelhardt_2024/derivative/Palinkas_and_Engelhardt_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
