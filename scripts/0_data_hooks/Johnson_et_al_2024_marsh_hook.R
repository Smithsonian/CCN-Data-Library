## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Johnson et al 2024 eelgrass dataset: Sediment carbon content of coastal Maine eelgrass beds

## Dataset: https://doi.org/10.25573/serc.22779893.v1 

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
methods_raw <- read_csv("data/primary_studies/Johnson_et_al_seagrass_2024/original/johnson_et_al_2024_eelgrass_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Johnson_et_al_seagrass_2024/original/johnson_et_al_2024_eelgrass_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Johnson_et_al_seagrass_2024/original/johnson_et_al_2024_eelgrass_depthseries.csv")
species_raw <- read_csv("data/primary_studies/Johnson_et_al_seagrass_2024/original/johnson_et_al_2024_eelgrass_species.csv")
study_citations_raw <- read_csv("data/primary_studies/Johnson_et_al_seagrass_2024/original/Johnson_et_al_2024_eelgrass_study_citations.csv")
  
  
## 1. Curation ####

id <- "Johnson_et_al_2024_eelgrass"


## ... Methods ####

methods <- methods_raw %>% 
  mutate(study_id = id,
         fraction_carbon_type = "organic carbon") # fraction carbon == organic carbon 

methods <- reorderColumns("methods", methods)


## ... Cores ####

cores <- cores_raw %>% 
  mutate(study_id = id,
         site_id = ifelse(grepl("Head", site_id), "Head_of_Bay", site_id))

cores <- reorderColumns("cores", cores)



## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  rename(fraction_carbon = fraction_organic_carbon) %>% #note in methods this is organic carbon
  mutate(method_id = ifelse(study_id == "Doyle_2018", "Doyle_2018", "Sonshine_2012"),
         study_id = id) %>% 
    select(-fraction_inorganic_carbon, -total_cn_ratio, -fraction_total_carbon, -inorganic_cn_ratio,
           -sample_volume, -sample_mass, -carbon_density, -pu_activity, -pu__unit) #remove uncontrolled variables 

depthseries <- reorderColumns("depthseries", depthseries)



## ... Species ####

species <- species_raw %>% 
  mutate(study_id = id)


species <- reorderColumns("species", species)



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
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass_methods.csv")
write_csv(cores, "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass_cores.csv")
write_csv(depthseries, "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass_depthseries.csv")
write_csv(species, "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass_species.csv")

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
                             bibliography_id = "Johnson_et_al_2024_eelgrass_data",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Sediment carbon content of coastal Maine eelgrass beds",
                             author = "Beverly Johnson, Emily Sonshine, John Doyle",
                             doi = "10.25573/serc.22779893",
                             url = "https://doi.org/10.25573/serc.22779893.v1",
                             year = "2024")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass.bib")
write_csv(study_citation, "data/primary_studies/Johnson_et_al_seagrass_2024/derivative/Johnson_et_al_2024_eelgrass.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
