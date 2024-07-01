## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for White and Spera 2024 dataset: Tidal and nontidal marsh restoration: a trade-off between carbon sequestration, methane emissions, and soil accretion

#link to dataset: https://doi.org/10.25573/serc.20807278 


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
methods_raw <- read_csv("data/primary_studies/White_and_Spera_2024/original/white_and_spera_2024_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/White_and_Spera_2024/original/white_and_spera_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/White_and_Spera_2024/original/white_and_spera_2024_depthseries.csv")
study_citations_raw <- read_csv("data/primary_studies/White_and_Spera_2024/original/white_and_spera_2024_associated_publications.bib")
  
  
## 1. Curation ####

id <- "White_and_Spera_2024"


## ... Methods ####

methods <- methods_raw %>% 
  mutate(method_id = "single set of methods",
         fraction_carbon_method = "EA")

methods <- reorderColumns("methods", methods)


## ... Cores ####

cores <- cores_raw %>% 
  mutate(vegetation_class = case_when(vegetation_class == "floating emergent" ~ "emergent", #classify this in veg notes 
                                      TRUE ~ vegetation_class),
         habitat = ifelse(is.na(habitat), "marsh", habitat))


cores <- reorderColumns("cores", cores)



## ... Depthseries ####

depthseries <- depthseries_raw %>%
  mutate(method_id = "single set of methods",
         fraction_carbon = total_carbon/1000) %>% 
  select(study_id, method_id, site_id, core_id, depth_min, depth_max, dry_bulk_density,
         fraction_organic_matter, depth_interval_notes, fraction_carbon) %>%  #remove uncontrolled variables 
  filter(!is.na(dry_bulk_density) & !is.na(fraction_organic_matter & !is.na(fraction_carbon))) #remove samples with only uncontrolled variables recorded 

depthseries <- reorderColumns("depthseries", depthseries)



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
write_csv(methods, "data/primary_studies/White_and_Spera_2024/derivative/White_and_Spera_2024_methods.csv")
write_csv(cores, "data/primary_studies/White_and_Spera_2024/derivative/White_and_Spera_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/White_and_Spera_2024/derivative/White_and_Spera_2024_depthseries.csv")


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
                             bibliography_id = "White_and_Spera_2024_data",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Tidal and nontodal marsh restoration: a trade-off between carbon sequestration, methane emissions,and soil accretion",
                             author = "Alina C. Spera, John R. White",
                             doi = "10.25573/serc.20807278",
                             url = "https://doi.org/10.25573/serc.20807278.v1",
                             year = "2024")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/White_and_Spera_2024/derivative/White_and_Spera_2024.bib")
write_csv(study_citation, "data/primary_studies/White_and_Spera_2024/derivative/White_and_Spera_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/

## ... Write data vis report in docs folder ####
writeDataVizReport(id)

