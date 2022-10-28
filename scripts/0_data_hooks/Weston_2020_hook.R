## CCN Data Library ########

## Soil core data curation script for Weston et al 2020
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://smithsonian.figshare.com/articles/dataset/Dataset_Recent_Acceleration_of_Coastal_Wetland_Accretion_Along_the_U_S_East_Coast/13043054
# Assoc. publication: ??


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
methods_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2020_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2020_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2020_depthseries.csv")
species_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2020_species.csv")



## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Weston_et_al_2020"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

methods <-  methods_raw %>% mutate(method_id = "single set of methods",
                                   excess_pb210_rate = "depth",
                                   excess_pb210_model = "CRS") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####

# if necessary, curate site-level data

## ... Cores ####
cores <- cores_raw %>% rename(year = core_year, 
                              month = core_month,
                              day = core_day,
                              latitude = core_latitude,
                              longitude = core_longitude,
                              elevation = core_elevation,
                              elevation_datum = core_elevation_datum,
                              elevation_method = core_elevation_method,
                              elevation_notes = core_elevation_notes) %>% 
                        mutate(position_method = "RTK",
                               elevation_method = "RTK",
                               vegetation_class = "emergent",
                               vegetation_method = "field observation",
                               habitat = "marsh")

## ... Depthseries ####

depthseries <- depthseries_raw %>% mutate(method_id = "single set of methods") %>% 
                                   relocate(method_id, .after = study_id) %>% 
                                   select(!depth_interval_notes) %>% 
                                   mutate(cs137_unit = "becquerelsPerKilogram",
                                          pb210_unit = "becquerelsPerKilogram",    ##check units?? not in data pub/based off of guidance 
                                          pb214_unit = "becquerelsPerKilogram",
                                          bi214_unit = "becquerelsPerKilogram")

## ... Species ####

species <- species_raw %>% mutate(code_type = "Genus species",
                           species_code = paste(genus, species),
                           habitat = "marsh") %>% 
                           select(!c("genus", "species"))



## ... Impacts ####

# if provided, curation table of anthropogenic impacts

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
testIDs(cores, depthseries, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
      #testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_methods.csv")
  #write_csv(sites, "data/primary_studies/Weston_et_al_2020/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_cores.csv")
write_csv(depthseries, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_species.csv")
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

study_citation <- data.frame(bibliography_id = "Weston_et_al_2022",
                             title = "Dataset: Recent Acceleration of Coastal Wetland Accretion Along the U.S. East Coast",
                             author = "Nathaniel B Weston and Elise Rodriguez and Brian Donnelly and Elena Solohin and Kristen Jezycki and Sandra Demberger 
                             and Lori Sutter and James T. Morris and Scott C. Neubauer and Christopher B Craft",
                             bibtype = "Misc", 
                             doi = "10.25573/serc.13043054.v1",
                             url = "https://smithsonian.figshare.com/articles/dataset/Dataset_Recent_Acceleration_of_Coastal_Wetland_Accretion_Along_the_U_S_East_Coast/13043054",
                             year = "2022") %>% 
                  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
