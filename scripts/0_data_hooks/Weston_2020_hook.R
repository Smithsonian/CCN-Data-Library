## CCN Data Library ########

## Soil core data curation script for Weston et al 2023
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://smithsonian.figshare.com/articles/dataset/Dataset_Recent_Acceleration_of_Coastal_Wetland_Accretion_Along_the_U_S_East_Coast/13043054

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
# library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

#load in data 
methods_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2023_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2023_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2023_depthseries.csv")
species_raw <- read_csv("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2023_species.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Weston_et_al_2023"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

methods <-  methods_raw %>% mutate(method_id = "single set of methods",
                                   # excess_pb210_rate = "depth",
                                   # excess_pb210_model = "CRS and CIC",
                                   dry_bulk_density_flag = "not specified",
                                   # age_depth_model_reference = "CE",
                                   dating_notes = "CRC model used to determine age-dependent rates of soil accretion for each section of soil cores. CIC model used to calculate a whole-core accretion rate using Pb210 activities."
                                   )

#reorder columns 
methods <- reorderColumns("methods", methods)

## ... Cores ####
cores <- cores_raw %>% 
  mutate(position_method = "RTK",
         vegetation_class = "emergent",
         vegetation_method = "field observation",
         habitat = "marsh") %>% 
  reorderColumns("cores", .)

## ... Depthseries ####

depthseries <- depthseries_raw %>% mutate(method_id = "single set of methods") %>% 
                                   select(!depth_interval_notes) %>% 
                                   mutate(cs137_unit = ifelse(!is.na(cs137_activity), "microcuriesPerGram", NA),
                                          pb210_unit = ifelse(!is.na(total_pb210_activity), "microcuriesPerGram", NA),
                                          pb214_unit = ifelse(!is.na(pb214_activity), "microcuriesPerGram", NA),
                                          bi214_unit = ifelse(!is.na(bi214_activity), "microcuriesPerGram", NA))

depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

species <- species_raw
  # mutate(species_code = paste(genus, species),
  #        code_type = "Genus species",
  #        habitat = "marsh") %>% 
  # select(!c("genus", "species"))


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
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
      #testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023_methods.csv")
  #write_csv(sites, "data/primary_studies/Weston_et_al_2020/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023_species.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####
  
# read in data and article citations
release_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.13043054")) %>% 
  mutate(bibliography_id = "Weston_et_al_2023_data", publication_type = "primary dataset")
pub_bib <- as.data.frame(ReadBib("data/primary_studies/Weston_et_al_2020/original/Weston_et_al_2023.bib")) %>% 
  mutate(bibliography_id = "Weston_et_al_2023_article", publication_type = "primary source")

study_citation <- bind_rows(release_bib, pub_bib) %>% 
  mutate(study_id = id) %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, bibtype, everything())
  
weston_bib <- study_citation %>% select(-study_id, -publication_type) %>%   
                  column_to_rownames("bibliography_id")

write_csv(study_citation, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023_study_citations.csv")
WriteBib(as.BibEntry(weston_bib), "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2023.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
