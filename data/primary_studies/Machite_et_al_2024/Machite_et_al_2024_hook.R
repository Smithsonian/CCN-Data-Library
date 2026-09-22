## CCRCN Data Library ########
## contact: cheneyr@si.edu

## Hook script for South Africa Synthesis 
# https://doi.org/10.25573/serc.24394426

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_methods <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_materials_and_methods.csv")
raw_sites <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_sites.csv")
raw_depthseries <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_depthseries.csv")
raw_cores <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_cores.csv")
raw_species <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_species.csv")
citations <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_study_citations.csv")
dataset_authors <- read_csv("data/primary_studies/Machite_et_al_2024/original/Machite_et_al_2024_dataset_authors.csv")

## 1. Curation ####

id <- "Machite_et_al_2024"

## ... Methods ####

# curate materials and methods
methods <- raw_methods 
methods <- reorderColumns("methods", methods)


## ... Cores ####

# curate core-level data
 # remove uncontrolled "sampling_season"
 # note whether surface sample in core_notes
cores <- raw_cores %>% 
  select(-sampling_season) %>% 
  mutate(core_notes = ifelse(grepl("surface", core_id), "surface sample, 0-5 cm", NA))

cores <- reorderColumns("cores", cores)


## ... Sites #####

sites <- raw_sites

## to review 

## ... Depthseries ####

#remove uncontrolled attributes, grain size and redox data 

depthseries <- raw_depthseries %>% 
  select(-redox, -sand, -silt, -clay, -moisture) %>% 
  mutate(method_id = study_id)

depthseries <- reorderColumns("depthseries", depthseries)

## ... Species #####

species <- raw_species
species <- reorderColumns("species", species)


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3)

#table names
table_names <- c("methods", "cores", "depthseries", "sites", "species")


# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


#check depth min depth max relationship 
depthseries_check <- depthseries %>% 
  mutate(depth_check = depth_max-depth_min)
print(unique(depthseries_check$depth_check))


## 3. Write datavis report ####
writeDataVizReport(id)

## 4. Study Citations ####

library(RefManageR)

synthesis_bib <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.25573/serc.24394426.v1")) %>% 
  mutate(bibliography_id = "Machite_et_al_2024", 
         study_id = "Machite_et_al_2024", 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())

#associated publications citations, table pulled from data release 
citations


human_et_al_2022 <- data.frame(study_id = "Human_et_al_2022",
                               bibliography_id = "Human_et_al_2022",
                               publication_type = "primary source",
                               bibtype = "Article",
                               title = "Blue carbon and nutrient stocks in salt marsh and seagrass from an urban African estuary",
                               author = "Lucienne R.D. Human, Jessica Els, Johan Wasserman, Janine B. Adams",
                               doi = "http://dx.doi.org/10.1016/j.scitotenv.2022.156955",
                               url = "http://dx.doi.org/10.1016/j.scitotenv.2022.156955",
                               journal = "Science of the Total Environment",
                               year = "2022")

mbense_2019 <- data.frame(study_id = "Mbense_2019",
                          bibliography_id = "Mbense_2019",
                          publication_type = "primary source",
                          bibtype = "Misc",
                          title = "",
                          author = "",
                          doi = "",
                          url = "",
                          journal = "Standard Theses",
                          year = "2019")

#join primary dataset and assoc pubs, write study_citations.csv
study_citations <- rbind(citations, human_et_al_2022, mbense_2019) %>% 
  full_join(synthesis_bib) %>% 
  mutate(publication_type = ifelse(publication_type == "primary source", "associated source", publication_type))

write_csv(study_citations, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_study_citations.csv") 

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_cores.csv") 
write_csv(depthseries, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_depthseries.csv")
write_csv(methods, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_materials_and_methods.csv")
write_csv(species, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_species.csv")
write_csv(sites, "data/primary_studies/Machite_et_al_2024/derivative/Machite_et_al_2024_sites.csv")
# write_csv(impacts, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_impacts.csv")


