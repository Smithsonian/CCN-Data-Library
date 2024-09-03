## CCRCN Data Library ########
## contact: cheneyr@si.edu

## Hook script for Dataset: 
# 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_plots <- read_csv("data/primary_studies/Cifuentes_et_al_2024_ElSalvador/original/Cifuentes_et_al_2024_ElSalvador_plots.csv")
raw_methods <- read_csv("data/primary_studies/Cifuentes_et_al_2024_ElSalvador/original/Cifuentes_et_al_2024_ElSalvador_methods.csv")
raw_depthseries <- read_csv("data/primary_studies/Cifuentes_et_al_2024_ElSalvador/original/Cifuentes_et_al_2024_ElSalvador_depthseries.csv")
raw_plants <- read_csv("data/primary_studies/Cifuentes_et_al_2024_ElSalvador/original/Cifuentes_et_al_2024_ElSalvador_plants.csv")
raw_debris <- read_csv("data/primary_studies/Cifuentes_et_al_2024_ElSalvador/original/Cifuentes_et_al_2024_ElSalvador_debris.csv")

## 1. Curation ####

id <- "Cifuentes_et_al_2024_ElSalvador"

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>% 
  mutate(method_id = "single set of methods")

methods <- reorderColumns("methods", methods)


## ... Core Depthseries ####

# curate depthseries-level data
depthseries <- raw_depthseries %>% 
  mutate(fraction_carbon = percent_carbon/100,
         method_id = "single set of methods") %>% 
  rename(core_id = plot_id) %>% 
  select(-percent_nitrogen, -percent_carbon, -carbon_stock, -nitrogen_stock)

#remove uncontrolled variables 
## plot_id --> core_id?

depthseries <- reorderColumns("depthseries", depthseries)


## ... Plot -Level//Cores ####
#1 core per plot id - using the same naming system 

cores <- raw_plots %>% 
  rename(core_id = plot_id) %>%  ## 1 core/sediment sample per plot 
  mutate(vegetation_class = "forested",
         year = 2014,
         position_method = "", #CHECK
         position_notes = "position at plot level",
         vegetation_method = "field observation") %>% 
  select(-field_or_manipulation_code, -land_use_class, -land_use_status)

cores <- reorderColumns("cores", cores)


## ... Impacts #####

impacts <- raw_plots %>% 
  select(study_id, site_id, plot_id, land_use_class, land_use_status) %>% 
  rename(core_id = plot_id,
         impact_class = land_use_class) %>% 
  mutate(impact_class = case_when(impact_class == "cattle farm" ~ "farmed",
                                  impact_class == "low disturbance" ~ "natural",
                                  impact_class == "salt pond" ~ "agro-industrial deforestation", 
                                  TRUE ~ impact_class)) %>%
  select(- land_use_status) %>% 
  filter(!is.na(impact_class))

impacts <- reorderColumns("impacts", impacts)



## ... Species #####

#pull dominant species from plants table 

species <- raw_plants %>% 
  select(study_id, site_id, plot_id, plant_id, genus, species) %>% 
  rename(core_id = plot_id) %>% 
  mutate(code_type = "Genus species",
         species_code = paste(genus, species, sep = " ")) %>% 
  select(-genus, -species, -plant_id) %>% distinct()

species <- reorderColumns("species", species)


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)
          #leaflet does not like special character in some site names 

#table names
table_names <- c("methods", "cores", "depthseries", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

testUniqueCoords(cores)
testIDs(cores, depthseries, by = "core_id")
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## 3. Write datavis report ####
writeDataVizReport(id)
    #getting error in Pb210 depth profiles? this dataset does not have pb210 activities 

## 4. Study Citations ####

library(RefManageR)

cifuentes_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.25828603")) %>% 
  mutate(bibliography_id = "Cifuentes_et_al_2024_ElSalvador_data", 
         study_id = id, 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())

write_csv(cifuentes_bib, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_study_citations.csv") 

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(methods, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_methods.csv")
write_csv(cores, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_cores.csv") 
write_csv(depthseries, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_depthseries.csv")
write_csv(impacts, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_impacts.csv")
write_csv(species, "data/primary_studies/Cifuentes_et_al_2024_ElSalvador/derivative/Cifuentes_et_al_2024_ElSalvador_species.csv")
#plants
#plots


