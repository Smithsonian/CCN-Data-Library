## CCRCN Data Library ########
## contact: cheneyr@si.edu

## Hook script for Dataset: Study of total economic valuation of the main services provided by mangroves in the Gulf of Chiriquí, Panama
## https://smithsonian.figshare.com/articles/dataset/Dataset_Study_of_total_economic_valuation_of_the_main_services_provided_by_mangroves_in_the_Gulf_of_Chiriqu_Panama/24294928


# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_plots <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_plots.csv")
raw_methods <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_methods.csv")
raw_depthseries <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_depthseries.csv")
raw_plants <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_plants.csv")
raw_crops <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_crops.csv")
raw_debris <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_debris.csv")
raw_understory <- read_csv("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_understory.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>% 
  mutate(method_id = "single set of methods")

## ...Plots ####
plots <- raw_plots %>% 
  mutate(plot_id = paste(site_id, plot_id, sep = "_"),
         plot_shape = "circular", 
         habitat = "mangrove") %>% 
  rename(
    # plant_plot_carbon = total_C_trees,
         # sapling_carbon = total_C_saplings,
         aboveground_plant_carbon = AGC_total,
         belowground_plant_carbon = root_carbon) %>% 
  select(-c(AGC_saplings, BGC_saplings, total_C_saplings, AGC_trees, BGC_trees, total_C_trees,
            soil_carbon_50cm, BGC_50cm:TEC_300cm))

## ... Vegetation ####

plants <- raw_plants %>% 
  mutate(plot_id = paste(site_id, plot_id, sep = "_")) %>% 
  rename(plant_belowground_mass = plant_BGB_kg,
         plant_aboveground_mass = plant_AGB_kg) %>% 
  select(-c(family, plant_AGB_MgHa:plant_total_carbon)) 
  
equation_input <- raw_plants %>% distinct(species, wood_density, carbon_conversion_factor)

## ... Core Depthseries ####

# curate depthseries-level data
depthseries <- raw_depthseries %>% 
  mutate(fraction_carbon = percent_carbon/100,
         method_id = "single set of methods") %>% 
  select(-plot_id, -percent_nitrogen, -percent_carbon,-soil_carbon)

depthseries <- reorderColumns("depthseries", depthseries)

## ... Core-Level ####

cores <- plots %>% 
  filter(plot_id %in% unique(depthseries$core_id)) %>% 
  mutate(core_id = plot_id) %>% 
  select(study_id, site_id, plot_id, core_id, latitude, longitude, position_method, position_notes, year) %>% 
  mutate(habitat = "mangrove",
         vegetation_class = "forested",
         vegetation_method = "measurement") 

#get site level latitude and longitude
# latlong <- raw_plots %>% 
#   select(site_id, plot_id, latitude, longitude, position_method, position_notes, year) %>% 
#   distinct()
# 
# # curate core-level data
# cores_raw <- raw_depthseries %>% 
#   select(study_id, site_id, core_id, plot_id) %>% 
#   distinct()
# 
# cores <- full_join(cores_raw, latlong, by = c("site_id", "plot_id")) %>% 
#   mutate(habitat = "mangrove",
#          vegetation_class = "forested",
#          vegetation_method = "measurement") %>% 
#   select(-plot_id) %>% 
#   drop_na() #remove plots that are not included in depthseries table

# cores <- reorderColumns("cores", cores)

## ... Impacts #####

#core list 
# core.list <- cores_raw %>% 
#   select(site_id, plot_id, core_id)

impacts <- plots %>% 
  select(study_id, site_id, plot_id, land_use_class) %>% 
  # right_join(core.list, by = c("site_id", "plot_id")) %>% 
  mutate(core_id = plot_id,
         impact_class = recode(land_use_class, 
                               "no disturbance" = "natural",
                               # generalize harvesting impact
                               "firewood extraction" = "wood harvesting",
                               "bark extraction" = "wood harvesting")) %>% 
  select(-land_use_class)

# impacts <- reorderColumns("impacts", impacts)
  
          ## need to recategorize the rest of listed impact classes 


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3)
          #leaflet does not like special character in some site names 

#table names


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

## 3. Study Citations ####

library(RefManageR)

cifuentes_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.24294928")) %>% 
  mutate(bibliography_id = "Cifuentes_et_al_2023_Panama_data", 
         study_id = "Cifuentes_et_al_2023_Panama", 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())

write_csv(cifuentes_bib, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_et_al_2023_Panama_study_citations.csv") 

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_excel_csv(cores, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_et_al_2023_Panama_cores.csv") 
write_excel_csv(depthseries, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_et_al_2023_Panama_depthseries.csv")
write_excel_csv(methods, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_et_al_2023_Panama_methods.csv")
write_excel_csv(impacts, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_et_al_2023_Panama_impacts.csv")
write_excel_csv(plots, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_2023_Panama_plots.csv")
write_excel_csv(plants, "data/primary_studies/Cifuentes_2023_Panama/derivative/Cifuentes_2023_Panama_plants.csv")

