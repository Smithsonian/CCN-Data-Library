## CCRCN Data Library ########
## contact: cheneyr@si.edu

## Hook script for Dataset: Land use dynamics and mitigation potential of the mangroves of the Gulf of Nicoya, Costa Rica
# https://doi.org/10.25573/serc.24943866 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_plots <- read_csv("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_plots.csv")
raw_methods <- read_csv("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_methods.csv")
raw_depthseries <- read_csv("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_depthseries.csv")
raw_plants <- read_csv("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_plants.csv")
raw_debris <- read_csv("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_debris.csv")

## 1. Curation ####

id <- "Cifuentes_et_al_2024_Nicoya"

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
  select(-percent_nitrogen, -soil_carbon, -percent_carbon)

## soil_carbon = Soil carbon calculated as the product of the fraction carbon content, dry bulk density, and depth interval. (g/cm3)
## plot_id --> core_id?

depthseries <- reorderColumns("depthseries", depthseries)

## ... Cores ####

cores <- raw_plots %>% 
  select(study_id, site_id, plot_id, year, latitude, longitude, position_method, habitat) %>% 
  mutate(habitat = "mangrove", # going to assign all as mangrove habitat (even for sites that were formally mangrove)
         core_id = plot_id) %>% 
  relocate(core_id, .after = plot_id)
  # rename(core_id = plot_id) ## 1 core/sediment sample per plot 

cores <- reorderColumns("cores", cores)

## missing lat long for site 
    # Buenaventura_Camaronera_4_x

## ... Plots ####

plots <- raw_plots %>% 
  mutate(habitat = "mangrove",
         land_use_class = recode(land_use_class, "low disturbance" = "natural"),
         plot_plant_carbon = AGC_total + root_carbon) %>% 
  select(-c(AGC_trees:TEC_300cm))

## ... Vegetation ####

plants <- raw_plants %>% 
  # removing derived values for now 
  select(-c(wood_density:plant_total_carbon)) %>% 
  select(-c(family, height_canopy))

## ... Impacts #####

impacts <- raw_plots %>% 
  select(study_id, site_id, plot_id, land_use_class, land_use_status) %>% 
  rename(core_id = plot_id) %>% 
  # lets just have farmed and natural for now, 
  # agro-industrial deforestation was the conversion event that allowed for the farming
  mutate(impact_class = case_when(land_use_class == "shrimp pond" ~ "farmed", #check impact classes 
                                  land_use_class == "low disturbance" ~ "natural",
                                  land_use_class == "salt pond" ~ "agro-industrial deforestation"),
         impact_class2 = case_when(startsWith(core_id, "Buenaventura_Camaronera") ~ "agro-industrial deforestation",
                                   startsWith(core_id, "Isla Chira_Camaronera") ~ "agro-industrial deforestation",
                                   startsWith(core_id, "Jicaral_Camaronera") ~ "agro-industrial deforestation",
                                   startsWith(core_id, "Lepanto_Camaronera") ~ "agro-industrial deforestation")) %>%
  # pivot_longer(cols = c(impact_class, impact_class2), values_to = "impact_class") %>% 
  select(-c(land_use_status, land_use_class, impact_class2)) %>% 
  filter(!is.na(impact_class))

impacts <- reorderColumns("impacts", impacts)

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

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## 3. Write datavis report ####
writeDataVizReport(id)
    #getting error in Pb210 depth profiles? this dataset does not have pb210 activities 

## 4. Study Citations ####

library(RefManageR)

cifuentes_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.24943866")) %>% 
  mutate(bibliography_id = "Cifuentes_et_al_2024_Nicoya_data", 
         study_id = "Cifuentes_et_al_2024_Nicoya", 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())

write_excel_csv(cifuentes_bib, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_study_citations.csv") 

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_excel_csv(cores, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_cores.csv") 
write_excel_csv(depthseries, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_depthseries.csv")
write_excel_csv(methods, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_methods.csv")
write_excel_csv(impacts, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_impacts.csv")
write_excel_csv(plants, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_plants.csv")
write_excel_csv(plots, "data/primary_studies/Cifuentes_et_al_2024_Nicoya/derivative/Cifuentes_et_al_2024_Nicoya_plots.csv")


