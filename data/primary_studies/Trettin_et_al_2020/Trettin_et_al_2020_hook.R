## CCN Data Library ####

## Soil core data curation script for Trettin et al 2020
## contact: Henry Betts, BettsH@si.edu

## URL: https://doi.org/10.2737/RDS-2020-0040


## 1. Set up ####

# Load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

# Load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# Link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# Read in data
soils_raw <- read.csv("./data/primary_studies/Trettin_et_al_2020/original/Gabon_Soils.csv")
plots_raw <- read.csv("./data/primary_studies/Trettin_et_al_2020/original/Gabon_Plots.csv")
methods_raw <- read_excel("./data/primary_studies/Trettin_et_al_2020/original/trettin_et_al_2020_methods.xlsx", sheet = 2)
species_over <- read.csv("./data/primary_studies/Trettin_et_al_2020/original/Gabon_Overstory.csv")
species_under <- read.csv("./data/primary_studies/Trettin_et_al_2020/original/Gabon_Understory.csv")


## 2. Clean data ####
depthseries <- soils_raw %>% 
  rename(core_id = Plot.ID,
         dry_bulk_density = Db..g.cm3.) %>% 
  separate(Depth..cm., into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(study_id = "Trettin_et_al_2020",
         method_id = "single set of methods",
         site_id = core_id) %>% 
  select(study_id, method_id, site_id, core_id, depth_min, depth_max, dry_bulk_density)
  
cores <- plots_raw %>% 
  rename(core_id = plot.ID) %>% 
  mutate(year = 2017,
         month = 02,
         day = 24,
         site_id = core_id,
         position_method = "handheld",
         habitat = "mangrove",
         core_length_flag = "core depth limited by length of corer",
         vegetation_class = "forested",
         vegetation_method = "measurement",
         study_id = "Trettin_et_al_2020") %>% 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, position_method, vegetation_class, vegetation_method,
         habitat, core_length_flag)

sites <- cores %>% 
  mutate(site_longitude_min = 9.26218122,
         site_longitude_max = 10.01052171,
         site_latitude_min = 1.1322898,
         site_latitude_max = 0.3590689,
         site_description = "Pongara National Park, Gabon, West Africa") %>% 
  select(study_id, site_id, site_description, site_latitude_max, site_latitude_min, site_longitude_max, site_longitude_min,
         vegetation_class, vegetation_method)

methods <- methods_raw %>% 
  select(where(notAllNA))

species <- full_join(species_over, species_under, by = c("Plot.ID", "Tree.Number", "Species.Code")) %>% 
  drop_na(Tree.Number) %>% 
  mutate(species_code = ifelse(Species.Code == 3, "Rhizophora harrisonii",
                               ifelse(Species.Code == 5, "Rhizophora racemosa", NA)),
         habitat = 'mangrove',
         code_type = "Genus species",
         study_id = "Trettin_et_al_2020",
         site_id = Plot.ID,
         core_id = site_id) %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)


## 3. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~site_id)

## Table testing
table_names <- c("cores", "depthseries", "sites", "methods", "species")

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


## 4. Bibliography ####
study_citation <- data.frame(study_id = "Trettin_et_al_2020",
                             bibliography_id = "Trettin_et_al_2020_data",
                             title = "Carbon stock inventory of mangroves, Pongara National Park, Gabon",
                             author = "Trettin, Carl C.; Dai, Zhaohua; Tang, Wenwu; Lagomasino, David; Thomas, Nathan; Lee, Seung-Kuk; Ebanega, Médard Obiang; Simard, Marc; Fatoyinbo, Temilola E.",
                             publication_type = "associated data",
                             doi = "10.2737/RDS-2020-0040",
                             url = "https://doi.org/10.2737/RDS-2020-0040",
                             bibtype = "Misc",
                             year = "2020",
                             month = "feb",
                             day = "24")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_cores.csv") 
write_csv(depthseries, "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_depthseries.csv")
write_csv(species, "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_methods.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_study_citation.bib")
write_csv(study_citation, "./data/primary_studies/Trettin_et_al_2020/derivative/trettin_et_al_2020_study_citations.csv")




