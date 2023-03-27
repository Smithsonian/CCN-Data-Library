## CCN Data Library ####

## Soil core data curation script for McGlathery et al 2014
## contact: Henry Betts, BettsH@si.edu
## URL: https://doi.org/10.6073/pasta/b43fe3f3341ff2266dc50cfb8f47d026


## 1. Set up ####

# Load necessary libraries
library(tidyverse)
library(lubridate) # for as.Date()
library(leaflet)
library(RefManageR)
library(sf) # for UTM_to_DD()


# Load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# Link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# Read in data
raw_data <- read.csv("./data/primary_studies/McGlathery_et_al_2018/original/210Pb_Dataset_VCR.csv")


## 2. Organize tables ####
depth_raw <- raw_data %>% 
  drop_na() %>% 
  rename(core_id = Core,
         depth_min = Core_Top,
         depth_max = Core_Bottom,
         dry_bulk_density = C,
         excess_pb210_activity = Lead210) %>% 
  mutate(core_id = gsub("-", "_", core_id),
         northing = ifelse(core_id == "SB152_C2", UTMY, UTMX), 
         easting = ifelse(core_id == "SB152_C2", UTMX, UTMY),
         dates = as.Date(Date_Collected, "%Y-%m-%d"),
         year = year(dates),
         month = month(dates),
         day = day(dates),
         method_id = "single set of methods",
         zone = 18,
         study_id = "McGlathery_et_al_2018",
         site_id = ifelse(grepl("SB", core_id), "southbay", "other"), # EDIT: find name of second site
         pb210_unit = "disintegrationsPerMinutePerGram") %>% 
  UTM_to_DD() %>% 
  distinct() 

depthseries <- depth_raw %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, 
         excess_pb210_activity, pb210_unit)

cores <- depth_raw %>% 
  mutate(vegetation_class = ifelse(grepl("2B|B5", core_id), "mudflat", "seagrass"),
         vegetation_method = "field observation",
         habitat = "seagrass",
         core_length_flag = "core depth limited by length of corer") %>% 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, 
         vegetation_class, vegetation_method, habitat, core_length_flag) %>% 
  distinct()

methods <- data.frame(study_id = "McGlathery_et_al_2018",
                         method_id = "single set of methods",
                         coring_method = "pvc and hammer",
                         roots_flag = "", # EDIT: ask about this and next one?
                         sediment_sieved_flag = "",
                         compaction_flag = "not specified",
                         dry_bulk_density_temperature = 60,
                         dry_bulk_density_flag = "to constant mass",
                         loss_on_ignition_temperature = 500,
                         loss_on_ignition_time = 6,
                         loss_on_ignition_flag = "not specified",
                         carbon_measured_or_modeled = "measured",
                         carbonates_removed = FALSE,
                         carbonate_removal_method = "carbonates not removed",
                         fraction_carbon_method = "EA",
                         fraction_carbon_type = "organic carbon",
                         pb210_counting_method = "alpha")


## 3. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods")

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
testNumericCols(depthseries)


## 4. Bibliography ####
study_citation <- data.frame(study_id = "McGlathery_et_al_2018",
                             bibliography_id = "McGlathery_et_al_2018_data",
                             title = "Lead 210 profiles in sediment cores in South Bay, Virginia",
                             author = "McGlathery, Karen; Greiner, Jill; Gunnell, John; McKee, Brent; Oreska, Matthew; Bost, Molly",
                             publication_type = "primary dataset",
                             doi = "10.6073/pasta/b43fe3f3341ff2266dc50cfb8f47d026",
                             url = "https://doi.org/10.6073/pasta/b43fe3f3341ff2266dc50cfb8f47d026",
                             bibtype = "Misc",
                             year = "2014",
                             month = "jun",
                             day = "17")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "./data/primary_studies/McGlathery_et_al_2018/final/McGlathery_et_al_2018_cores.csv") 
write_csv(depthseries, "./data/primary_studies/McGlathery_et_al_2018/final/McGlathery_et_al_2018_depthseries.csv")
write_csv(methods, "./data/primary_studies/McGlathery_et_al_2018/final/McGlathery_et_al_2018_methods.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/McGlathery_et_al_2018/final/McGlathery_et_al_2018_study_citations.bib")
write_csv(study_citation, "./data/primary_studies/McGlathery_et_al_2018/final/McGlathery_et_al_2018_study_citations.csv")





