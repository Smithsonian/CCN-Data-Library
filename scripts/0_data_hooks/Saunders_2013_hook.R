## CCN Data Library ####

## Soil core data curation script for Saunders 2011
## contact: Henry Betts, BettsH@si.edu
## URL: https://doi.org/10.6073/pasta/c0cb8ff0f150e429674ecf0db15bedc5


## 1. Set up ####

# Load necessary libraries
library(tidyverse)
library(lubridate) # for as.Date()
library(leaflet)
library(RefManageR) # for bibliography

# Load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# Link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# Read in data
# raw_data <- read.csv("./data/primary_studies/Saunders_2013/original/ST_OMD_Saunders_002")
raw_data <- read_csv("./data/primary_studies/Saunders_2013/original_2024/ST_OMD_Saunders_002.csv")
# unique(raw_data == raw_data2)

## 2. Organize tables ####
depth_raw <- raw_data %>% 
  rename(site_id = SITENAME,
         core_id = Core_ID,
         dry_bulk_density = BulkDensity,
         excess_pb210_activity = Excess_Pb,
         cs137_activity = `137Cs`,
         habitat = Habitat,
         age = Year_210Pb_CF) %>% 
  separate(SoilDepth_Interval, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(dates = as.Date(Date),
         year = year(dates),
         month = month(dates),
         day = day(dates),
         method_id = "single set of methods",
         study_id = "Saunders_2013",
         site_id = gsub("-", "_", site_id),
         core_id = paste(site_id, core_id, sep = "_"),
         habitat = case_when(habitat == "sawgrass" ~ "marsh",
                             habitat == "slough" ~ "swamp", T ~ NA),
         age = ifelse(grepl("-", age), "NA", age),
         cs137_activity = ifelse(grepl("-", cs137_activity), "NA", cs137_activity),
         cs137_unit = "picocuriesPerGram",
         pb210_unit = "disintegrationsPerMinutePerGram") 

depthseries <- depth_raw %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, 
         cs137_activity, cs137_unit, excess_pb210_activity, pb210_unit, age)
         
cores <- depth_raw %>% 
  mutate(latitude = as.numeric(ifelse(site_id == "NE_SRS1", 25.698, 
                           ifelse(site_id == "SRS3", 25.468,
                                  ifelse(site_id == "SRS4", 25.410, "NA")))),
         longitude = as.numeric(ifelse(site_id == "NE_SRS1", -80.635, 
                            ifelse(site_id == "SRS3", -80.853,
                                   ifelse(site_id == "SRS4", -80.964, "NA")))),
         core_length_flag = "core depth limited by length of corer",
         position_method = "other low resolution",
         position_notes = "position listed at site level",
         vegetation_method = "field observation",
         vegetation_class = case_when(habitat == "marsh" ~ "emergent", 
                                      habitat == "swamp" ~ "forested to emergent")) %>%
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, position_method,
         position_notes, vegetation_class, vegetation_method, habitat, core_length_flag) %>% 
  distinct()

# methods aren't really provided?
# methods <- data.frame(study_id = "Saunders_2013",
#                       method_id = "single set of methods",
#                       coring_method = "")


## 3. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries")

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
# testNumericCols(depthseries)


## 4. Bibliography ####
bib <- as.data.frame(RefManageR::GetBibEntryWithDOI("10.6073/pasta/0a012d9bffa94911109aad7b8447145c"))

study_citation <- bib %>% 
  mutate(study_id = "Saunders_2013",
          bibliography_id = "Saunders_2013_data",
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, everything())

# study_citation <- data.frame(study_id = "Saunders_2013",
#                              bibliography_id = "Saunders_2013_data",
#                              title = "Radiometric Characteristics of Soil Sediments from Shark River Slough, Everglades National Park (FCE) from 2005 and 2006",
#                              author = "Saunders, Colin",
#                              publication_type = "primary dataset",
#                              doi = "10.6073/pasta/0a012d9bffa94911109aad7b8447145c",
#                              url = "https://doi.org/10.6073/pasta/0a012d9bffa94911109aad7b8447145c",
#                              bibtype = "Misc",
#                              year = "2013")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "data/primary_studies/Saunders_2013/derivative/Saunders_2013_cores.csv") 
write_csv(depthseries, "data/primary_studies/Saunders_2013/derivative/Saunders_2013_depthseries.csv")
write_csv(methods, "data/primary_studies/Saunders_2013/derivative/Saunders_2013_methods.csv")
# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Saunders_2013/derivative/Saunders_2013_study_citations.bib")
write_csv(study_citation, "data/primary_studies/Saunders_2013/derivative/Saunders_2013_study_citations.csv")




