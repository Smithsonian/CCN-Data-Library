## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data releases
# FL: https://www.sciencebase.gov/catalog/item/60bfb8a4d34e86b938916d6f
# RI: https://www.sciencebase.gov/catalog/item/60bfb7c2d34e86b938916d1e
# MA: https://www.sciencebase.gov/catalog/item/60bfb987d34e86b938916dc9
# MA: https://www.sciencebase.gov/catalog/item/60bfb916d34e86b938916da1
# Puerto Rico: https://www.sciencebase.gov/catalog/item/60902e3fd34e93746a710491

# load necessary libraries
library(tidyverse)
# library(readxl)
library(lubridate)
# library(rgdal)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_PR_age <- read_csv("data/primary_studies/Eagle_et_al_2021/original/Data_PR_AgeModel.csv")
raw_PR <- read_csv("data/primary_studies/Eagle_et_al_2021/original/Data_PR_Cores.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Eagle_et_al_2021"

## ... Methods ####

# curate materials and methods
# methods <- raw_methods %>% 
#   select_if(function(x) {!all(is.na(x))})

## ... Core Depthseries ####

# suttle_ds <- bind_rows(raw_FL, raw_MA1, raw_MA2, raw_RI) %>%
#   mutate(year = year(as.Date(Date, format = "%m/%d/%Y")),
#          month = month(as.Date(Date, format = "%m/%d/%Y")),
#          day = day(as.Date(Date, format = "%m/%d/%Y"))) %>% 
#   select(-Date)

eagle_ds <- full_join(raw_PR, raw_PR_age) %>%
  mutate(year = year(as.Date(Date, format = "%m/%d/%y")),
         month = month(as.Date(Date, format = "%m/%d/%y")),
         day = day(as.Date(Date, format = "%m/%d/%y"))) %>% 
  rename(longitude = Lon, 
         latitude = Lat,
         site_id = Site, 
         core_id = ID,
         depth_min = Depth_min,
         depth_max = Depth_max,
         dry_bulk_density = DBD,
         delta_c13 = `13C`,
         total_pb210_activity = `210Pb`, 
         total_pb210_activity_se = `210Pb_e`,
         excess_pb210_activity = `210Pbex`, 
         excess_pb210_activity_se = `210Pbex_e`,
         cs137_activity = `137Cs`,
         cs137_activity_se = `137Cs_e`,
         ra226_activity = `226Ra`,
         ra226_activity_se = `226Ra_e`,
         be7_activity = `7Be`,
         be7_activity_se = `7Be_e`,
         age = Year_50,
         age_min = Year_LL,
         age_max = Year_UL) %>% 
  mutate(study_id = id,
         fraction_carbon = wtC/100)

depthseries <- eagle_ds %>% 
  select(-c(year, month, day, latitude, longitude, wtC, wtN, `15N`,
            Date, Replicate, Depth_mid, contains("_UL"), contains("_LL"),
            contains("_50")))

depthseries <- reorderColumns("depthseries", depthseries)

## ... Core-Level ####

cores <- eagle_ds %>%
  distinct(study_id, site_id, core_id, year, month, day, latitude, longitude) %>% 
  mutate(position_accuracy = 5,
         position_method = "handheld GPS (cellphone)")


# cores <- reorderColumns('cores', cores)

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 3, label = ~site_id)


## 2. QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

table_names <- c("methods", "cores", "depthseries")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

# consider this a synthesis? 

# if(!file.exists("data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_study_citations.csv")){
# Create bibtex file
dois <- c("10.5066/P97CAF30", "10.3389/ffgc.2021.676691")

data_bibs <- GetBibEntryWithDOI(dois)

study_citations <- as.data.frame(data_bibs) %>%
  mutate(study_id = id,
         bibliography_id = "Eagle_et_al_2021_data",
         publication_type = "primary dataset") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021.bib")
# write_csv(study_citations, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
# write_csv(cores, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_cores.csv") 
# write_csv(depthseries, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_depthseries.csv")
# write_csv(methods, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_methods.csv")
# write_csv(sites, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_sites.csv")
# write_csv(species, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_species.csv")
# write_csv(impacts, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_impacts.csv")


