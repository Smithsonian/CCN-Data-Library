## CCN Data Library ####

# Curation script for Vinent and Kirwan 2017 data curation
# Contact: Henry Betts, BettsH@si.edu
# URL: https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.264
# also here: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-vcr.264.2

## Set up workspace
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)


source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


## Read in data ####
soil_raw <- read_csv("./data/primary_studies/Vinent_and_Kirwan_2017/original/PhilCrk_soil_data_summary2a.csv", skip = 21) 

## Curate ####
depth_raw <- soil_raw %>% 
  mutate(study_id = "Vinent_and_kirwan_2017",
         site_id = "Phillips_Creek_Marsh",
         core_id = paste(transect, point, replicate, sep = "_"),
         depth_min = 0,
         depth_max = 10,
         method_id = "single set of methods",
         year = 2017,
         month = 4,
         day = 7,
         elevation_method = "LiDAR",
         core_length_flag = "core depth limited by length of corer") %>% 
  rename(elevation = elevation_msl,
         fraction_organic_matter = organic_frac,
         dry_bulk_density = bulk_density)

depthseries <- depth_raw %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

cores <- depth_raw %>% 
  mutate(habitat = "marsh") %>% 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, elevation, elevation_method, habitat, core_length_flag)

methods <- data.frame(study_id = "Vinent_and_kirwan_2017",
                      method_id = "single set of methods",
                      coring_method = "pvc and hammer",
                      loss_on_ignition_temperature = 550)

study_citations <- data.frame(study_id = "Vinent_and_kirwan_2017", 
                              bibliography_id = "Vinent_and_kirwan_2017_data",
                              publication_type = "primary dataset", 
                              bibtype = "Misc", 
                              title = "Upper Phillips Creek soil organic content and bulk density April, 2017",
                              author = "Vinent, O.D. and ML. Kirwan",
                              doi = "10.6073/pasta/0f1cceb5f013643be08dbc5386f073ac", 
                              url = "https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.264", 
                              year = 2017,
                              month = "apr", 
                              publisher = "Virginia Coast Reserve Long-Term Ecological Research Project Data Publication")

bib_file <- study_citations %>%
  remove_rownames() %>%
  select(-c(study_id)) %>%
  column_to_rownames("bibliography_id")


## QAQC ####

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
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## Write files ####
write_csv(cores, "./data/primary_studies/Vinent_and_Kirwan_2017/derivative/Vinent_and_Kirwan_2017_cores.csv")
write_csv(depthseries, "./data/primary_studies/Vinent_and_Kirwan_2017/derivative/Vinent_and_Kirwan_2017_depthseries.csv")
write_csv(methods, "./data/primary_studies/Vinent_and_Kirwan_2017/derivative/Vinent_and_Kirwan_2017_methods.csv")
write_csv(study_citations, "./data/primary_studies/Vinent_and_Kirwan_2017/derivative/Vinent_and_Kirwan_2017_study_citations.csv")
WriteBib(as.BibEntry(study_citations), "./data/primary_studies/Vinent_and_Kirwan_2017/derivative/Vinent_and_Kirwan_2017_study_citations.bib")









