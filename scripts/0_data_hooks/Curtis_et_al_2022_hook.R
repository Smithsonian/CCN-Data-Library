## CCN Data Library ####

## Soil core data curation script for Curtis et al 2022
## contact: Henry Betts, BettsH@si.edu

## DOI: https://doi.org/10.5066/P9QLAL7B
## Associated article: A Summary of Water-Quality and Salt Marsh Monitoring, Humboldt Bay, California
## Article URL: https://pubs.er.usgs.gov/publication/ofr20221076/full

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(sf)
library(leaflet)
library(skimr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html


## Step 1: Setting up the datasets ####

methods_raw <- read_excel("./data/primary_studies/Curtis_et_al_2022/intermediate/curtis_et_al_2022_methods.xlsx", sheet = 2)
cores_raw <- read_csv("./data/primary_studies/Curtis_et_al_2022/original/Summary_of_carbon_storage_in_five_salt_marshes_Humboldt_Bay_CA_2018.csv")
depthseries_raw <- read_csv("./data/primary_studies/Curtis_et_al_2022/original/Sediment_characteristics_of_five_salt_marshes_Humboldt_Bay_CA_2018.csv")

## Depthseries
depthseries <- depthseries_raw %>% 
  separate(col = `Sample ID`, into = "core_id", sep = '-', remove = FALSE) %>% 
  mutate(study_id = "Curtis_et_al_2022",
         site_id = substr(core_id, 1, 3),
         sample_id = gsub("-", "_", `Sample ID`),
         depth_min = `Section Bottom Depth (cm)` - 10,
         method_id = 'single set of methods',
         marker_type = "artificial horizon",
         marker_date = '2013',
         compaction_notes = "little to no compaction because once corer is inserted, the core is rotated 180 degrees, and a sample is collected adjacent to where it was driven into the ground") %>% 
  rename(depth_max = `Section Bottom Depth (cm)`,
         dry_bulk_density = `Dry Bulk Density (g cm-3)`,
         fraction_carbon = `Fraction OC`) %>% 
  select(c(study_id, site_id, core_id, method_id, depth_min, depth_max, sample_id, dry_bulk_density, fraction_carbon, 
           marker_type, marker_date))

## Cores
cores <- cores_raw %>% 
  rename(core_id = `Core ID`,
         easting = `Easting (meters)`,
         northing = `Northing (meters)`) %>% 
  mutate(study_id = 'Curtis_et_al_2022',
         site_id = substr(core_id, 1, 3),
         day = day(as.Date(Date, "%m/%d/%Y")),
         month = month(as.Date(Date, "%m/%d/%Y")),
         year = year(as.Date(Date, "%m/%d/%Y")),
         zone = 10,
         salinity_class = "estuarine",
         salinity_method = "measurement",
         vegetation_class = 'emergent',
         vegetation_method = 'field observation',
         habitat = "marsh",
         inundation_class = ifelse(site_id == "MMF", "low",
                                   ifelse(site_id == "MRF", "high",
                                          ifelse(site_id == "JNF", "high",
                                                 ifelse(site_id == "JSF", "high",
                                                        ifelse(site_id == "WMF", "low",
                                                               ifelse(site_id == "HSF", "low", "")))))),
         inundation_method = "measurement",
         position_method = "RTK",
         elevation_notes = "measurements taken at site level",
         core_length_flag = ifelse(core_id == 'JSF1', "core depth represents deposit depth", "core depth limited by length of corer"),
         core_id = ifelse(core_id == 'JSF1', "JSF2", core_id)) %>%  # core_id JSF1 in this dataset is named JSF2 in depthseries; correcting the name here
  UTM_to_DD() %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_method, elevation_notes, 
           salinity_class, salinity_method, vegetation_class, vegetation_method, habitat, inundation_class, inundation_method, 
           core_length_flag))

## Methods
methods <- methods_raw %>% 
  select(where(notAllNA))

## Impacts
impacts <- cores[, c("study_id", "site_id", "core_id")] %>% 
  mutate(impact_class = "natural")


## Step 2: QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods", "impacts")

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


## Step 3: Writing Curated Data ####
write_csv(cores, "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_cores.csv") 
write_csv(depthseries, "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_depthseries.csv")
write_csv(methods, "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_methods.csv")
write_csv(impacts, "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_impacts.csv")


## Step 4: Bibliography ####
citation_article <- data.frame(study_id = "Curtis_et_al_2022",
                               bibliography_id = "Curtis_et_al_2022_article",
                               title = "A Summary of Water-Quality and Salt Marsh Monitoring, Humboldt Bay, California",
                               author = "Jennifer A. Curtis, Karen M. Thorne, Chase M. Freeman, Kevin J. Buffington, and Judith Z. Drexler",
                               doi = "10.3133/ofr20221076",
                               url = "https://doi.org/10.3133/ofr20221076", 
                               bibtype = "Article",
                               journal = "USGS Publications Warehouse",
                               publication_type = 'associated source',
                               year = "2022",
                               month = "sep",
                               day = '16')

citation_data <- data.frame(study_id = "Curtis_et_al_2022",
                            bibliography_id = "Curtis_et_al_2022_data",
                            title = "Salt marsh monitoring during water years 2013 to 2019, Humboldt Bay, CA – water levels, surface deposition, elevation change, and carbon storage",
                            author = "Curtis, J.A., Thorne, K.M., Freeman C.M., Buffington, K.J., and Drexler, J.Z.",
                            publication_type = "primary dataset",
                            doi = "10.5066/P9QLAL7B",
                            url = 'https://doi.org/10.5066/P9QLAL7B',
                            bibtype = "Misc",
                            year = "2022",
                            month = "jun",
                            day = '23')

study_citations <- bind_rows(citation_article, citation_data)

bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Curtis_et_al_2022/derivative/Curtis_et_al_2022_study_citations.csv")



