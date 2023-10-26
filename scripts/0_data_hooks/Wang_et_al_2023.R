## CCN Data Library
# Hook script for Wang et al 2023; contact: wangh@usgs.gov
# URL: https://doi.org/10.5066/P9XQBYXU
# Curation contact: Henry Betts, BettsH@si.edu

## Libraries
library(tidyverse)
library(leaflet)

## Read in the data
date_raw <- read.csv("data/primary_studies/Wang_et_al_2023/original/JB-Cs-Pb-rev.csv", na = "no data")
soil_raw <- read.csv("data/primary_studies/Wang_et_al_2023/original/JB-soil-property-rev.csv", na = "no data")

# Link to database guidance:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html


## Curation ####
depth_raw <- full_join(date_raw, soil_raw, by = c("Site", "Depth")) %>% 
  rename(core_id = Site,
         depth_max = Depth,
         dry_bulk_density = Bulk.Density,
         fraction_organic_matter = LOI,
         fraction_carbon = TOC,
         cs137_activity = Cs.137.Activity,
         cs137_activity_se = Cs.Error,
         excess_pb210_activity = Pbxs.210.Activity,
         excess_pb210_activity_se = Pbxs.Error) %>% 
  mutate(study_id = "Wang_et_al_2023",
         core_id = gsub(" ", "_", core_id),
         site_id = core_id,
         method_id = "single set of methods",
         depth_min = depth_max - 2,
         fraction_carbon = gsub("no data", NA, fraction_carbon),
         fraction_carbon = as.numeric(fraction_carbon)/100,
         fraction_organic_matter = as.numeric(fraction_organic_matter)/100,
         cs137_unit = "dpm/g",
         pb210_unit = "dpm/g",
         habitat = "marsh")

depthseries <- depth_raw %>% 
  select(c(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, 
           fraction_organic_matter, fraction_carbon, cs137_activity, cs137_activity_se,
           cs137_unit, excess_pb210_activity, excess_pb210_activity_se, pb210_unit))

cores <- depth_raw %>% 
  mutate(position_method = "other low resolution",
         position_notes = "site bounding box in site table",
         salinity_class = "estuarine",
         salinity_method = "field observation") %>% 
  select(c(study_id, site_id, core_id, position_method, position_notes, salinity_class, salinity_method,
           habitat)) %>% 
  distinct()
  
sites <- depth_raw %>% 
  mutate(site_latitude_max = 40.6890,
         site_latitude_min = 40.5227,
         site_longitude_max = -73.9819,
         site_longitude_min = -73.7128,
         site_description = "Jamaica Bay estuary") %>% 
  select(c(study_id, site_id, site_description, site_latitude_max, site_latitude_min, 
           site_longitude_max, site_longitude_min)) %>% 
  distinct()
  
methods <- data.frame(study_id = "Wang_et_al_2023",
                      method_id = "single set of methods",
                      coring_method = "push core",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_sample_volume = 39.23,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 2,
                      loss_on_ignition_sample_volume = 39.23,
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "acid fumigation",
                      fraction_carbon_type = "organic carbon",
                      cs137_counting_method = "gamma",
                      pb210_counting_method = "gamma",
                      excess_pb210_model = "CRS and CFCS")


## QAQC ####

# ## Mapping
# leaflet(cores) %>%
#   addTiles() %>% 
#   addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "sites", "methods")

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
study_citation <- data.frame(study_id = "Wang_et_al_2023",
                             bibliography_id = "Wang_et_al_2023_data",
                             title = "Spatial Variability in Vertical Accretion and Carbon Sequestration in Salt Marsh Soils of an Urban Estuary",
                             author = "Wang, H., Snedden, G., Hartig, E., Chen, Q.",
                             publication_type = "primary dataset",
                             doi = "10.1007/s13157-023-01699-y",
                             url = "https://doi.org/10.1007/s13157-023-01699-y",
                             bibtype = "Misc",
                             year = "2023",
                             month = "may",
                             day = "17")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_cores.csv") 
write_csv(depthseries, "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_depthseries.csv")
write_csv(sites, "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_sites.csv")
write_csv(methods, "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_methods.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_study_citations.bib")
write_csv(study_citation, "data/primary_studies/Wang_et_al_2023/derivative/Wang_et_al_2023_study_citations.csv")


                      



  