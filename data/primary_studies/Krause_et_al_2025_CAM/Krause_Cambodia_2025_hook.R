## CCN Data Library ########

## Soil core data curation script for Krause et al 2025 Cambodia
## contact: Jaxine Wolfe, wolfejax@si.edu

## Notes about the dataset 
## Link to the data release and associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# read data
orig_sites <- read_csv("data/primary_studies/Krause_et_al_2025_CAM/original/Krause_et_al_2025_sites.csv")

orig_ds <- read_csv("data/primary_studies/Krause_et_al_2025_CAM/original/Krause_et_al_2025_depthseries.csv") %>% 
  mutate(core_id = site_id, site_id = substr(site_id, 1,2)) # make IDs more nested

orig_biomass <- read_csv("data/primary_studies/Krause_et_al_2025_CAM/original/Krause_et_al_2025_biomass_cores.csv") %>% 
  mutate(core_id = site_id, site_id = substr(site_id, 1,2))

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Krause_et_al_2025"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- data.frame(
    study_id = id,
    method_id = "single set of methods",
    coring_method = "piston corer",
    sediment_sieved_flag = "sediment not sieved",
    roots_flag = "roots and rhizomes included",
    dry_bulk_density_temperature = 60,
    dry_bulk_density_flag = "to constant mass",
    loss_on_ignition_temperature = 500,
    loss_on_ignition_time = 5,
    carbon_measured_or_modeled = "measured",
    fraction_carbon_method = "EA",
    fraction_carbon_type = "organic carbon",
    carbonates_removed = TRUE,
    carbonate_removal_method = "total carbon difference after LOI"
  )

## ... Cores ####

# curate core-level data table
# same as site level for this study
# site table has some summary stats too for biomass
cores <- orig_ds %>% 
  select(c(site_id, core_id, year, month, day, Min_sed_depth)) %>% distinct() %>% 
  # bring in site coords (depthseries table coords are wrong..)
  full_join(orig_sites %>% rename(core_id = site_id) %>% select(core_id, site_latitude, site_longitude)) %>% 
  rename(latitude = site_latitude, longitude = site_longitude) %>% 
  mutate(study_id = id,
         habitat = "seagrass", 
         vegetation_class = "seagrass",
         vegetation_method = "measurement",
         position_method = "handheld",
         core_notes = paste("sediment deposit depth measured:", Min_sed_depth)) %>% 
  reorderColumns("cores", .) %>% select(-Min_sed_depth)


## ... Depthseries ####

# curate core depthseries data table
depthseries <- orig_ds %>% 
  rename(dry_bulk_density = DBD,
         depth_min = Depth_min, 
         depth_max = Depth_max,
         delta_c13 = Sed_d13C) %>% 
  mutate(study_id = id,
         method_id = "single set of methods",
         fraction_organic_matter = round(LOI/100, 5), 
         # drop predicted values for OC
         fraction_carbon = case_when(EA_method == "predicted" ~ NA, 
                                    T ~ round(Sed_Corg/100, 5))) %>% 
  select(-c(year, month, day, latitude, longitude, Sed_N, Sed_P, Corg_dens_interval, Corg_dens_accu, Seagrass_species, Vial_ID,
            Vial_weight, Vial_and_sample_weight, Depth_center, Interval_thickness, LOI, EA_method, Min_sed_depth,
            Sed_Ctot, Sed_Corg, Sed_Cinorg, Sample_weight, Subcore_volume)) %>% 
  reorderColumns("depthseries", .)

## ... Species ####

species <- orig_ds %>%  
  distinct(study_id, site_id, core_id, Seagrass_species) %>% 
  rename(species_code = Seagrass_species) %>% 
  mutate(study_id = id,
         code_type = "Genus species")

## ... Plants ####
# plants <- orig_plants %>% 
#   rename(plant_id = tree_id) %>% 
#   separate(species, into = c("genus", "species"), sep = " ") %>% 
#   mutate(plot_id = paste(transect_id, plot_id, sep = "_")) %>% 
#   select(-transect_id, -family)

## ... Plots ####


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species")

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
test_numeric_vars(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_excel_csv(methods, "data/primary_studies/Krause_et_al_2025_CAM/derivative/Krause_et_al_2025_methods.csv")
# write_excel_csv(plants, "data/primary_studies/Krause_et_al_2025/derivative/Krause_et_al_2025_plants.csv")
write_excel_csv(cores, "data/primary_studies/Krause_et_al_2025_CAM/derivative/Krause_et_al_2025_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Krause_et_al_2025_CAM/derivative/Krause_et_al_2025_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Krause_et_al_2025_CAM/derivative/Krause_et_al_2025_species.csv")
# write_excel_csv(impacts, "data/primary_studies/Krause_et_al_2025/derivative/Krause_et_al_2025_impacts.csv")

## 4. Bibliography ####

library(RefManageR)

orig_datapub <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.28507154")) %>%
  mutate(study_id = "Krause_et_al_2025",
         bibliography_id = "Krause_et_al_2025_data",
         publication_type = "primary dataset") %>%
  select(-keywords)

paperbib <- as.data.frame(GetBibEntryWithDOI("10.1016/j.scitotenv.2025.180000")) %>%
  mutate(study_id = "Krause_et_al_2025",
         bibliography_id = "Krause_et_al_2025_article",
         publication_type = "associated source")

study_citations <-  bind_rows(paperbib, orig_datapub) %>%
  select(study_id, bibliography_id, everything()) %>%
  remove_rownames()

write_csv(study_citations, "data/primary_studies/Krause_et_al_2025_CAM/derivative/Krause_et_al_2025_study_citations.csv")
