## CCN Data Library ####

## Soil core data curation script for Turck 2014
## contact: Henry Betts, BettsH@si.edu
## URL: https://doi.org/10.6073/pasta/4541ae084d807962b8c331eea61908bd


## 1. Set up ####

# Load necessary libraries
library(tidyverse)
library(leaflet)
library(RefManageR)
library(sf) # for UTM_to_DD()


# Load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# Link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# Read in data
cores_raw <- read.csv("./data/primary_studies/Turck_2014/original/GEL-GCES-1402_sampling_1_0.csv", skip = 2)
depth_raw <- read.csv("./data/primary_studies/Turck_2014/original/GEL-GCES-1402_sediment_analysis_1_1.csv", skip = 2)


## 2. Organize tables ####
cores <- cores_raw[ , -1] %>% 
  filter(grepl("MT", Name)) %>% 
  rename(core_id = Name,
         easting = East_NAD83,
         northing = Nrth_NAD83,
         species_code = Vegetation,
         elevation = Elv_NAVD88) %>% 
  mutate(study_id = "Turck_2014",
         site_id = core_id,
         elevation = as.numeric(elevation)/100,
         position_method = "RTK",
         position_accuracy = .1,
         elevation_method = "RTK",
         elevation_datum = "NAVD88",
         elevation_accuracy = .2,
         zone = 17,
         vegetation_class = "seagrass",
         vegetation_method = "field observation",
         habitat = "marsh",
         core_length_flag = "core depth represents deposit depth",
         year = 2009) %>% 
  UTM_to_DD() %>% 
  select(study_id, site_id, core_id, year, latitude, longitude, position_accuracy, 
         position_method, elevation, elevation_datum, elevation_accuracy, elevation_method, 
         vegetation_class, vegetation_method, habitat, core_length_flag)

species <- cores_raw %>% 
  rename(species_code = Vegetation,
         core_id = Name) %>% 
  mutate(study_id = "Turck_2014",
         site_id = core_id,
         species_code = gsub("Short |Medium ", "", species_code),
         code_type = "Genus",
         habitat = "marsh") %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)

depthseries <- depth_raw %>% 
  filter(grepl("MT", Name)) %>% 
  rename(core_id = Name,
         depth_min = Distance_Below_Ground_Surface,
         fraction_organic_matter = Percent_Organic_Matter,
         fraction_carbon = Total_Carbon) %>% 
  mutate(study_id = "Turck_2014",
         depth_max = as.numeric(depth_min) + 10,
         site_id = core_id,
         method_id = "single set of methods",
         sample_id = ifelse(grepl("extra", Distance_Above_MLLW), paste(core_id, "_2", sep = ""), core_id),
         fraction_organic_matter = as.numeric(fraction_organic_matter)/100,
         fraction_carbon = as.numeric(fraction_carbon)/100,
         compaction_notes = "Compaction measured at the entire-core level") %>% 
  left_join(cores_raw[ -c(1,2), ] %>% 
              mutate(compaction_fraction = as.numeric(Compaction)/as.numeric(Sampl_Lgth)), 
            by = c(core_id = "Name")) %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, sample_id, fraction_organic_matter, fraction_carbon,
         compaction_fraction, compaction_notes)

methods <- data.frame(study_id = "Turck_2014",
                      method_id = "single set of methods",
                      coring_method = "vibracore",
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment not sieved", 
                      sediment_sieve_size = .063,
                      compaction_flag = "compaction quantified",
                      loss_on_ignition_temperature = 1350,
                      loss_on_ignition_flag = "not specified",
                      carbon_measured_or_modeled = "measured", 
                      carbonates_removed = FALSE,
                      carbonate_removal_method = "carbonates not removed",
                      fraction_carbon_method = "not specified", 
                      fraction_carbon_type = "total carbon")


## 3. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods", "species")

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
study_citation <- data.frame(study_id = "Turck_2014",
                             bibliography_id = "Turck_2014_data",
                             title = "Vibracore and Tree Stump Data from the Marsh Near Mary Hammock, McIntosh County, GA",
                             author = "Turck, John A",
                             publication_type = "primary dataset",
                             doi = "10.6073/pasta/4541ae084d807962b8c331eea61908bd",
                             url = "https://doi.org/10.6073/pasta/4541ae084d807962b8c331eea61908bd",
                             bibtype = "Misc",
                             year = "2014")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "./data/primary_studies/Turck_2014/final/Turck_2014_cores.csv") 
write_csv(species, "./data/primary_studies/Turck_2014/final/Turck_2014_species.csv") 
write_csv(depthseries, "./data/primary_studies/Turck_2014/final/Turck_2014_depthseries.csv")
write_csv(methods, "./data/primary_studies/Turck_2014/final/Turck_2014_methods.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Turck_2014/final/Turck_2014_study_citations.bib")
write_csv(study_citation, "./data/primary_studies/Turck_2014/final/Turck_2014_study_citations.csv")





