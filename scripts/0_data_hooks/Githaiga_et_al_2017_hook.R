## CCN Data Library ####

## Soil core data curation script for Githaiga et al 2017
## contact: Henry Betts, BettsH@si.edu
## URL: https://doi.org/10.1371/journal.pone.0177001


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

  
## 2. Organize depthseries table ####

# T. hemprichii
depth_raw_th <- read_xlsx("./data/primary_studies/Githaiga_et_al_2017/original/S1 File.xlsx", sheet = 1, skip = 1)
depth_th <- depth_raw_th %>% 
  separate(Depth, into = c('depth_min', 'depth_max'), sep = '-') %>% 
  rename(core_id = "Core",
         dry_bulk_density = B.density,
         fraction_organic_matter = `% Organic matter`) %>% 
  mutate(core_id = gsub(" ", "", core_id),
         core_id = ifelse(Treatment == "Non-vegetated" & core_id == "Q1", "THQ1_2",
                          ifelse(Treatment == "Non-vegetated" & core_id == "Q2", "THQ2_2",
                                 ifelse(Treatment == "Non-vegetated" & core_id == "Q3", "THQ3_2",
                                        ifelse(Treatment == "Non-vegetated" & core_id == "Q4", "THQ4_2",
                                               ifelse(Treatment == "Non-vegetated" & core_id == "Q5", "THQ5_2", core_id)))))) %>%
  fill(core_id) %>% 
  mutate(fraction_organic_matter = fraction_organic_matter/100,
         study_id = "Githaiga_et_al_2017") %>% 
    select(study_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

# E. acoroides
depth_raw_ea <- read_xlsx("./data/primary_studies/Githaiga_et_al_2017/original/S1 File.xlsx", sheet = 2, skip = 1)
depth_ea <- depth_raw_ea %>% 
  separate(Depth, into = c('depth_min', 'depth_max'), sep = '-') %>% 
  rename(core_id = "Core",
         dry_bulk_density = "B. density",
         fraction_organic_matter = `% Organic matter`) %>% 
  mutate(core_id = gsub(" ", "", core_id),
         core_id = ifelse(Treatment == "Non-vegetated" & core_id == "Q1", "Q1_2",
                          ifelse(Treatment == "Non-vegetated" & core_id == "Q2", "Q2_2",
                                 ifelse(Treatment == "Non-vegetated" & core_id == "Q3", "Q3_2",
                                        ifelse(Treatment == "Non-vegetated" & core_id == "Q4", "Q4_2",
                                               ifelse(Treatment == "Non-vegetated" & core_id == "Q5", "Q5_2", core_id)))))) %>%
  fill(core_id) %>% 
  mutate(core_id = paste("EA", core_id, sep = ""),
         fraction_organic_matter = fraction_organic_matter/100,
         study_id = "Githaiga_et_al_2017") %>% 
  select(study_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

# T. ciliatum
depth_raw_tc <- read_xlsx("./data/primary_studies/Githaiga_et_al_2017/original/S1 File.xlsx", sheet = 3)
depth_tc <- depth_raw_tc %>% 
  separate(Depth, into = c('depth_min', 'depth_max'), sep = '-') %>% 
  rename(core_id = "Core",
         dry_bulk_density = "B. density",
         fraction_organic_matter = `% Organic matter`,
         Treatment = T.ciliatum) %>% 
  mutate(core_id = gsub(" ", "", core_id),
         core_id = ifelse(Treatment == "Non-vegetated" & core_id == "Q1", "Q1_2",
                          ifelse(Treatment == "Non-vegetated" & core_id == "Q2", "Q2_2",
                                 ifelse(Treatment == "Non-vegetated" & core_id == "Q3", "Q3_2",
                                        ifelse(Treatment == "Non-vegetated" & core_id == "Q4", "Q4_2",
                                               ifelse(Treatment == "Non-vegetated" & core_id == "Q5", "Q5_2", core_id)))))) %>%
  fill(core_id) %>% 
  mutate(core_id = paste("TC", core_id, sep = ""),
         fraction_organic_matter = fraction_organic_matter/100,
         study_id = "Githaiga_et_al_2017") %>% 
  select(study_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

# S. isoetifolium
depth_raw_si <- read_xlsx("./data/primary_studies/Githaiga_et_al_2017/original/S1 File.xlsx", sheet = 4, skip = 1)
depth_si <- depth_raw_si %>% 
  separate(Depth, into = c('depth_min', 'depth_max'), sep = '-') %>% 
  rename(core_id = "Core",
         dry_bulk_density = Bd,
         fraction_organic_matter = `% Org matter`,
         Treatment = treat) %>% 
  mutate(core_id = gsub(" ", "", core_id),
         core_id = ifelse(Treatment == "nonveg" & core_id == "Q1", "Q1_2",
                          ifelse(Treatment == "nonveg" & core_id == "Q2", "Q2_2",
                                 ifelse(Treatment == "nonveg" & core_id == "Q3", "Q3_2",
                                        ifelse(Treatment == "nonveg" & core_id == "Q4", "Q4_2",
                                               ifelse(Treatment == "nonveg" & core_id == "Q5", "Q5_2", core_id)))))) %>%
  fill(core_id) %>% 
  mutate(core_id = paste("SI", core_id, sep = ""),
         fraction_organic_matter = fraction_organic_matter/100,
         study_id = "Githaiga_et_al_2017") %>% 
  select(study_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

## 3. Finalize tables ####
depthseries <- rbind(depth_th, depth_ea, depth_tc, depth_si) %>% 
  mutate(site_id = core_id)

cores <- depthseries %>% 
  mutate(vegetation_class = "seagrass",
         vegetation_method = "measurement",
         core_length_flag = "core depth limited by length of corer",
         salinity_class = "mixoeuhaline",
         salinity_method = "field observation",
         habitat = ifelse(grepl("_2", core_id), "unvegetated", "seagrass"),
         year = 2017,
         month = 02,
         day = 17) %>% 
  select(study_id, site_id, core_id, year, month, day, salinity_class, salinity_method, vegetation_class, 
         vegetation_method, habitat, core_length_flag) %>% 
  distinct()

# site table? Gazi Bay (4°25’S, and 39°30’E)

species <- cores %>% 
  mutate(species_code = ifelse(grepl("TH", core_id), "Thalassia hemprichii",
                               ifelse(grepl("EA", core_id), "Enhalus acoroides",
                                      ifelse(grepl("TC", core_id), "Thalassodendron ciliatum",
                                             ifelse(grepl("SI", core_id), "Syringodium isoetifolium", core_id)))),
         code_type = "Genus species") %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)

methods <- data.frame(study_id = "Githaiga_et_al_2017",
                      coring_method = "russian corer",
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = .5,
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_time = 72,
                      dry_bulk_density_sample_volume = 98.125,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_temperature = 450,
                      loss_on_ignition_time = 6,
                      loss_on_ignition_flag = "not specified",
                      carbon_measured_or_modeled = "modeled",
                      carbonates_removed = FALSE,
                      carbonate_removal_method = "none specified",
                      fraction_carbon_method = "local regression",
                      fraction_carbon_type = "organic carbon",
                      carbon_profile_notes = "%LOI < 0.2: %Corg = 0.40 x %LOI - 0.21; %LOI > 0.2: %Corg = 0.43 x %LOI - 0.33")


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
testNumericCols(depthseries)


## 4. Bibliography ####
study_citation <- data.frame(study_id = "Githaiga_et_al_2017",
                             bibliography_id = "Githaiga_et_al_2017_data",
                             title = "Carbon storage in the seagrass meadows of Gazi Bay, Kenya",
                             author = "Githaiga, Michael N.; Kairo, James G.; Gilpin, Linda; Huxham, Mark",
                             publication_type = "primary dataset",
                             doi = "10.1371/journal.pone.0177001",
                             url = "https://doi.org/10.1371/journal.pone.0177001",
                             bibtype = "Misc",
                             year = "2017",
                             month = "may",
                             day = "10")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_cores.csv") 
write_csv(depthseries, "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_depthseries.csv")
write_csv(species, "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_species.csv")
write_csv(methods, "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_methods.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_study_citations.bib")
write_csv(study_citation, "./data/primary_studies/Githaiga_et_al_2017/derivative/Githaiga_et_al_2017_study_citations.csv")
# site table?
                      
                      
                      
                      

