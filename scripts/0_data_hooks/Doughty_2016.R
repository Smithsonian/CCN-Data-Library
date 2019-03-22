## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Citation #########

# Publication citation: 
# Doughty, C.L., Langley, J.A., Walker, W.S. et al. 
# Mangrove Range Expansion Rapidly Increases Coastal Wetland Carbon Storage. Estuaries and Coasts (2016) 39: 385. 
# https://doi.org/10.1007/s12237-015-9993-8

## 2. Prep workspace and read in data ####################

## ... 2A. Load packages #######################
library(tidyverse)
library(lubridate)
library(readxl)

doughty_cores <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_core_level.csv")
doughty_depthseries <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_soil_depth_series.csv")
doughty_species <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_dominant_species.csv")
doughty_pubs <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_associated_publications.csv")
doughty_study_info <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_study_information.csv")
doughty_keyword <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_keywords.csv")
doughty_funding <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_funding_sources.csv")
doughty_authors <- read.csv("./data/Doughty_2016/original/Doughty2016_CCRCN_authors.csv")

## 3. Curate files ########
# The only major tasks to change the format of the study id to "Doughty_et_al_2016" from "Doughty_etal_2016"

cores <- doughty_cores %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

depthseries <- doughty_depthseries %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

species <- doughty_species %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

doughty_study_info <- doughty_study_info %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

doughty_pubs <- doughty_pubs %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

doughty_keyword <- doughty_keyword %>%
  mutate(study_id = gsub("etal", "et_al", study_id))

doughty_funding <- doughty_funding %>%
  mutate(study_id = gsub("etal", "et_al", study_id))
  
doughty_authors <- doughty_authors %>%  
  mutate(study_id = gsub("etal", "et_al", study_id))

## 4. QA/QC #################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("species", species) 
test_colnames("depthseries", depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 5. Write data #################

write_csv(cores, "./data/Doughty_2016/derivative/Doughty_et_al_2016_cores.csv")
write_csv(depthseries, "./data/Doughty_2016/derivative/Doughty_et_al_2016_depthseries.csv")
write_csv(species, "./data/Doughty_2016/derivative/Doughty_et_al_2016_species.csv")
write_csv(doughty_pubs, "./data/Doughty_2016/derivative/Doughty_et_al_2016_associated_publications.csv")
write_csv(doughty_study_info, "./data/Doughty_2016/derivative/Doughty_et_al_2016_study_information.csv")
write_csv(doughty_keyword, "./data/Doughty_2016/derivative/Doughty_et_al_2016_keywords.csv")
write_csv(doughty_funding, "./data/Doughty_2016/derivative/Doughty_et_al_2016_funding_sources.csv")
write_csv(doughty_authors, "./data/Doughty_2016/derivative/Doughty_et_al_2016_authors.csv")
