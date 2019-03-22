## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Data and publication citations #########
# Data citation: 
# Trettin, Carl C.; Stringer, Christina E.; Zarnoch, Stanley J.; Tang, Wenwu; Dai, Zhaohua. 2017. 
# Mangrove carbon stocks in Zambezi River Delta, Mozambique. Fort Collins, CO: Forest Service Research Data Archive. 
# https://doi.org/10.2737/RDS-2017-0053

study <- "Trettin_et_al_2017"

## 2. Prep workspace and read in data ####################
# Load RCurl, a package used to download files from a URL

library(tidyverse)
library(readxl)

raw_depthseries <- read.csv("./data/Trettin_2017/original/Zambezi_Soils.csv")

## 3. Curate data #############

## ... Depth series data ###################
# Issues: 
# 1. Depthseries in this data does not reflect the entire core interval. 
# About 4 to 6 five cm intervals were extracted from 200 cm cores and tested

depthseries <- raw_depthseries %>%
  mutate(site_id = "Zambezi_River_Delta") %>%
  mutate(study_id = study) %>%
  # Paste plot and subplot values to create a unique core ID 
  mutate(core_id = as.factor(paste(Plot, Subplot, sep="_"))) %>%
  # One sample interval has double _ _ 
  mutate(Actual.Sample.Interval = gsub("__", "_", Actual.Sample.Interval)) %>%
  # Rename variables
  rename(dry_bulk_density = "Bulk.Density..g.cm.3.") %>%
  rename(percent_carbon = "X.C",
         percent_nitrogen = "X.N") %>%
  mutate(fraction_carbon = percent_carbon / 100,
         fraction_nitrogen = percent_nitrogen / 100) %>%
  separate(col="Actual.Sample.Interval", into=c("depth_min", "depth_max"), sep="_") %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon, fraction_nitrogen) %>%
  mutate(depth_min = ifelse(is.na(depth_max==TRUE),100,depth_min)) %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) 

## ... Core-level ###########
# We'll scale up to the core-level from the depthseries

cores <- depthseries %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id), study_id = first(study_id))

## ... Site-level ##########
sites <- cores %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = -18.89591,
            site_longitude_min = -18.80846,
            site_latitude_max = 36.30681,
            site_latitude_min = 36.11881)

# West_Bounding_Coordinate: 36.30681
# East_Bounding_Coordinate: 36.11881
# North_Bounding_Coordinate: -18.89591
# South_Bounding_Coordinate: -18.80846

## 4. Create study-citation table ######
# import the CCRCN bibliography 
library(bib2df)
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

study_data_primary <- CCRCN_bib %>%
  select(BIBTEXKEY, CATEGORY, DOI) %>%
  rename(bibliography_id = BIBTEXKEY,
         study_type = CATEGORY,
         doi = DOI) %>%
  filter(bibliography_id %in% study) %>%
  mutate(study_id = bibliography_id, 
         study_type = tolower(study_type)) %>%
  select(study_id, study_type, bibliography_id, doi) 

## 5. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("sites", sites) 
test_colnames("depthseries", depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 6. Write data ##############
write.csv(sites, "./data/Trettin_2017/derivative/Trettin_et_al_2017_sites.csv")
write.csv(cores, "./data/Trettin_2017/derivative/Trettin_et_al_2017_cores.csv")
write.csv(depthseries, "./data/Trettin_2017/derivative/Trettin_et_al_2017_depthseries.csv")
write.csv(study_data_primary, "./data/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")
