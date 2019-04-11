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

raw_depthseries <- read_csv("./data/Trettin_2017/original/Zambezi_Soils.csv")

## 3. Curate data #############

## ... Depth series data ###################
# Issues: 
# 1. Depthseries in this data does not reflect the entire core interval. 
# About 4 to 6 five cm intervals were extracted from 200 cm cores and tested

depthseries <- raw_depthseries %>%
  mutate(site_id = as.character(Plot)) %>%
  mutate(study_id = study) %>%
  # Paste plot and subplot values to create a unique core ID 
  mutate(core_id = as.factor(paste(Plot, Subplot, sep="_"))) %>%
  # One sample interval has double _ _ 
  # Rename variables
  rename(dry_bulk_density = 'Bulk Density (g cm-3)') %>%
  rename(percent_carbon = '%C') %>%
  mutate(fraction_carbon = percent_carbon / 100) %>%
  separate(col='Actual Sample Interval', into=c("depth_min", "depth_max"), sep="_") %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon) %>%
  mutate(depth_min = ifelse(is.na(depth_max==TRUE),100,depth_min)) %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  arrange(study_id, site_id, core_id, depth_min)

## ... Core-level ###########
# We'll scale up to the core-level from the depthseries
coreLocations <- read_csv("./data/Trettin_2017/original/Zambezi_PlotLocations.csv")
coreLocations <- coreLocations %>%
  mutate(site_id = as.character(Plot)) %>%
  rename(species_code = 'Dominant Species')

species <- coreLocations %>% 
  mutate(species_code = strsplit(as.character(species_code), "; ")) %>% 
  unnest(species_code) %>%
  mutate(study_id = study) %>%
  select(study_id, site_id, species_code) %>%
  arrange(study_id, site_id, species_code)

print(unique(species$species_code))

species <- species %>%
  mutate(species_code = recode_factor(species_code, "H. Littoralis" = "Heritiera littoralis",
                "B. Gymnorrhiza" = "Bruguiera gymnorrhiza",
                "R. Mucronata" = "Rhizophora mucronata",
                "X. Granatum" = "Xylocarpus granatum",
                "A. Marina" = "Avicennia marina",
                "C. Tagal" = "Ceriops tagal",
                "S. Alba" = "Sonneratia alba"))

cores <- depthseries %>%
  select(study_id, site_id, core_id) %>%
  distinct() %>%
  left_join(coreLocations) %>%
  rename(core_latitude = Lati, core_longitude = Long) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude)

## ... Site-level ##########
sites <- cores %>%
  group_by(study_id, site_id) %>% 
  summarize(site_longitude_max = max(core_longitude),
            site_longitude_min = min(core_longitude),
            site_latitude_max = max(core_latitude),
            site_latitude_min = min(core_latitude))

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
test_colnames("core_level", cores)
test_colnames("site_level", sites) 
test_colnames("depthseries", depthseries)
test_colnames("species", species)


# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 6. Write data ##############
write_csv(sites, "./data/Trettin_2017/derivative/Trettin_et_al_2017_sites.csv")
write_csv(cores, "./data/Trettin_2017/derivative/Trettin_et_al_2017_cores.csv")
write_csv(depthseries, "./data/Trettin_2017/derivative/Trettin_et_al_2017_depthseries.csv")
write_csv(study_data_primary, "./data/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")
write_csv(species, "./data/Trettin_2017/derivative/Trettin_et_al_2017_species.csv")

