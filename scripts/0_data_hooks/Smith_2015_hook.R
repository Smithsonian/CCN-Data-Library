## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

# This is a hook script for the Wetland Paleoecological Study of Southwest Coastal Louisiana: Sediment Cores and Diatom Calibration Dataset

## 1. Citations ###########

# Data Citation
# Smith, K.E.L., Flocks, J.G., Steyer, G.D., and Piazza, S.C., 2015, 
# Wetland paleoecological study of southwest coastal Louisiana—Sediment cores and diatom calibration dataset: 
# U.S. Geological Survey Data Series 877, https://dx.doi.org/10.3133/ds877.

# Publication Citation (PhD Thesis)
# Smith, Kathryn E.L., 2012, 
# Paleoecological study of coastal marsh in the Chenier Plain, Louisiana: Investigating the diatom composition of hurricane-deposited 
# sediments and a diatom-based quantitative reconstruction of sea-level characteristics. 
# PhD Thesis. University of Florida


## 2. Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(tidyverse)
library(lubridate)

## 3. Data Location ########

# I'm unable to scrape files from USGS. Until I figure it out, I'll download and place in original folder. 

URL <- "https://pubs.usgs.gov/ds/0877/html/ds877_data.html"

## 4. Import data ####################

raw_depthseries <- read.csv("./data/Smith_2015/original/sediment_core_properties.txt")
raw_cores <- read.csv("./data/Smith_2015/original/sediment_core_sites.txt")

## ... 4A. Core-level data ###########
# Although the original file says sites, it is the core-level data. 

core_data <- raw_cores %>%
  rename(core_id = Site, 
         core_latitude = Latitude, 
         core_longitude = Longitude,
         core_date = Date) %>%
  mutate(core_date = as.Date(core_date, format = "%m/%d/%Y")) %>%
  mutate(core_position_method = "handheld", 
         study_id = "Smith_et_al_2015", site_id = "09WCC01") %>%
  select(core_id, site_id, study_id, 
         core_date, core_latitude, core_longitude, core_position_method)

## ... 4B. Depthseries data ##########

depthseries_data <- raw_depthseries %>%
  rename(core_id = Sample, 
         dry_bulk_density = BD,
         fraction_organic_matter = LOI,
         total_pb210_activity = Pb210, total_pb210_activity_sd = Pb_error,
         ra226_activity = Ra226, ra226_activity_sd = Ra_error,
         cs137_activity = Cs137, cs137_activity_sd = Cs_error) %>%
  # create unit code for dating methods
  mutate(pb210_unit = ifelse(is.na(total_pb210_activity) == FALSE, "disintegrations_per_minute_per_gram", NA),
         cs137_unit = ifelse(is.na(cs137_activity) == FALSE, "disintegrations_per_minute_per_gram", NA)) %>%
  # convert LOI to a fraction
  mutate(fraction_organic_matter = fraction_organic_matter / 100) %>%
  separate(core_id, into=c("core_id", "depth_interval"), sep=10) %>%
  # establish min and max depths for each sample
  mutate(depth_interval = as.numeric(gsub("_", "", depth_interval))) %>%
  mutate(depth_min = depth_interval - 1, depth_max = depth_interval + 1,
         study_id = "Smith_et_al_2015", site_id = "09WCC01") %>%
  select(study_id, core_id, depth_min, depth_max,
         dry_bulk_density,
         fraction_organic_matter,
         cs137_activity, cs137_activity_sd, cs137_unit,
         total_pb210_activity, total_pb210_activity_sd,
         ra226_activity, ra226_activity_sd, pb210_unit)

## ... 4C. Site-level data ##########
site_data <- core_data %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude)

# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)
# remove NAs before aggregation
site_data <- na.omit(site_data)

# Now aggeregate data to the site level
site_data <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  mutate(site_description = "Rockefeller Wildlife Refuge")

## 5. Create study-citation table ######
# import the CCRCN bibliography 
library(bib2df)
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

# link each study to primary citation and join with synthesis table
studies <- unique(core_data$study_id)

study_data_primary <- CCRCN_bib %>%
  select(BIBTEXKEY, CATEGORY, DOI) %>%
  rename(bibliography_id = BIBTEXKEY,
         study_type = CATEGORY,
         doi = DOI) %>%
  filter(bibliography_id %in% studies) %>%
  mutate(study_id = bibliography_id, 
         study_type = tolower(study_type)) %>%
  select(study_id, study_type, bibliography_id, doi) 

## 6. QA/QC  ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", core_data)
test_colnames("site_level", site_data) 
test_colnames("depthseries", depthseries_data) 

# core_data <- reorder_columns(core_data, "core_level")
site_data <- reorder_columns(site_data, "site_level")

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(core_data, depthseries_data)

## 7. Write data ##################
write_csv(core_data, "./data/Smith_2015/derivative/Smith_et_al_2015_cores.csv")
write_csv(site_data, "./data/Smith_2015/derivative/Smith_et_al_2015_sites.csv")
write_csv(depthseries_data, "./data/Smith_2015/derivative/Smith_et_al_2015_depthseries.csv")
write_csv(study_data_primary, "./data/Smith_2015/derivative/Smith_et_al_2015_study_citations.csv")
