# Coastal Carbon Research Coordination Network
# This script curates Drexler_et_al_2009 age depth data extracted from figures
# and joins with carbon stock data from the clearinghouse 
# Contact: lonnemanm@si.edu

# Drexler, J. Z., de Fontaine, C. S., & Brown, T. A. (2009). 
# Peat accretion histories during the past 6,000 years in marshes of the Sacramento–San Joaquin Delta, CA, USA. 
# Estuaries and Coasts, 32(5), 871-892.

## Workspace prep ###################

library(tidyverse)
library(readxl)

## Read in data #####################

# The original data from the table is stored in the Drexler_et_al_2009_peat_accretion_age_depth.csv file. 
# Because the site-core-sample ID field could not easily be separated using tidy logic, I did it manually in excel
# Additionally, the negative sign character used in the elevation field was causing problems in both excel and R and was replaced with (-)
# That edited file is read in 
# Different computers are having trouble parsing the column names with spaces, leading to errors during cutation. 
# Renaming columns in the read_excel call and skipping the first line which represents the old column names
age_depth_data <- read_excel("./data/Drexler_2009/original/Drexler_et_al_2009_peat_accretion_age_depth_edited.xlsx", 
                             col_names = c("CAMS_lab_code", 
                                           "site_id", "core_id", "sample_id", 
                                           "sample_thickness_cm", "min_elevation_meters", 
                                           "c14_age", "c14_age_sd", "age", "c14_material"), 
                             skip = 1)
carbon_stock_data <- read_csv("./data/Drexler_2009/original/Drexler_et_al_2009_carbon_depthseries.csv")
cores <- read_csv("./data/Drexler_2009/original/Drexler_et_al_2009_cores.csv")
impacts <- read_csv("./data/Drexler_2009/original/Drexler_et_al_2009_impact_data.csv")

## Curate data ######################
## ... depthseries ##################
# There are multiple dated cores that do not have carbon stock data from the clearinghouse
# Age depth intervals do not match up with intervals with carbon stock data

age_depthseries <- age_depth_data %>%
  # remove outside data sources from table 
  filter(CAMS_lab_code != "Shlemon and Begg (1975)" & CAMS_lab_code != "Atwater et al. (1977)") %>%
  mutate(min_elevation_meters = as.numeric(min_elevation_meters), 
         study_id = "Drexler_et_al_2009") %>%
  mutate(max_elevation_meters = min_elevation_meters + .02) %>%
  mutate(core_elevation_datum = "MSL") %>%
  mutate(elevation_to_msl = ifelse(site_id == "BACL", -5.86, ifelse(site_id == "BACPTC", -6.28, 
                              ifelse(site_id == "BRI", 0.51, ifelse(site_id == "Franks Wetland", 0.27, 
                              ifelse(site_id == "SHERCI", -4.52, ifelse(site_id == "SHERL", -4.44, 
                              ifelse(site_id == "Time of Mandeval Tip", .2, 
                              ifelse(site_id == "VICI", -6.95, ifelse(site_id == "VIPP", -4.52, 
                              ifelse(site_id == "WTCI", -7.25, ifelse(site_id == "WTL", -5.18, 
                              ifelse(site_id == "Bacon Channel Island", .21, NA))))))))))))) %>%
  mutate(depth_min = (elevation_to_msl - max_elevation_meters) * 100, 
         depth_max = (elevation_to_msl - min_elevation_meters) * 100, 
         # The c14 error variable is 2*sd
         c14_age_sd = c14_age_sd / 2, 
         age_depth_model_reference = "YBP") %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd, c14_material, age,
         age_depth_model_reference, min_elevation_meters)

# Joining carbon stock and age depthseries 
# Because there are 13 cores with age depth data but only 3 of those are in the clearinghouse
# and thus have locational data, I'm filtering the non-matching cores out
depthseries_joined <- age_depthseries %>%
  filter(core_id %in% carbon_stock_data$core_id) %>%
  bind_rows(carbon_stock_data) %>%
  mutate(fraction_carbon_type = recode(fraction_carbon_type, 
                                       "fraction_total_carbon" = "total carbon")) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, 
         dry_bulk_density:fraction_carbon_type, 
         c14_age:age_depth_model_reference) %>%
  group_by(core_id) %>%
  arrange(depth_min, .by_group = TRUE)

## ... core-level ##################

source("./scripts/1_data_formatting/curation_functions.R")

cores_updated <- age_depthseries %>%
  filter(core_id %in% cores$core_id) %>%
  group_by(core_id) %>%
  summarize(core_elevation = first(min_elevation_meters),
            core_elevation_datum = "MSL") %>%
  merge(cores, by="core_id", all.x=TRUE, all.y=TRUE) %>%
  rename(core_elevation = core_elevation.x, 
         salinity_class = salinity_code, 
         vegetation_class = vegetation_code, 
         core_position_notes = position_code) %>%
  mutate(core_position_notes = recode(core_position_notes, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure")) %>% 
  recode_salinity(salinity_class = salinity_class) %>%
  recode_vegetation(vegetation_class = vegetation_class) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_elevation, core_position_notes, core_elevation_datum, 
         salinity_class, vegetation_class, core_length_flag)

## ... impact ###################
impacts <- impacts %>%
  # There were no diked sites in the study
  mutate(impact_class = recode(impact_class, "Diked" = "Natural"))

## Create study-level data ######

# import the CCRCN bibliography 
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

# link each study to primary citation and join with synthesis table
studies <- unique(cores_updated$study_id)

study_data_primary <- CCRCN_bib %>%
  select(BIBTEXKEY, CATEGORY, DOI) %>%
  rename(bibliography_id = BIBTEXKEY,
         study_type = CATEGORY,
         doi = DOI) %>%
  filter(bibliography_id %in% studies) %>%
  mutate(study_id = bibliography_id) %>%
  mutate(study_type = tolower(study_type)) %>%
  select(study_id, study_type, bibliography_id, doi) 

## QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores_updated)
test_colnames("depthseries", depthseries_joined)

test_variable_names(cores_updated)
test_variable_names(depthseries_joined)
test_variable_names(impacts)

numeric_test_results <- test_numeric_vars(depthseries_joined)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores_updated, depthseries_joined)

## Write data ######################
write_csv(depthseries_joined, "./data/Drexler_2009/derivative/Drexler_et_al_2009_depthseries.csv")
write_csv(cores_updated, "./data/Drexler_2009/derivative/Drexler_et_al_2009_cores.csv")
write_csv(impacts, "./data/Drexler_2009/derivative/Drexler_et_al_2009_impacts.csv")
write_csv(study_data_primary, "./data/Drexler_2009/derivative/Drexler_et_al_2009_study_citations.csv")
