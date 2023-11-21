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
library(RefManageR)

## Read in data #####################

# The original data from the table is stored in the Drexler_et_al_2009_peat_accretion_age_depth.csv file. 
# Because the site-core-sample ID field could not easily be separated using tidy logic, I did it manually in excel
# Additionally, the negative sign character used in the elevation field was causing problems in both excel and R and was replaced with (-)
# That edited file is read in 
# Different computers are having trouble parsing the column names with spaces, leading to errors during cutation. 
# Renaming columns in the read_excel call and skipping the first line which represents the old column names
age_depth_data <- read_excel("./data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_peat_accretion_age_depth_edited.xlsx", 
                             col_names = c("CAMS_lab_code", 
                                           "site_id", "core_id", "sample_id", 
                                           "sample_thickness_cm", "min_elevation_meters", 
                                           "c14_age", "c14_age_sd", "age", "c14_material"), 
                             skip = 1)
carbon_stock_data <- read_csv("./data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_carbon_depthseries.csv")
cores_raw <- read_csv("./data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_cores.csv")
impacts_raw <- read_csv("./data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_impact_data.csv")
methods_raw <- read_csv("./data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_methods.csv")

## Curate data ######################

## ... methods ####

methods <- methods_raw %>%
  select_if(function(x) {!all(is.na(x))}) %>%
  select(-publication_type, -fraction_carbon_flag) %>%
  mutate(fraction_carbon_type = "total carbon",
         method_id = "single set of methods",
         carbon_profile_notes = "fraction carbon extrapolated from subset of samples")

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
  select(study_id, site_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd, c14_material, age,
         age_depth_model_reference, min_elevation_meters) %>%
  arrange(site_id, core_id, depth_min, sample_id)

# Joining carbon stock and age depthseries 
# Because there are 13 cores with age depth data but only 3 of those are in the clearinghouse
# and thus have locational data, I'm filtering the non-matching cores out
depthseries <- age_depthseries %>%
  filter(core_id %in% carbon_stock_data$core_id) %>%
  bind_rows(carbon_stock_data) %>%
  mutate(fraction_carbon_type = recode(fraction_carbon_type, 
                                       "fraction_total_carbon" = "total carbon"),
         method_id = "single set of methods") %>% 
  group_by(core_id) %>%
  arrange(core_id, depth_min, sample_id, .by_group = TRUE) %>%
  # Following attribute should only be in methods table
  select(-c(age_depth_model_reference, fraction_carbon_type, min_elevation_meters, site_id)) %>%
  ungroup()

# adding in site_id from cores_raw
ids <- cores_raw[,c("site_id", "core_id")]
depthseries <- left_join(depthseries, ids)

## ... core-level ##################

source("./scripts/1_data_formatting/curation_functions.R")

core_elevations_navd88 <- data.frame(core_id = c("BACHI", "FW", "TT"),
                                     core_elevation = c(1.48, 1.54, 1.47),
                                     core_elevation_datum = rep("NAVD88", 3))

cores <- cores_raw %>%
  mutate(year = 2005,
    core_position_notes = recode(position_code, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure")) %>% 
  rename(vegetation_class = vegetation_code,
         salinity_class = salinity_code) %>% 
  recode_salinity(salinity_class = salinity_class) %>%
  recode_vegetation(vegetation_class = vegetation_class) %>%
  select(-core_elevation) %>% 
  left_join(core_elevations_navd88) %>%
  mutate(core_elevation_accuracy = 0.075,
         core_elevation_method = "RTK",
         core_position_method = "RTK") %>% 
  select(study_id, site_id, core_id, core_latitude, core_longitude, year, core_position_method, core_position_notes, core_elevation, core_elevation_datum, 
         core_elevation_accuracy, core_elevation_method,
         salinity_class, vegetation_class, core_length_flag)

## ... impact ###################
impacts <- impacts_raw %>%
  # There were no diked sites in the study
  mutate(impact_class = recode(impact_class, "Diked" = "Natural")) %>%
  mutate(impact_class = tolower(impact_class),
         # fixing an error from Holmquist 2018 data.
         impact_class = ifelse(core_id == "BACHI", "natural", impact_class))

## ... species ####

species <- read_csv("data/primary_studies/Drexler_2009/original/Drexler_et_al_2009_species.csv")

## Create study-level data ######

if(!file.exists("./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_study_citations.csv")){
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI("10.1007/s12237-009-9202-8")
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Drexler_et_al_2009_article", 
           study_id = "Drexler_et_al_2009",
           publication_type = "associated source") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009.bib")
  write_csv(study_citations, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects

impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write data ######################
write_csv(depthseries, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_depthseries.csv")
write_csv(cores, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_cores.csv")
write_csv(impacts, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_impacts.csv")
write_csv(species, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_species.csv")
write_csv(methods, "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_methods.csv")

