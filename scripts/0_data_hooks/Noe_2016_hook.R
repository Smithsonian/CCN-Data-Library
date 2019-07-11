# Coastal Carbon Research Coordination Network
# This script prepares carbon and age depth data for Noe et al 2016 
# Contact: klingesd@si.edu
#          lonnemanM@si.edu

# Noe, Gregory B., et al. "Contemporary deposition and long-term accumulation of sediment and nutrients 
# by tidal freshwater forested wetlands impacted by sea level rise." 
# Estuaries and Coasts 39.4 (2016): 1006-1019.
# 10.1007/s12237-016-0066-4


## workspace Prep ######################

library(tidyverse)
library(RefManageR)

# Import data
depthseries_raw <- read_csv("./data/primary_studies/Noe_2016/original/Noe_2016_depthseries_supplemental.csv")
cores_raw <- read_csv("./data/primary_studies/Noe_2016/original/Noe_2016_cores.csv")
species_raw <- read_csv("./data/primary_studies/Noe_2016/original/Noe_2016_species.csv")
impacts_raw <- read_csv("./data/primary_studies/Noe_2016/original/Noe_2016_impacts.csv")

## Curate depthseries data ##################
# The raw data is in a Word document where red colored text indicated a modeled value. 
# The table was moved to an excel sheet and red colored text was moved to new columns that indicate they were modeled. 

depthseries <- depthseries_raw %>%
  rename(core_id = `Coring location`, 
         mid_depth = `Increment mid-depth (cm)`,
         fraction_carbon = `C (%)`, 
         dry_bulk_density = `Bulk density (g cm-3)`,
         total_pb210_activity = `Total 210Pb activity (dpm g-1)`, 
         total_pb210_activity_sd = `Total 210Pb error (dpm g-1)`,
         excess_pb210_activity = `Unsupported 210Pb`, 
         age = `Age (yr)`, 
         age_sd = `Age uncertainty (+- yr)`) %>%
  mutate(study_id = "Noe_et_al_2016",
  # specify site IDs based on CCRCN clearinghouse data 
         site_id = recode(core_id, 
                   "W1" = "Butler_Island",
                   "W2" = "Richmond_Island", 
                   "W3" = "Turkey_Creek",
                   "W4" = "Turkey_Creek")) %>%
  mutate(pb210_unit = ifelse(is.na(total_pb210_activity) == FALSE, "disintegrations_per_minute_per_gram", NA),
         fraction_carbon = fraction_carbon / 100) %>% #, 
         #age = ifelse(is.na(total_pb210_activity) == TRUE, NA, age),
         #age_sd = ifelse(is.na(total_pb210_activity) == TRUE, NA, age_sd)) %>%
  # Convert mid depth to min and max
  mutate(depth_min = ifelse(core_id == "W3", mid_depth - 1, mid_depth - .5), 
         depth_max = ifelse(core_id == "W3", mid_depth + 1, mid_depth + .5)) %>%
  select(study_id, site_id, core_id, depth_min, depth_max, 
         dry_bulk_density, fraction_carbon, 
         total_pb210_activity, total_pb210_activity_sd, excess_pb210_activity, pb210_unit, 
         age, age_sd)

## Curate core data #########################
cores <- cores_raw %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_elevation, 
         salinity_class, vegetation_class, core_length_flag)

## Curate species and impact data ###########
species <- species_raw %>%
  mutate(site_id = recode(core_id, 
                   "W1" = "Butler_Island",
                   "W2" = "Richmond_Island", 
                   "W3" = "Turkey_Creek",
                   "W4" = "Turkey_Creek")) %>%
  mutate(species_code = ifelse(core_id == "W4", "Taxodium distichum", species_code)) %>%
  select(study_id, site_id, core_id, species_code)

impacts <- impacts_raw %>%
  mutate(impact_class = tolower(impact_class)) %>%
  mutate(site_id = recode(core_id, 
                          "W1" = "Butler_Island",
                          "W2" = "Richmond_Island", 
                          "W3" = "Turkey_Creek",
                          "W4" = "Turkey_Creek")) %>%
  select(study_id, site_id, core_id, impact_class)


## Create study-level data ######
# Get bibtex citation from DOI
biblio_raw <- GetBibEntryWithDOI("10.1007/s12237-016-0066-4")
biblio_df <- as.data.frame(biblio_raw)
study_citations <- biblio_df %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = "Noe_et_al_2016", 
         study_id = "Noe_et_al_2016",
         key = "Noe_et_al_2016",
         publication_type = "Article", 
         year = as.numeric(year), 
         volume = as.numeric(volume), 
         number = as.numeric(number)) %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "./data/Noe_2016/derivative/Noe_et_al_2016.bib")

## QA/QC ##########################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("impact", impacts)
test_varnames(cores)
test_varnames(depthseries)
test_varnames(impacts)
test_varnames(species)

test_numeric_vars(depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## Write data ######################
write_csv(depthseries, "./data/Noe_2016/derivative/Noe_et_al_2016_depthseries.csv")
write_csv(cores, "./data/Noe_2016/derivative/Noe_et_al_2016_cores.csv")
write_csv(impacts, "./data/Noe_2016/derivative/Noe_et_al_2016_impacts.csv")
write_csv(species, "./data/Noe_2016/derivative/Noe_et_al_2016_species.csv")
write_csv(study_citations, "./data/Noe_2016/derivative/Noe_et_al_2016_study_citations.csv")
