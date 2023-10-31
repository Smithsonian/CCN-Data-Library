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
methods_raw <- read_csv("./data/primary_studies/Noe_2016/original/Noe_et_al_2016_methods.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Curate Data Tables ####

methods <- methods_raw %>%
  select(-publication_type) %>%
  mutate(fraction_carbon_type = "organic carbon",
         method_id = "single set of methods")

## Curate depthseries data #####
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
         method_id = "single set of methods",
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
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, 
         dry_bulk_density, fraction_carbon, 
         total_pb210_activity, total_pb210_activity_sd, excess_pb210_activity, pb210_unit, 
         age, age_sd)

## Curate core data #########################
cores <- cores_raw %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_elevation, 
         salinity_class, vegetation_class, core_length_flag) %>%
  select_if(function(x) {!all(is.na(x))}) %>%
  mutate(salinity_class = recode(salinity_class, "freshwater" = "fresh"),
         year = 2011,
         month = 11)

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

if(!file.exists("data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_study_citations.csv")){
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI("10.1007/s12237-016-0066-4")
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Noe_et_al_2016_combined", 
           study_id = "Noe_et_al_2016",
           publication_type = "combined dataset and manuscript") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016.bib")
  write_csv(study_citations, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_study_citations.csv")
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

## QA/QC ##########################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write data ######################
write_csv(depthseries, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_depthseries.csv")
write_csv(cores, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_cores.csv")
write_csv(impacts, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_impacts.csv")
write_csv(species, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_species.csv")
write_csv(methods, "./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_methods.csv")
