# Coastal Carbon Research Coordination Network
# This script curates age depth data from a table in Johnson et al. (2007) 
# and merges with carbon stock data in the CCRCN clearinghouse

# Core-and species-level data was already appropriately formatted from Holmquist synthesis 
# and will be read in for QA only

# Contact: lonnemanm@si.edu

# Johnson, Beverly J., et al. "Middle to Late Holocene fluctuations of C3 and C4 vegetation in a northern New England salt marsh,
# Sprague Marsh, Phippsburg Maine." Organic Geochemistry 38.3 (2007): 394-403.

## Workspace prep ###################
library(tidyverse)
library(RefManageR)

age_depth_raw <- read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_age_depthseries.csv")
carbon_stocks_raw <- read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_depthseries_data.csv")
cores <-  read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_core_data.csv")
species <- read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_species_data.csv")
methods_raw <- read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_methods_data.csv")

## Curate depthseries #########

methods <- methods_raw %>%
  select_if(function(x) {!all(is.na(x))}) %>%
  select(-publication_type) %>%
  mutate(fraction_carbon_type = "organic carbon",
         method_id = "single set of methods")

## ... Age depth #####
age_depth <- age_depth_raw %>%
  # delta c13 was measured for the bulk sediment and for higher plant leaf wax (HPLW) lipid biomarkers
  # I'll only include bulk sediment for now, check if there should be a separate column for HPLW delta c13 
  rename(delta_c13 = delta_c13_bulk,
         age = calibrated_average, 
         age_sd = cal_yr_bp_sd, 
         c14_notes = depth_interval_notes) %>%
  select(depth_min, sample_id,
         c14_age, c14_age_sd, c14_material, c14_notes,
         delta_c13, 
         age, age_sd)

## ... Carbon and join ####
depthseries <- carbon_stocks_raw %>%
  mutate(site_id = "Sprague_Marsh") %>%
  select(study_id, site_id, core_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon) %>%
  merge(age_depth, by="depth_min", all.x=TRUE, all.y=TRUE) %>%
  mutate(method_id = "single set of methods") %>%
  select(study_id, site_id, core_id, depth_min, depth_max, sample_id,
         everything())
  
## Add extra info for cores
cores <- cores %>% 
  mutate(year = 2002,
         month = 5)

## Generate citation ##############

if(!file.exists("data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_study_citations.csv")){
  doi <- "10.1016/j.orggeochem.2006.06.006"
  study <- "Johnson_et_al_2007"
  
  biblio_raw <- GetBibEntryWithDOI(doi)
  biblio_df <- as.data.frame(biblio_raw) 
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Johnson_et_al_2007_article", 
           study_id = study,
           publication_type = "associated source") %>%
    select(study_id, bibliography_id, publication_type, everything()) %>%
    remove_rownames()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007.bib")
  write_csv(study_citations, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_study_citations.csv")
  
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ##########################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names) # year and positon method

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write data #####################
write_csv(depthseries, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_depthseries.csv")
write_csv(cores, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_cores.csv")
write_csv(methods, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_methods.csv")
write_csv(species, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_species.csv")
