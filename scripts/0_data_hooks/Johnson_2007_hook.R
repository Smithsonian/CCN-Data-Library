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
methods <- read_csv("./data/primary_studies/Johnson_2007/original/Johnson_et_al_2007_methods_data.csv")

## Curate depthseries #########
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
  select(study_id, site_id, core_id, depth_min, depth_max, sample_id,
         everything())
  
## Generate citation ##############
doi <- "10.1016/j.orggeochem.2006.06.006"
study <- "Johnson_et_al_2007"

biblio_raw <- GetBibEntryWithDOI(doi)
biblio_df <- as.data.frame(biblio_raw)
study_citations <- biblio_df %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study, 
         study_id = study,
         key = study,
         publication_type = "Article", 
         year = as.numeric(year), 
         volume = as.numeric(volume), 
         number = as.numeric(number)) %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007.bib")

## QA/QC ##########################
source("./scripts/1_data_formatting/qa_functions.R")

test_colnames("core_level", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)

test_varnames(cores)
test_varnames(depthseries)
test_varnames(species)

test_numeric_vars(depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## Write data #####################
write_csv(depthseries, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_depthseries.csv")
write_csv(cores, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_cores.csv")
write_csv(methods, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_methods.csv")
write_csv(species, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_species.csv")
write_csv(study_citations, "./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_study_citations.csv")
