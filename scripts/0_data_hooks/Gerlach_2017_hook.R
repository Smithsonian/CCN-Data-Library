library(tidyverse)
library(RefManageR)

# Appending c14 data to carbon stocks data that was already prepared in the Holmquist synthesis
cores_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_species.csv")
methods_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_methods.csv")

# Digitized table from Gerlach et al 2017
age_depth_raw <- read_csv("./data/primary_studies/Gerlach_2017/original/gerlach_2017_c14.csv")

# Digitized extra horizon data
horizons_raw <- read_csv("data/primary_studies/Gerlach_2017/original/Gerlach_DepthSeries_HorizonData.csv")

# Interpreted extra elevation data from paper SI
extra_elevations <- read_csv("data/primary_studies/Gerlach_2017/original/gerlach_extra_elevations_191023.csv")

## Curate Data ####

# Core ID for c14 data
core_id_value <- "LMR_14_9B"
study_id_value <- "Gerlach_et_al_2017"

site_relationships <- cores_raw %>%
  select(core_id, site_id)
  
# species
species <- site_relationships %>%
  full_join(., species_raw %>% select(-site_id)) %>%
  select(study_id, site_id, core_id, species_code)

# methods
methods <- methods_raw %>% mutate(method_id = "single set of methods")

# depthseries
age_depth <- age_depth_raw %>%
  mutate(core_id = core_id_value,
         study_id = study_id_value,
         depth_min = Depth_in_core,
         depth_max = Depth_in_core,
         sample_id = paste("OS", NOSAMS_lab_number, sep="-")) %>%
  separate(calibrated_age, into=c("age_min", "age_max"), "-") %>%
  mutate(age_min = as.numeric(age_min),
         age_max = as.numeric(age_max)) %>%
  select(-c(Depth_in_core, NOSAMS_lab_number)) %>%
  rename(c14_age_sd=c14_age_error)

depthseries <- depthseries_raw %>%
  select(-X1) %>%
  bind_rows(age_depth) %>%
  full_join(horizons_raw) %>%
  arrange(core_id, depth_min) %>%
  rename(marker_date_sd = marker_sd,
         marker_notes = marker_type) %>%
  mutate(method_id = "single set of methods") %>%
  select(-site_id) %>%
  full_join(site_relationships)

# cores
cores <- cores_raw %>%
  full_join(extra_elevations) %>%
  rename(core_year = core_date) %>%
  mutate(salinity_class = recode(salinity_class, "freshwater" = "fresh"))


## Citations ####
citations_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_bibliography.csv")

study_citations <- citations_raw %>%
  select(-key) %>%
  mutate(publication_type = "associated source",
         bibliography_id = "Gerlach_et_al_2017_article") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017.bib")
write_csv(study_citations, "data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_study_citations.csv")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects

methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
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

# write files
write_csv(depthseries, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_depthseries.csv")
write_csv(cores, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_cores.csv")
write_csv(species, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_species.csv")
write_csv(methods, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_methods.csv")
