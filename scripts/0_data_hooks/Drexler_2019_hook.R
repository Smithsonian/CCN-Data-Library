# Curation script for Drexler et al. 2019 
# Drexler, Judith Z., et al. 
# "Carbon accumulation and vertical accretion in a restored versus historic salt marsh 
# in southern Puget Sound, Washington, United States." Restoration Ecology.

# Contact: Michael Lonneman, lonnemanM@si.edu

library(tidyverse)
library(RefManageR)

raw_cores <- read_csv("./data/primary_studies/drexler_2019/original/coordinates.csv")
raw_depthseries <- read_csv("./data/primary_studies/drexler_2019/original/nisqually_depthseries_manual_edits.csv")
raw_species <- read_csv("./data/primary_studies/drexler_2019/original/species.csv")

## Curate data #########
study_id_value <- "Drexler_et_al_2019"

## ... depthseries #####
depthseries <- raw_depthseries %>%
  rename(total_pb210_activity_sd = total_pb210_sd) %>%
  mutate(core_id = gsub("Core ", "", core_id)) %>%
  mutate(study_id = study_id_value,
         depth_min = depth_max - 2,
         site_id = ifelse(grepl("SG", core_id), "Six_Gill_Slough", "Animal_Slough")) %>%
  # Modify carbon stock variables: 
  mutate(fraction_carbon = percent_carbon / 100,
         pb210_unit = ifelse(!is.na(total_pb210_activity), "distintegrations per minute per gram", NA)) %>%
  select(study_id, site_id, core_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon, 
         total_pb210_activity, total_pb210_activity_sd,  
         ra226_activity, ra226_activity_sd, 
         excess_pb210_activity, excess_pb210_activity_sd, pb210_unit)

## ... core data #######
coordinates <- raw_cores %>%
  filter(!is.na(Latitude)) %>%
  rename(core_latitude = Latitude,
         core_longitude = Longitude, 
         core_id = `Core abbreviations`,
         core_notes = `Full core ID`, 
         core_date = `Date of collection`) %>%
  mutate(study_id = study_id_value)

cores <- depthseries %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id)) %>%
  merge(coordinates, by="core_id", all.x=TRUE, all.y=TRUE) %>%
  mutate(core_date = mdy(core_date)) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, core_notes)

## ... site data #######
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- cores %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) 

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

sites <- cores %>%
  group_by(site_id) %>%
  summarize(study_id = study_id_value) %>%
  merge(site_boundaries, by="site_id") %>%
  mutate(salinity_class = "polyhaline",
         vegetation_class = "emergent") %>%
  select(study_id, site_id, everything())

## ... impact data #####
impacts <- cores %>%
  select(study_id, site_id, core_id) %>%
  filter(site_id == "Six_Gill_Slough") %>%
  mutate(impact_class = "tidally restored")
  
# ## ... species data ####
species <- cores %>%
  select(study_id, site_id, core_id) %>%
  filter(site_id == "Animal_Slough") %>%
  merge(raw_species, by="site_id", all.x=TRUE, all.y=TRUE)

## Create study-level data ######
# Get bibtex citation from DOI
biblio_raw <- GetBibEntryWithDOI("10.1111/rec.12941")
biblio_df <- as.data.frame(biblio_raw)
study_citations <- biblio_df %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study_id_value, 
         study_id = study_id_value,
         key = study_id_value,
         publication_type = "Article", 
         year = as.numeric(year)) %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019.bib")

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores)
test_colnames("site_level", sites) 
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("impact", impacts)

test_variable_names(cores)
test_variable_names(sites)
test_variable_names(impacts)
test_varnames(depthseries)
test_variable_names(species)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

test_numeric_vars(depthseries)

## Export curated data ###########
write_csv(depthseries, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_depthseries.csv")
write_csv(cores, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_cores.csv")
write_csv(sites, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_sites.csv")
write_csv(impacts, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_impacts.csv")
write_csv(species, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_species.csv")
write_csv(study_citations, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_study_citations.csv")
