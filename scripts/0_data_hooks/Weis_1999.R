# Coastal Carbon Research Coordination Network
# This script curates Weis et al (2001) age depth data and joins with Weis et al. 2001 carbon stock/core/species data in the CCRCN clearinghouse
# Contact: lonnemanm@si.edu

# Weis, Daniel A., John C. Callaway, and Richard M. Gersberg. 
# "Vertical accretion rates and heavy metal chronologies in wetland sediments of the Tijuana Estuary." Estuaries 24.6 (2001): 840-850.


## Workspace prep ###################
library(tidyverse)
library(RefManageR)

## Read in data #####################
raw_data <- read_csv("./data/Weis_et_al_2001/original/Weis_1999_age_depthseries.csv")
core_data <- read_csv("./data/Weis_et_al_2001/original/CCRCN_core_data.csv")
carbon_stocks_depthseries <- read_csv("./data/Weis_et_al_2001/original/CCRCN_depthseries_data.csv")
methods <- read_csv("./data/Weis_et_al_2001/original/CCRCN_methods_data.csv")
species <- read_csv("./data/Weis_et_al_2001/original/CCRCN_species_data.csv")

## Curate data ######################

## ... depthseries ##################
# Remove empty columns from CCRCN clearinghouse carbon stock data
# I want to check to make sure the raw carbon stock data in the thesis matches the data associated with the 2001 publication
carbon_stocks_depthseries <- carbon_stocks_depthseries %>%
  rename(dry_bulk_density_2001 = dry_bulk_density, 
         fraction_organic_matter_2001 = fraction_organic_matter) %>%
  select(core_id, depth_min, depth_max, dry_bulk_density_2001, fraction_organic_matter_2001)

depthseries <- raw_data %>%
  mutate(study_id = "Weis_et_al_2001", 
         core_id = as.factor(core_id),
         site_id = "Tijuana_Estuary") %>%
  rename(dry_bulk_density = bulk_density) %>%
  # 2 cm sections were taken. Each depth is an odd number, likely represents +- 1 of depth min and max 
  mutate(depth_min = depth - 1, depth_max = depth + 1, 
         fraction_organic_matter = organic_content_percent / 100,
  # recode core IDs to match core IDs in CCRCN synthesis
         core_id = recode_factor(core_id, 
                                 "E1A" = "Tijuana_Estuary_East_low",
                                 "E1B" = "Tijuana_Estuary_East_mid_1",
                                 "E1C" = "Tijuana_Estuary_East_mid_2",
                                 "W1A" = "Tijuana_Estuary_West_low",
                                 "W1B" = "Tijuana_Estuary_West_mid_1",
                                 "W1C" = "Tijuana_Estuary_West_mid_2")) %>%
  mutate(cs137_activity = cs137_net_counts) %>%
  mutate(cs137_unit = ifelse(is.na(cs137_activity) == FALSE, "counts_per_hour", NA)) %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, cs137_activity, cs137_unit)

# Merge with carbon stock data from the clearinghouse
# Make sure they match the carbon stock data digitized from the thesis tables
# test <- depthseries %>%  
#   # Merge with carbon stock data from the clearinghouse
#   # Must match core ID and depth interval 
#   merge(carbon_stocks_depthseries, by=c("core_id", "depth_min", "depth_max"), 
#         all.x=TRUE, all.y = TRUE) %>%
#   mutate(dbd = ifelse(dry_bulk_density == dry_bulk_density_2001, TRUE, FALSE), 
#          fom = ifelse(fraction_organic_matter == fraction_organic_matter_2001, TRUE, FALSE))
# Results: They do match, except it looks like one interval's depth max is off 
# I'll keep the carbon stock derived from the thesis table

## Core data curation ##################
core_data <- core_data %>%
  mutate(study_id = "Weis_et_al_2001") %>%
  mutate(core_position_notes = recode(core_position_notes, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure"),
         core_date = "1998-09-01", # dissertation says between august and september 1997
         inundation_class = tolower(inundation_class)) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, core_position_notes, 
         salinity_class, vegetation_class, inundation_class, core_length_flag)
  
## Method and Species
methods <- methods %>%
  select(-X1) 

species <- species %>%
  mutate(site_id = "Tijuana_Estuary") %>%
  select(study_id, site_id, core_id, species_code) 

## Generate citation ##############
study <- "Weis_et_al_2001"

# Secondary thesis citation 
weis <- as.data.frame(BibEntry(bibtype = "Mastersthesis", 
                 key = "Weis 2001", 
                 title = "Vertical accretion rates and heavy metal chronologies in wetland sediments of Tijuana Estuary",
                 author = "Weis, Daniel Anthony", 
                 school = "San Diego State University",
                 year = "1999")) %>%
  rownames_to_column("key") %>%
  mutate(study_id = study, 
         bibliography_id = study, 
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Primary citation
doi <- "10.2307/1353175"

biblio_raw <- GetBibEntryWithDOI(doi)
biblio_df <- as.data.frame(biblio_raw)
study_citations <- biblio_df %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study, 
         study_id = study,
         key = study,
         publication_type = "Article") %>%
  bind_rows(weis) %>%
  mutate(year = as.numeric(year), 
         volume = as.numeric(volume), 
         number = as.numeric(number)) %>%
  select(-pages) %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key") 

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001.bib")

## QA/QC ##########################
source("./scripts/1_data_formatting/qa_functions.R")

test_colnames("core_level", core_data)
test_colnames("depthseries", depthseries)
test_colnames("species", species)

test_varnames(core_data)
test_varnames(depthseries)
test_varnames(species)

test_numeric_vars(depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(core_data, depthseries)

## Write data #####################
write_csv(depthseries, "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_depthseries.csv")
write_csv(core_data, "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_cores.csv")
write_csv(methods, "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_methods.csv")
write_csv(species, "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_species.csv")
write_csv(study_citations, "./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_study_citations.csv")


