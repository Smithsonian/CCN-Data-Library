## CCRCN Data Library Hook Script ####

# Data Release: Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5f7e18d382ce1d74e7dda178
# Contact: Sarai Piazza

# Data Citation:
# Piazza, S.C., Steyer, G.D., Cretini, K.F., Sasser, C.E., Visser, J.M., Holm, G.O., Sharp, L.A., Evers, E., and Meriwether, J.R., 2021, 
# Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities: U.S. Geological Survey data release, 
# https://doi.org/10.5066/P9D8WTQW.

# Report Citation:
# Piazza, S.C., Steyer, G.D., Cretini, K.F., Sasser, C.E., Visser, J.M., Holm, G.O., Jr., Sharp, L.A., Evers, D.E., and
# Meriwether, J.R., 2011, Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities:
# U. S. Geological Survey Open-File Report 2011-1094, 126 p.

## Prep Workspace ####

# load libraries
library(tidyverse)
# library(lubridate)
library(RefManageR)
# library(anytime)

# read in data
raw_sites <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_site.csv")
raw_cores <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_depthseries.csv")
raw_species <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_species.csv")
raw_impacts <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_impacts.csv")
raw_methods <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_material_and_methods.csv")
crms_coords <- read_csv("docs/CRMS_Long_Lat.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Piazza_et_al_2020"

# sites
# this is a redundant with the cores table
# sites <- raw_sites %>%
#   mutate(salinity_class = recode(salinity_class, "intermediate" = "brackish"))
# sites <- reorderColumns("sites", sites)

# depthseries
# uncontrolled: fraction_nitrogen
depthseries <- raw_depthseries %>% select(-fraction_nitrogen) %>% 
  mutate(method_id = "single set of methods",
         cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA))

# isolate cores missing from core table...Jim says there should be 45...WTH
# I believe Sarai sent more depthseries information for the publication but the core-level wasn't updated with the new ones
missing_cores <- raw_depthseries %>% 
  filter(!(core_id %in% unique(raw_cores$core_id))) %>% 
  distinct(study_id, site_id, core_id) %>% 
  # add site-level crms coordinates
  mutate(crms_id = gsub("_H", "", site_id)) %>% 
  left_join(crms_coords, by = c("crms_id" = "CRMS Site")) %>% 
  rename(core_latitude = Latitude, core_longitude = Longitude) %>% 
  mutate(core_position_method = "other low resolution", 
         core_position_notes = "coordinates obtained at the site-level from the CRMS network website.",
         salinity_class = "fresh",
         salinity_method = "measurement", 
         vegetation_class = "emergent", 
         vegetation_method = "measurement",
         core_length_flag = "core depth limited by length of corer") %>% 
  select(-crms_id)
# any other information for these cores to add?

# cores
cores <- raw_cores %>%
  bind_rows(missing_cores) %>%
  mutate(salinity_class = recode(salinity_class, "intermediate" = "brackish"),
         core_year = 2007,
         core_notes = "core sampling year approximate, sites where surveyed between Spring 2006 and Fall 2007") %>% 
  arrange(site_id, core_id)
# cores <- reorderColumns("cores", cores)

# species
species <- raw_species %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

# methods
methods <- raw_methods %>%
  mutate(method_id = "single set of methods",
         carbonates_removed = FALSE,
         carbonate_removal_method = "carbonates not removed",
         fraction_carbon_type = "total carbon",
         fraction_carbon_method = "EA") %>% 
  reorderColumns("methods",.)

# impacts

impacts <- raw_impacts %>% 
  bind_rows(
    missing_cores %>% 
      select(study_id, site_id, core_id) %>% 
      filter(site_id %in% c("CRMS0605", "CRMS0605_H", "CRMS1277", "CRMS1277_H")) %>% 
      mutate(impact_class = "storm or wind")
  )

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

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)

fractionNotPercent(depthseries)
results <- test_numeric_vars(depthseries)

testIDs(cores, depthseries, by = "core")
testIDs(cores, depthseries, by = "site")

## ... Map sampling points ####
library(leaflet)

leaflet(cores) %>% 
  addTiles() %>%
  # addProviderTiles(options = ) %>% 
  addCircles(lng = ~longitude, lat = ~latitude, label = ~core_id)

## Write derivative data ####
write_csv(impacts, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_impacts.csv")
write_csv(cores, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_depthseries.csv")

#### Study Citation ####

data_release_doi <- "10.5066/P9D8WTQW"
report_doi <- "10.3133/ofr20111094"

data_bib <- GetBibEntryWithDOI(data_release_doi)
report_bib <- GetBibEntryWithDOI(report_doi)

# Convert citations to dataframe
data_citation <- as.data.frame(data_bib) %>%
  mutate(study_id = id) %>%
  mutate(bibliography_id = "Piazza_et_al_2011_data",
         publication_type = "primary dataset")

report_citation <- as.data.frame(report_bib) %>%
  mutate(study_id = id,
         bibliography_id = "Piazza_et_al_2011_article",
         publication_type = "associated source")

# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- bind_rows(report_citation, data_citation) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020.bib")
write_csv(study_citations, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_study_citations.csv")

