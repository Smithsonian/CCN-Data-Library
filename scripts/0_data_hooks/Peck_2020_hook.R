# Dataset: Controls on sediment accretion and blue carbon burial in tidal saline wetlands: Insights from the Oregon coast, U.S.A.
# 
# Authors: Erin Peck <peckerin@oregonstate.edu>; Robert Wheatcroft;  Laura Brophy 
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11317820
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# peck_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# peck_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# peck_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# peck_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
# peck_et_al_2020_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.

library(tidyverse)
library(RefManageR)
library(lubridate)
# library(anytime)

cores_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_materials_and_methods.csv")
impacts_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_impacts.csv")

# Align tables to database guidance

impacts <- impacts_raw

methods <- methods_raw %>%
  mutate(method_id = "single set of methods",
         dating_notes = "activities collected to the detection limit",
         excess_pb210_model = "CIC",
         excess_pb210_rate = "mass accumulation") %>% 
  select(-sediment_sieved_flag) # not confirmed in the paper whether sediment was sieved, just that large plant matter was removed

cores <- cores_raw %>%
  mutate(core_date = as.Date(core_date, format = "%m/%d/%y"),
         core_notes = ifelse(!is.na(core_notes),
                             paste0("Abbreviated core ID: ", abbreviated_core_id, ". ", core_notes),
                             paste0("Abbreviated core ID: ", abbreviated_core_id))) %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK")) %>%
  select(-abbreviated_core_id, -core_date) %>%
  select(study_id, site_id, core_id, core_year, everything())

depthseries <- depthseries_raw %>%
  select(-fraction_carbon_modeled, -dry_bulk_density_modeled) %>%
  rename(fraction_carbon = fraction_carbon_measured,
         dry_bulk_density = dry_bulk_density_measured) %>%
  mutate(method_id = "single set of methods",
         pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         pb214_unit = ifelse(!is.na(pb214_activity), "disintegrationsPerMinutePerGram", NA), 
         cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA))

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

# write citation

if(!file.exists("data/primary_studies/peck_2020/derivative/peck_et_al_2020_study_citations.csv")){
  
  data_doi <- "https://doi.org/10.25573/serc.11317820.v2"
  pub_doi <- "10.1029/2019JG005464"
  
  citation_raw <- as.data.frame(GetBibEntryWithDOI(c(data_doi, pub_doi)))
  
  study_citations <- citation_raw %>%
    mutate(bibliography_id = c("Peck_et_al_2020_data", "Peck_et_al_2020_article"),
           study_id = "Peck_et_al_2020",
           publication_type = c("primary dataset", "associated source")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  ## Format bibliography
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/peck_2020/derivative/peck_et_al_2020.bib")
  write_csv(study_citations, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

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

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries) # a few cores will not be present in the depthseries
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

# write files
write_csv(cores, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_depthseries.csv")
write_csv(impacts, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_impacts.csv")

