# Hook script for:
# Arriola, Jill M., and Jaye E. Cable. 
# "Variations in carbon burial and sediment accretion along a tidal creek in a Florida salt marsh." 
# Limnology and Oceanography 62.S1 (2017): S15-S28.
# 10.1002/lno.10652

# Data in supplementary information associated with publication
# Core lat/long and date acquired from correspondence with Jill Arriola. 

library(tidyverse)
library(RefManageR)
library(rnaturalearth)
library(sf)
source("./scripts/1_data_formatting/qa_functions.R")

cores_raw <- read_csv("./data/primary_studies/Arriola_2017/intermediate/arriola_and_cable_2017_core_raw.csv")
depthseries_raw <- read_csv("./data/primary_studies/Arriola_2017/intermediate/arriola_and_cable_2017_depthseries_raw.csv", na = "N/A")
methods_raw <- read_csv("./data/primary_studies/Arriola_2017/original/arriola_cable_2017_material_and_methods.csv")

## Curate cores ####
study_id_value = "Arriola_and_Cable_2017"

cores <- cores_raw %>%
  rename(core_id = `Core ID`) %>%
  separate(`Extraction date`, into=c("core_month", "core_day", "core_year"), sep="/") %>%
  separate(`Lat (N)`, into=c("lat_deg", "lat_min"), sep=" ") %>%
  separate(`Long (W)`, into=c("long_deg", "long_min"), sep=" ") %>%
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         long_deg = as.numeric(long_deg),
         long_min = as.numeric(long_min)) %>%
  mutate(core_latitude = lat_deg + (lat_min/60),
         core_longitude = -(long_deg + (long_min/60)),
         study_id = study_id_value,
         site_id = recode(core_id,
                          "M" = "snipe_creek_low_marsh",
                          "MC" = "snipe_creek_low_marsh",
                          "HE" = "snipe_creek_hammock",
                          "HI" = "snipe_creek_hammock",
                          "S" = "snipe_creek_high_marsh",
                          "F" = "snipe_creek_high_marsh"),
         core_position_method = "handheld",
         core_id = paste("snipe_creek", core_id, sep="_")) %>%
  select(study_id, site_id, core_id, core_year, core_month, core_day, 
         core_latitude, core_longitude, core_position_method)

## Curate depthseries ####
depthseries_curated <- depthseries_raw %>%
  rename(delta_c13 = d13C, fraction_carbon = `OC %`, dry_bulk_density = `dbd_gsed/cc`,
         cs137_activity = `137Cs`, ra226_activity =  `226Ra`, excess_pb210_activity = `210Pbex`, excess_pb210_activity_se = error,
         age = `Year of`, fraction_organic_matter = `OM%`) %>%
  separate(core_id, into=c("core_id", "depth"), sep=" ") %>%
  separate(depth, into=c("depth_min", "depth_max"), sep="-") %>%
  mutate(site_id = recode(core_id,
                          "M" = "snipe_creek_low_marsh",
                          "MC" = "snipe_creek_low_marsh",
                          "HE" = "snipe_creek_hammock",
                          "HI" = "snipe_creek_hammock",
                          "S" = "snipe_creek_high_marsh",
                          "F" = "snipe_creek_high_marsh")) %>%
  mutate(fraction_carbon = fraction_carbon / 100,
         fraction_organic_matter = fraction_organic_matter / 100,
         study_id = study_id_value,
         core_id = paste("snipe_creek", core_id, sep="_"),
         depth_min = as.numeric(depth_min),
         depth_max = as.numeric(depth_max))

depthseries <- reorderColumns("depthseries", depthseries_curated) %>%
  select(1:14) # Remove all uncontrolled variables
  
methods <- methods_raw %>%
  mutate(study_id = study_id_value)

## Citation ####
doi <- "https://doi.org/10.1002/lno.10652"

citation_raw <- as.data.frame(GetBibEntryWithDOI(doi))

study_citations <- citation_raw %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study_id_value,
         study_id = study_id_value,
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  select(-number) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume))

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017.bib")

## QAQC ######
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("methods", methods)

results <- test_core_relationships(cores, depthseries)

numeric_results <- test_numeric_vars(depthseries)

test_varnames(methods)
test_varnames(cores)
test_varnames(depthseries)

## Write files ####
write_csv(cores, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_cores.csv")
write_csv(methods, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_methods.csv")
write_csv(depthseries, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_study_citations.csv")

