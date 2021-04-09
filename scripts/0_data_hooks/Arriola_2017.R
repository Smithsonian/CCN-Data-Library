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
  mutate(vegetation_class = "emergent",
         vegetation_notes = ifelse(grepl("snipe_creek_HI", core_id) | grepl("snipe_creek_F", core_id) | grepl("snipe_creek_S", core_id),
                                   "Core collected in emergent vegetation but located near tree hammocks", NA)) %>%
  select(study_id, site_id, core_id, core_year, core_month, core_day, 
         core_latitude, core_longitude, core_position_method, vegetation_class, vegetation_notes)

## Curate depthseries ####
depthseries_curated <- depthseries_raw %>%
  rename(delta_c13 = `δ13C`, fraction_carbon = `OC %`, dry_bulk_density = `dbd_gsed/cc`,
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
         depth_max = as.numeric(depth_max),
         cs137_activity = ifelse(cs137_activity == 0, NA, cs137_activity)
         ) %>% 
  mutate(cs137_unit = ifelse(is.na(cs137_activity), NA, "becquerelsPerKilogram"),
         pb210_unit = ifelse(is.na(excess_pb210_activity), NA, "becquerelsPerKilogram"),
         ra226_unit = ifelse(is.na(ra226_activity), NA, "becquerelsPerKilogram"))

depthseries <- reorderColumns("depthseries", depthseries_curated) %>%
  select(1:17) # Remove all uncontrolled variables
  
methods <- methods_raw %>%
  mutate(study_id = study_id_value) %>%
  select(-age_depth_model_notes)

## Citation ####

if(!file.exists("./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_study_citations.csv")){
  doi <- "https://doi.org/10.1002/lno.10652"
  
  citation_raw <- as.data.frame(GetBibEntryWithDOI(doi))
  
  study_citations <- citation_raw %>%
    mutate(bibliography_id = "Arriola_and_Cable_2017_article",
           study_id = study_id_value,
           publication_type = "associated") %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  ## Format bibliography
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017.bib")
  write_csv(study_citations, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_study_citations.csv")
  
}

## QA/QC ###############

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write files ####
write_csv(cores, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_cores.csv")
write_csv(methods, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_methods.csv")
write_csv(depthseries, "./data/primary_studies/arriola_2017/derivative/arriola_and_cable_2017_depthseries.csv")

