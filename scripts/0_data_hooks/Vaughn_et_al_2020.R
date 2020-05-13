# Dataset: Increased Organic Carbon Burial in Northern Florida Mangrove-Salt Marsh Transition Zones
# 
# Authors: Derrick Vaughn <dvaughn2@fsu.edu>; Thomas Bianchi; Michael Shields; William  Kenney; Todd Osborne
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.10552004
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# vaughn_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# vaughn_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# vaughn_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# vaughn_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.

## Version 1 of Vaughn et al 2020 hook
library(tidyverse)
library(RefManageR)
library(lubridate)

cores_raw <- read.csv("./data/primary_studies/Vaughn_et_al_2020/original/vaughn_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Vaughn_et_al_2020/original/vaughn_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Vaughn_et_al_2020/original/vaughn_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/Vaughn_et_al_2020/original/vaughn_et_al_2020_materials_and_methods.csv")

cores <- cores_raw %>%
  mutate(core_date = as_date(core_date)) %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date))

depthseries <- depthseries_raw %>%
  select(study_id:CRS_age_se) %>%
  rename(age = CRS_model_age,
         age_se = CRS_age_se)

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

methods <- methods_raw %>%
  mutate(excess_pb210_model = "CRS") %>%
  rename(c14_counting_method = cs14_counting_method)

## Citations
doi <- "10.25573/serc.10552004"
study_id_value <- "Vaughn_et_al_2020"


bib <- GetBibEntryWithDOI(doi)

study_citations <- as.data.frame(bib) %>%
  mutate(year = as.numeric(year),
         study_id = study_id_value,
         bibliography_id = study_id_value,
         publication_type = bibtype,
         key = doi)

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Vaughn_et_al_2020/derivative/Vaughn_et_al_2020.bib")

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)
methods <- reorderColumns("methods", methods)
cores <- reorderColumns("cores", cores)

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

write_csv(cores, "./data/primary_studies/Vaughn_et_al_2020/derivative/vaughn_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Vaughn_et_al_2020/derivative/vaughn_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Vaughn_et_al_2020/derivative/vaughn_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Vaughn_et_al_2020/derivative/vaughn_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Vaughn_et_al_2020/derivative/vaughn_et_al_2020_study_citations.csv")
