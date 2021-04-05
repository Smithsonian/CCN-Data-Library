# Dataset: Carbon data for intertidal soils and sediments, Skeena River estuary, British Columbia
# 
# Authors: Paul Sanborn  <paul.sanborn@unbc.ca>, and Darwyn Coxson.
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.12252005
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can be joined by the core_id and/or site_id attributes. 
# 
# sanborn_and_coxson_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# sanborn_and_coxson_2020_site.csv - Contains positional information on site locations.
# sanborn_and_coxson_2020_cores.csv - Contains positional and descriptive information on core locations.
# sanborn_and_coxson_2020_depthseries.csv - Contains raw depth-series information for all cores.
# sanborn_and_coxson_2020_species.csv - Contains information on the dominant plant species at coring locations.
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

sites_raw <- read_csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_site.csv")
cores_raw <- read.csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_material_and_methods.csv")
study_citations_raw <- read_csv("./data/primary_studies/Sanborn_Coxson_2020/original/sanborn_and_coxson_2020_citations.csv")

## Trim Data to Library ####

study_id_value <- "Sanborn_and_Coxson_2020"

# site uncontrolled:
sites <- sites_raw %>%
  mutate(study_id = study_id_value)

# cores uncontrolled: 
# pH pH_notes carbon_stock carbon_stock_method carbon_stock_0.5m carbon_stock_0.1m
cores <- cores_raw %>%
  mutate(study_id = study_id_value) %>%
  select(-c(pH, pH_notes, carbon_stock, carbon_stock_method, 
            carbon_stock_0.5m, carbon_stock_0.1m))

# depthseries uncontrolled: 
# fraction_carbon_measured fraction_carbon_modeled fraction_nitrogen C_to_N_ratio carbon_stock carbon_density
depthseries <- depthseries_raw %>%
  mutate(study_id = study_id_value) %>%
  rename(fraction_carbon = fraction_carbon_measured) %>%
  drop_na(fraction_carbon) %>%
  select(-c(fraction_carbon_modeled, fraction_nitrogen,
            C_to_N_ratio, carbon_stock, carbon_density))

# methods
methods <- methods_raw %>% 
  mutate(study_id = study_id_value,
         carbon_measured_or_modeled = "measured")

# species uncontrolled:
# family genus species
species <- species_raw %>%
  mutate(study_id = study_id_value,
         species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

## Citations ####
data_doi <- "10.25573/serc.12252005"

study_citations <- study_citations_raw %>%
  select(-key, -keywords, -journal) %>%
  mutate(doi = data_doi,
         publication_type = "primary")

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020.bib")

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries", "species"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries",  "species"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_sites.csv")
write_csv(cores, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_cores.csv")
write_csv(species, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_species.csv")
write_csv(methods, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Sanborn_Coxson_2020/derivative/sanborn_and_coxson_2020_study_citations.csv")


