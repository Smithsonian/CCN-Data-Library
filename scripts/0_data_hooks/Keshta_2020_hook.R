# CCRCN Data Library hook script
# contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Soil Redox and Hydropattern control Soil Carbon Stocks across different habitats in Tidal Freshwater Wetlands in a Sub-estuary of the Chesapeake Bay
# Authors: Amr E. Keshta, Stephanie A. Yarwood, and Andrew H. Baldwin.
# DOI: 10.25573/serc.13187549

# load libs
library(tidyverse)
library(RefManageR)
library(lubridate)

# load data
cores_raw <- read.csv("./data/primary_studies/Keshta_et_al_2020/original/Keshta_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Keshta_et_al_2020/original/Keshta_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Keshta_et_al_2020/original/Keshta_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/Keshta_et_al_2020/original/Keshta_et_al_2020_material_and_methods.csv")
study_citations_raw <- read_csv("./data/primary_studies/Keshta_et_al_2020/original/Keshta_et_al_2020_study_citations.csv")

## Trim Data to Library ####

# id <- "Keshta_et_al_2020"

# cores uncontrolled: 
# core_length, habitat (keep habitat, it will be included in V2 guidance)
cores <- cores_raw %>% select(-core_length)

# depthseries uncontrolled: 
# total_carbon_stock, soil_moisture
depthseries <- depthseries_raw %>%
  select(-c(total_carbon_stock, soil_moisture))

# methods (no change)
methods <- methods_raw

# species uncontrolled:
# genus species
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)


## 2. Create Citations ####

study_citations <- study_citations_raw

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  select(-study_id, -bibliography_id) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Keshta_et_al_2020/derivative/Keshta_et_al_2020_study_citations.csv")


