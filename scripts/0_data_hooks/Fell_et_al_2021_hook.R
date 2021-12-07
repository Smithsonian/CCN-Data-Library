## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data ingestion

# Figshare data release: https://figshare.com/s/bf5d09fdca0b0ee98bd3

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
fell_cores <- read_csv("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_cores.csv")
fell_ds <- read_csv("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_depthseries.csv")
fell_species <- read_csv("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_species.csv")
fell_methods <- read_csv("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_materials_and_methods.csv")
fell_sites <- read_csv("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_site.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

# id <- "Fell_et_al_2021"

methods <- fell_methods %>% 
  mutate(method_id = "single set of methods") %>% 
  reorderColumns("methods", .)

sites <- fell_sites

names(fell_cores) <- str_remove(names(fell_cores), "core_")
cores <- fell_cores %>% 
  rename(core_id = id, core_length_flag = length_flag) %>% 
  mutate(elevation_method = "RTK",
         habitat = ifelse(vegetation_class == "emergent", "marsh", "scrub/shrub"))

depthseries <- fell_ds %>% 
  mutate(method_id = "single set of methods") %>% 
  reorderColumns("depthseries", .) %>% 
  drop_na(site_id)

species <- fell_species %>% 
  mutate(code_type = "Genus species")

## 2. QAQC ####

table_names <- c("methods", "sites", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

# Read in bibtex files
data_bib <- ReadBib("data/primary_studies/Fell_et_al_2021/original/fell_et_al_2021_citation.bib")

study_citations <- as.data.frame(data_bib) %>%
  mutate(study_id = "Fell_et_al_2021",
         bibliography_id = "Fell_et_al_2021_data",
         publication_type = "primary dataset") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021.bib")
write_csv(study_citations, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_study_citations.csv")

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_cores.csv")
write_csv(depthseries, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_depthseries.csv")
write_csv(methods, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_methods.csv")
write_csv(sites, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_sites.csv")
write_csv(species, "data/primary_studies/Fell_et_al_2021/derivative/Fell_et_al_2021_species.csv")


