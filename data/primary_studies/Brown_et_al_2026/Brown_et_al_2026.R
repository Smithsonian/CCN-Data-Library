## CCRCN Data Library ########
## contact: James Holmquist, HolmquistJ@si.edu 

## Hook script for Brown et al., 2026, Coastal California
## Information about the dataset (i.e. title, authors, citation, DOI)

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data by inserting the path to the dataset
# if you opened this Rstudio session from the CCRCN-Data-Library.Rproj
# the working directory will start from the CCRCN-Data-Library folder
# Tip: use tab to autocomplete the file path
raw_cores <- read_csv("data/primary_studies/Brown_et_al_2026/original/brown_et_al_2026_cores.csv")
raw_methods <- read_csv("data/primary_studies/Brown_et_al_2026/original/brown_et_al_2026_materials_and_methods.csv")
raw_depthseries <- read_csv("data/primary_studies/Brown_et_al_2026/original/brown_et_al_2026_depthseries.csv")
raw_species <- read_csv("data/primary_studies/Brown_et_al_2026/original/brown_et_al_2026_species.csv")
raw_bib <- read_csv("data/primary_studies/Brown_et_al_2026/original/b")


# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

# NOTE: Disregard the following if this is a synthesis study
# Define a study ID (if one hasn't already been assigned via data release)
# include this id in a study_id column of every table
# if there are only two authors: Author_and_Author_year

id <- "Brown_et_al_2026"

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>% 
  mutate(method_id = "Single set of methods")

## ... Core-Level ####

# curate core-level data
cores <- raw_cores %>% 
  filter(! core_id %in% c("BOL13-05", "PTL15-02")) %>% 
  mutate(habitat = "marsh")

## ... Core Depthseries ####

# curate depthseries-level data
depthseries <- raw_depthseries %>% 
  select(-c(fraction_modern, fraction_modern_se,
            delta_c14, delta_c14_se, am241_activity, am241_activity_se)) %>% 
  mutate(method_id = "Single set of methods",
         cs137_unit = "becquerelsPerKilogram",
         pb210_unit = "becquerelsPerKilogram",
         ra226_unit = "becquerelsPerKilogram"
         )

# The following tables are optional:
## ... Sites ####
## ... Species ####
species <- raw_species %>% 
  mutate(code_type = "Genus species")

## ... Impacts ####

## 2. QAQC ####

table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- testNumericCols(depthseries)

## 3. Study Citations ####

raw_bib <- ReadBib("data/primary_studies/Brown_et_al_2026/original/brown_et_al_2026_associated_publications.bib")

old_citations <- as.data.frame(raw_bib) %>%
  mutate(study_id = id,
         bibliography_id = c("Brown_2019_dissertation", "Fard_et_al_2021_article",
                             "Thorne_et_al_2016_report"
                             ),
         publication_type = "associated source"
         # publication_type = c("primary dataset", "associated source"
         ) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()


# Use RefManageR package to pull DOI
dois <- c("10.25573/serc.28672772",
          "10.1038/s41597-026-06935-8")

new_citations_bib <- GetBibEntryWithDOI(dois)

all_citations <- as.data.frame(new_citations_bib) %>% 
  mutate(study_id = id,
         bibliography_id = c("Brown_et_al_2026_data_release", "Holmquist_et_al_2026_article"),
         publication_type = c("primary dataset", "associated source")
  ) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames() %>% 
  bind_rows(old_citations)
  
# Write .bib file
bib_file <- all_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")
  
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026.bib")
write_csv(study_citations, "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026_study_citations.csv")


## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026_cores.csv") 
write_csv(depthseries, "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026_depthseries.csv")
write_csv(methods, "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026_methods.csv")
write_csv(species, "data/primary_studies/Brown_et_al_2026/derivative/Brown_et_al_2026_species.csv")


