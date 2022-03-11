## CCRCN Data Library Hook Script ####
## contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Contributions of Organic and Mineral Matter to Vertical Accretion in Tidal Wetlands across a Chesapeake Bay Subestuary
# Authors: Jenny R. Allen <jennyrallen@gmail.com>, Jeffrey C. Cornwell, and Andrew H. Baldwin.
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.18130892

## DATA INGESTED UNDER GUIDANCE V2.1.1

library(tidyverse)
library(RefManageR)
# library(lubridate)
# library(anytime)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in datasets
raw_cores <- read_csv("./data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_depthseries.csv")
raw_methods <- read_csv("./data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_materials_and_methods.csv")
raw_species <- read_csv("./data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_species.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

# cores
cores <- raw_cores %>% select(-average_salinity) %>% 
  mutate(habitat = "marsh",
         core_id = paste0("NRM_", core_id))

# depthseries
depthseries <- raw_depthseries %>% 
  mutate(method_id = "single set of methods",
         # make core id more unique
         core_id = paste0("NRM_", core_id)) %>% 
  reorderColumns("depthseries", .)

# methods
methods <- raw_methods %>% mutate(method_id = "single set of methods")

# species
species <- raw_species %>% 
  mutate(code_type = ifelse(grepl("spp.", species_code), "Genus", "Genus species"))
  # resolveTaxa(.)

################

## Create citation info 

id <- "Allen_et_al_2022"

# read in bib files
paper_bib_raw <- ReadBib("data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_associated_publication.bib")
data_bib_raw <- ReadBib("data/primary_studies/Allen_et_al_2022/original/allen_et_al_2022_citation.bib")

# Convert this to a dataframe
paper_biblio <- as.data.frame(paper_bib_raw) %>%
  mutate(study_id = id,
         bibliography_id = "Allen_et_al_2021_article",
         publication_type = "associated source") %>%
  remove_rownames()

data_biblio <- as.data.frame(data_bib_raw) %>%
  mutate(study_id = id,
         bibliography_id = "Allen_et_al_2022_data",
         publication_type = "primary dataset") %>%
  remove_rownames()

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_biblio %>%
  bind_rows(paper_biblio) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  column_to_rownames("bibliography_id")

# write files
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022.bib")
write_csv(study_citations, "./data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022_study_citations.csv")

## QA/QC ###############

table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022_cores.csv")
write_csv(methods, "./data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022_methods.csv")
write_csv(depthseries, "./data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022_depthseries.csv")
write_csv(species, "./data/primary_studies/Allen_et_al_2022/derivative/Allen_et_al_2022_species.csv")
