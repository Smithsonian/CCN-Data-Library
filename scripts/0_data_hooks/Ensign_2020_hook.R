## CCRCN Data Library Hook Script ####
## contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Head of tide bottleneck of particulate material transport from watersheds to estuaries
# 
# Authors: Scott H. Ensign, Gregory B. Noe, Cliff R. Hupp, and Katherine J. Skalak.
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.13483332


library(tidyverse)
library(RefManageR)
# library(lubridate)
# library(anytime)

raw_cores <- read_csv("./data/primary_studies/Ensign_et_al_2020/original/Ensign_et_al_2020_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Ensign_et_al_2020/original/Ensign_et_al_2020_depthseries.csv")
raw_methods <- read_csv("./data/primary_studies/Ensign_et_al_2020/original/Ensign_et_al_2020_material_and_methods.csv")

## Trim Data to Library ####

id <- "Ensign_et_al_2020"

# cores (no change)
cores <- raw_cores

# depthseries (no change)
depthseries <- raw_depthseries

# methods (no change)
methods <- raw_methods

################
## Create citation info  
associated_bib_doi <- "10.1002/2015GL066830"
data_release_doi <- "10.25573/serc.13483332"

paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
data_bib_raw <- GetBibEntryWithDOI(data_release_doi)

# Convert this to a dataframe
paper_biblio <- as.data.frame(paper_bib_raw) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

data_biblio <- as.data.frame(data_bib_raw) %>%
  rownames_to_column("key") %>%
  mutate(study_id = str_c(id, "_data")) %>%
  mutate(doi = tolower(doi),
         bibliography_id = str_c(id, "_data"),
         key = str_c(id, "_data"))

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_biblio %>%
  bind_rows(paper_biblio) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(month = ifelse(is.na(month), "jan", month))

# study_citations <- study_citations_raw %>%
#   mutate(key = c("Ensign_et_al_2020_data", "Ensign_et_al_2020"))

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("methods", methods)

test_varnames(cores)
test_varnames(methods)

test_core_relationships(cores, depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_cores.csv")
write_csv(methods, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_study_citations.csv")


