# CCRCN Data Library hook script
# contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Assessing coastal carbon variability in two Delaware tidal marshes
# Authors: Kari A. St. Laurent, Daniel J. Hribar, Annette J. Carlson, Calyn M. Crawford, Drexel Siok 
# DOI: 10.25573/serc.13315472

# load libs
library(tidyverse)
library(RefManageR)
library(lubridate)

# load data
cores_raw <- read.csv("./data/primary_studies/StLaurent_et_al_2020/original/StLaurent_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/StLaurent_et_al_2020/original/StLaurent_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/StLaurent_et_al_2020/original/StLaurent_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/StLaurent_et_al_2020/original/StLaurent_et_al_2020_material_and_methods.csv")
# study_citations_raw <- read_csv("./data/primary_studies/StLaurent_et_al_2020/original/StLaurent_et_al_2020_study_citations.csv")

## 1. Trim Data to Library ####

id <- "StLaurent_et_al_2020"

# cores (no change)
cores <- cores_raw

# depthseries uncontrolled: 
# fraction_carbon_measured fraction_carbon_modeled fraction_carbonate fraction_nitrogen 
# fraction_hydrogen fraction_sulfur porewater_salinity
depthseries <- depthseries_raw %>%
  rename(fraction_carbon = fraction_carbon_measured) %>%
  select(-c(fraction_carbon_modeled, fraction_carbonate, 
            fraction_nitrogen, fraction_hydrogen, fraction_sulfur, porewater_salinity))

# methods (no change)
methods <- methods_raw

# species uncontrolled:
# fraction_cover
species <- species_raw %>%
  filter(species != "bare ground" & species != "wrack") %>%
  select(-fraction_cover) %>%
  rename(species_code = species)

## 2. Create Citation ####

associated_bib_doi <- "10.1007/s11852-020-00783-3"
data_release_doi <- "10.25573/serc.13315472"

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
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_biblio %>%
  bind_rows(paper_biblio) %>%
  mutate(publication_type = bibtype) %>%
  mutate(key = c("StLaurent_et_al_2020_data", "StLaurent_et_al_2020")) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         month = ifelse(is.na(month), "dec", month))

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020.bib")


## 3. QA/QC ####

source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_study_citations.csv")


