## CCRCN Data Library Hook Script ####

# Data Release: Collection, analysis, and age-dating of sediment cores from a salt marsh platform and ponds, Rowley, Massachusetts, 2014-15
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5fb41153d34eb413d5e0af37
# Contact: Sheron Luk

# Data Citation:
# Luk, S.Y., Spivak, A.C., Eagle, M.J., and O'Keefe-Suttles, J.A., 2020, Collection, analysis, and age-dating of sediment cores from a salt marsh platform and ponds, Rowley, Massachusetts, 2014-15: 
# U.S. Geological Survey data release, https://doi.org/10.5066/P9HIOWKT.

# Associated Pub Citation:
# Luk, S.Y., Todd-Brown, K. Gonneea, M.E., McNichol, A.P., Sanderman, J., Gosselin, K., Spivak, A.C., 2020, Soil organic carbon development and turnover in natural and disturbed salt marsh environments: 
# Geophysical Research Letters, https://doi.org/10.1029/2020GL090287.

## Prep Workspace ####

# load libraries
library(tidyverse)
library(lubridate)
library(RefManageR)
# library(anytime)

# read in data
raw_data <- read_csv("./data/primary_studies/Luk_2020/original/data_PIE_marsh_radioisotope.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Luk_et_al_2020"

# sites
# cores
# depthseries
# species
# methods
# impacts

#### Study Citation ####

data_release_doi <- "10.5066/P9HIOWKT"
pub_doi <- "10.1029/2020GL090287" 

data_bib <- GetBibEntryWithDOI(data_release_doi)
pub_bib <- GetBibEntryWithDOI(pub_doi)

# Convert citations to dataframe
pub_citation <- as.data.frame(pub_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

# double check this citation
data_citation <- as.data.frame(data_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = "Luk_et_al_2020_data") %>%
  mutate(doi = tolower(doi),
         bibliography_id = "Luk_et_al_2020_data",
         key = "Luk_et_al_2020_data")

# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- pub_citation %>%
  bind_rows(data_citation) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  # select(-study_id, -bibliography_id, -publication_type) %>%
  # distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Luk_2020/derivative/Luk_et_al_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("sites", sites)
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_sites.csv")
write_csv(cores, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Luk_et_al_2020/derivative/Luk_et_al_2020_study_citations.csv")


