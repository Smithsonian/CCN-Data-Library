## CCRCN Data Library Hook Script ####

# Data Release: Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5f7e18d382ce1d74e7dda178
# Contact: Sarai Piazza

# Data Citation:
# Piazza, S.C., Steyer, G.D., Cretini, K.F., Sasser, C.E., Visser, J.M., Holm, G.O., Sharp, L.A., Evers, E., and Meriwether, J.R., 2021, 
# Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities: U.S. Geological Survey data release, 
# https://doi.org/10.5066/P9D8WTQW.

# Report Citation:
# Piazza, S.C., Steyer, G.D., Cretini, K.F., Sasser, C.E., Visser, J.M., Holm, G.O., Jr., Sharp, L.A., Evers, D.E., and
# Meriwether, J.R., 2011, Geomorphic and ecological effects of Hurricanes Katrina and Rita on coastal Louisiana marsh communities:
# U. S. Geological Survey Open-File Report 2011-1094, 126 p.

## Prep Workspace ####

# load libraries
library(tidyverse)
# library(lubridate)
library(RefManageR)
# library(anytime)

# read in data
raw_sites <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_site.csv")
raw_cores <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_depthseries.csv")
raw_species <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_species.csv")
raw_impacts <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_impacts.csv")
raw_methods <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_material_and_methods.csv")
# study_citations <- read_csv("./data/primary_studies/Piazza_2020/original/piazza_et_al_2020_study_citations.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Piazza_et_al_2020"

# sites
sites <- raw_sites

# cores
cores <- raw_cores

# depthseries
# uncontrolled: fraction_nitrogen
depthseries <- raw_depthseries %>% select(-fraction_nitrogen)

# species
species <- raw_species %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

# methods
methods <- raw_methods

# impacts
impacts <- raw_impacts

#### Study Citation ####

data_release_doi <- "10.5066/P9D8WTQW"
# report_doi <- "" # what about the report citation

data_bib <- GetBibEntryWithDOI(data_release_doi)

# Convert citations to dataframe
data_citation <- as.data.frame(data_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

# report_citation <- as.data.frame(report_bib) %>%
#   rownames_to_column("key") %>%
#   mutate(study_id = "White_et_al_2020") %>%
#   mutate(doi = tolower(doi),
#          bibliography_id = "White_et_al_2020",
#          key = "White_et_al_2020")
# 
# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_citation %>%
  # bind_rows(report_citation) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  # select(-study_id, -bibliography_id, -publication_type) %>%
  # distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("sites", sites)
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)
test_colnames("impacts", impacts)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_sites.csv")
write_csv(cores, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Piazza_2020/derivative/Piazza_et_al_2020_study_citations.csv")


