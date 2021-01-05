## CCRCN Data Library Hook Script ####

# Project: Fate of Coastal Wetland Carbon Under Increasing Sea Level Rise: Using the Subsiding Louisiana Coast as a Proxy for Future World-Wide Sea Level Projections
# Data published by BCO-DMO: https://www.bco-dmo.org/project/670520

# Datasets:
# Carbon mineralization. Contains 9, 1-meter long cores from Barataria Bay, LA coastal wetlands. 
# https://www.bco-dmo.org/dataset/775547
# Chambers, L., Steinmuller, H., Dittmer, K., White, J., Cook, R., Xue, Z. (2019) Barataria Bay carbon mineralization and biogeochemical properties from nine soil cores. 
# Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2019-09-05 [if applicable, indicate subset used]. doi:10.1575/1912/bco-dmo.775547.1 [access date]

# Core biogeochemical properties, 2018-2019. Contains 11, 2-m long cores in the Barataria Bay, LA coastal wetlands and 3 submerged, 0.5 m cores taken in the adjacent estuary.
# https://www.bco-dmo.org/dataset/833824
# White, J. R., Sapkota, Y., Chambers, L. G., Cook, R. L., Xue, Z. (2020) Biogeochemical properties of sediment cores from Barataria Basin, Louisiana, 2018 and 2019. 
# Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2020-12-16 [if applicable, indicate subset used]. doi:10.26008/1912/bco-dmo.833824.1 [access date]


## Prep Workspace ####

# load libraries
library(tidyverse)
library(lubridate)
library(RefManageR)
# library(anytime)

# read in data

# couldn't find a csv for the core biogeochemical properties dataset for some reason
# raw_biogeochem <- read_delim("http://dmoserv3.bco-dmo.org/jg/serv/BCO-DMO/Submerged_Wetland_Carbon/cores_2018_19.flat0?", delim = " ")
# biogeochem <- raw_biogeochem %>% mutate_all(trimws) %>% select(-X25)
# names(biogeochem) <- trimws(names(biogeochem))
# write_csv(biogeochem, "./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_833824.csv")

raw_mineralization <- read_csv("./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_775547_712b_5843_9069.csv")
raw_biogeochem <- read_csv("./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_833824.csv", na = "nd")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Chambers_et_al_2019"



# sites
# cores
# depthseries
# species
# methods
# impacts

#### Study Citation ####

# associated_bib_doi <- ""
data_release_doi_2019 <- "10.1575/1912/bco-dmo.775547.1"
data_release_doi_2020 <- "10.26008/1912/bco-dmo.833824.1"

# paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
data_bib_raw_2019 <- GetBibEntryWithDOI(data_release_doi_2019)
data_bib_raw_2020 <- GetBibEntryWithDOI(data_release_doi_2020)

# Convert citations to dataframe
study_citations_2019 <- as.data.frame(data_bib_raw_2019) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

study_citations_2020 <- as.data.frame(data_bib_raw_2020) %>%
  rownames_to_column("key") %>%
  mutate(study_id = "White_et_al_2020") %>%
  mutate(doi = tolower(doi),
         bibliography_id = "White_et_al_2020",
         key = "White_et_al_2020")

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <-  bind_rows(study_citations_2019, study_citations_2020) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  # select(-study_id, -bibliography_id, -publication_type) %>%
  # distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("sites", sites)
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_sites.csv")
write_csv(cores, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_cores.csv")
write_csv(species, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_species.csv")
write_csv(methods, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_methods.csv")
write_csv(depthseries, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_study_citations.csv")


