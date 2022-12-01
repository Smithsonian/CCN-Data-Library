## CCRCN Data Library Hook Script ####
## contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Sedimentary organic carbon measurements in a restored coastal wetland in San Francisco Bay, CA, USA
# Authors: Joseph Carlin <jcarlin@fullerton.edu>, Patty Oikawa, Ariane Arias-Ortiz, Sadie Kanneg, Theresa Duncan, Katya Beener.
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.16416684

## DATA INGESTED UNDER GUIDANCE V2.1.1

library(tidyverse)
library(RefManageR)
# library(lubridate)
# library(anytime)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in datasets
raw_cores <- read_csv("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_depthseries.csv")
raw_methods <- read_csv("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_material_and_methods.csv")
raw_impacts <- read_csv("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_impacts.csv")
raw_species <- read_csv("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_species.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

# cores
cores <- raw_cores %>%
  rename(year = core_year, month = core_month, day = core_day, 
         latitude = core_latitude, longitude = core_longitude,
         position_method = core_position_method, elevation_notes = core_elevation_notes)

# depthseries
depthseries <- raw_depthseries %>% 
  mutate(method_id = "single set of studies",
         cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA_character_), 
         pb210_unit = ifelse(!is.na(total_pb210_activity), "becquerelsPerKilogram", NA_character_),
         ra226_unit = ifelse(!is.na(ra226_activity), "becquerelsPerKilogram", NA_character_)) %>%
  select(-c(fraction_nitrogen, fraction_sand, fraction_silt, fraction_clay)) 

# methods
# ra226_counting_method not in guidance
methods <- raw_methods %>% 
  mutate(method_id = "single set of studies",
         excess_pb210_model = "CRS",
         age_depth_model_notes = "PLUM software used") %>% 
  select(-ra226_counting_method) 

# impacts (no change)
impacts <- raw_impacts 

# species
species <- raw_species %>% 
  mutate(code_type = "Genus species") 

################

## Create citation info 

id <- "Carlin_et_al_2021"

# if(!file.exists("./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_study_citations.csv")){
# associated_bib_doi <- "" # has to be added once the paper is published
data_release_doi <- "10.25573/serc.16416684"
data_bib_raw <- GetBibEntryWithDOI(data_release_doi)

paper_bib_raw <- ReadBib("./data/primary_studies/Carlin_et_al_2021/original/carlin_et_al_2021_associated_publication.bib")

# Convert this to a dataframe
paper_biblio <- as.data.frame(paper_bib_raw) %>%
  mutate(study_id = id,
         bibliography_id = "Arias-Ortiz_et_al_2021_article",
         publication_type = "associated source") %>%
  remove_rownames()

data_biblio <- as.data.frame(data_bib_raw) %>%
  mutate(study_id = id,
         bibliography_id = "Carlin_et_al_2021_data",
         publication_type = "primary dataset") %>%
  remove_rownames()

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_biblio %>%
  bind_rows(paper_biblio) %>%
  # mutate(month = ifelse(is.na(month), "jan", month)) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  column_to_rownames("bibliography_id")

# write files
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021.bib")
write_csv(study_citations, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_study_citations.csv")
# }


## QA/QC ###############

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

testRequired(table_names)
testConditional(table_names)

testTaxa(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)

testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

fractionNotPercent(depthseries)
results <- testNumericCols(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_cores.csv")
write_csv(methods, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_methods.csv")
write_csv(depthseries, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_depthseries.csv")
write_csv(impacts, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_impacts.csv")
write_csv(species, "./data/primary_studies/Carlin_et_al_2021/derivative/Carlin_et_al_2021_species.csv")


