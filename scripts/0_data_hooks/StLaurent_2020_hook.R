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
  mutate(method_id = "single set of methods") %>% 
  select(-c(fraction_carbon_modeled, fraction_carbonate, 
            fraction_nitrogen, fraction_hydrogen, fraction_sulfur, porewater_salinity)) %>% 
  
  # QAQC correction: sample IDs are not unique for some replicates. corrected here.
  mutate(sample_id = make.unique(sample_id))

# methods 
methods <- methods_raw %>%
  mutate(fraction_carbon_method = "EA",
         carbon_measured_or_modeled = "measured",
         method_id = "single set of methods")

# species uncontrolled:
# fraction_cover
species <- species_raw %>%
  filter(species != "bare ground" & species != "wrack") %>%
  select(-fraction_cover) %>%
  rename(species_code = species) %>%
  mutate(species_code = trimws(species_code)) %>%
  mutate(species_code = recode(species_code, "Spatina cyanosuriodes" = "Spartina cynosuroides"))

## 2. Create Citation ####

if(!file.exists("./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_study_citations.csv")){
  associated_bib_doi <- "10.1007/s11852-020-00783-3"
  data_release_doi <- "10.25573/serc.13315472"
  
  paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
  data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
  
  # Convert this to a dataframe
  paper_biblio <- as.data.frame(paper_bib_raw) %>%
    mutate(study_id = id) %>%
    mutate(bibliography_id = paste0(id, "_article"),
           publication_type = "associated source")
  
  data_biblio <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = id) %>%
    mutate(bibliography_id = paste0(id, "_data"),
           publication_type = "primary dataset")
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- bind_rows(data_biblio, paper_biblio) %>%
    mutate(month = ifelse(is.na(month), "dec", month)) %>% 
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020.bib")
  write_csv(study_citations, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects

methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## 3. QA/QC ####

source("./scripts/1_data_formatting/qa_functions.R")

# test cols and vars
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/StLaurent_et_al_2020/derivative/StLaurent_et_al_2020_depthseries.csv")


