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

if(!file.exists("./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_study_citations.csv")){
  associated_bib_doi <- "10.1002/2015GL066830"
  data_release_doi <- "10.25573/serc.13483332"
  
  paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
  data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
  
  # Convert this to a dataframe
  paper_biblio <- as.data.frame(paper_bib_raw) %>%
    mutate(study_id = id,
           bibliography_id = "Ensign_et_al_2015_article",
           publication_type = "associated") %>%
    remove_rownames()
  
  data_biblio <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = id,
           bibliography_id = "Ensign_et_al_2020_data",
           publication_type = "primary") %>%
    remove_rownames()
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- data_biblio %>%
    bind_rows(paper_biblio) %>%
    mutate(month = ifelse(is.na(month), "jan", month)) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  # write files
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020.bib")
  write_csv(study_citations, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_study_citations.csv")
}


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_cores.csv")
write_csv(methods, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Ensign_et_al_2020/derivative/Ensign_et_al_2020_depthseries.csv")


