## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for Burden et al 2018
## Information about the dataset (i.e. title, authors, citation, DOI)

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_data_2010_2017 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2010_2017.csv")
raw_data_2011 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2011.csv")
# locations extracted from the .rtf file
site_info <- read_csv("data/primary_studies/Burden_et_al_2018/intermediate/Burden_2018_site_locations.csv")
raw_methods <- read_csv("data/primary_studies/Burden_et_al_2018/intermediate/Burden_2018_materials_and_methods.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Burden_et_al_2018"

## ... Methods ####

sample_volume <- pi*(2^2)*30

# curate materials and methods
# methods <- raw_methods


## ... Core Depthseries ####

ds_2010_2017 <- raw_data_2010_2017 %>%
  mutate(year = ifelse(SiteName == "Tollesbury Field", 2010, 2017),
         month = ifelse(SiteName == "Tollesbury Field", 7, 4))

ds_2011 <- raw_data_2011 %>%
  mutate(year = 2011, month = 10)
  
# curate depthseries-level data
depthseries <- bind_rows(ds_2010_2017, ds_2011) %>% 
  rename(site_id = SiteName,
         core_id = SiteCode,
         dry_bulk_density = BulkDensity) %>% 
  mutate(study_id = id,
         depth_min = 0,
         depth_max = 30,
         fraction_organic_matter = OrganicMatter/100,
         fraction_carbon = Carbon/100)

## ... Core-Level ####

# curate core-level data
cores <- depthseries %>% 
  distinct(study_id, site_id, core_id) %>% 
  left_join(site_info) %>% # 	Barrow Hill Field doesn't exist in the depthseries
  mutate(elevation = 1.5,
         elevation_datum = "OD")

## ... Impacts ####

# there is natural salt marsh, restored and accidentally breached, and fields on former salt marsh 

## 2. QAQC ####

table_names <- c("methods", "cores", "depthseries")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

# if(!file.exists("data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_study_citations.csv")){
  # Create bibtex file
  data_release_doi <- '10.5285/0b1faab4-3539-457f-9169-b0b1fbd59bc2'
  
  data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
  
  study_citations <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = id,
           bibliography_id = "Burden_et_al_2018_data",
           publication_type = "primary dataset") %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  # WriteBib(as.BibEntry(bib_file), "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018.bib")
  # write_csv(study_citations, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_cores.csv") 
write_csv(depthseries, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_depthseries.csv")
write_csv(methods, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_methods.csv")
# write_csv(sites, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_sites.csv")
# write_csv(species, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_species.csv")
# write_csv(impacts, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_impacts.csv")


