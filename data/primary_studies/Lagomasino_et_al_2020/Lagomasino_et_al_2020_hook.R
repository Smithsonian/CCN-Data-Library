# Dataset: Influence of Wind-Driven Inundation and Coastal Geomorphology on Sedimentation in Two Microtidal Marshes, Pamlico River Estuary, NC
# 
# Authors: David  Lagomasino <david.lagomasino@gmail.com>, D. Reide  Corbett, J.P.  Walsh  
# 
# Any use of this dataset must include a citation. The DOI: https://doi.org/10.25573/serc.12043335
# 
# The data release contains marsh soil carbon and dated profiles. The data and metadata is located in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# lagomasino_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# lagomasino_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# lagomasino_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# lagomasino_et_al_2020_species.csv - Contains dominant species information for core and site locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# lagomasino_et_al_2020_associated_publication - Contains bibliographic information for publication associated with this data release

library(tidyverse)
library(RefManageR)
library(lubridate)
# library(anytime)

cores_raw <- read_csv("./data/primary_studies/lagomasino_et_al_2020/original/lagomasino_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/lagomasino_et_al_2020/original/lagomasino_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/lagomasino_et_al_2020/original/lagomasino_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/lagomasino_et_al_2020/original/lagomasino_et_al_2020_materials_and_methods.csv")
associated_pub_raw <- ReadBib("./data/primary_studies/lagomasino_et_al_2020/original/lagomasino_et_al_2020_associated_publication.bib")

cores <- cores_raw %>%
  mutate(core_year = year(core_date),
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK"),
         core_length_flag = "not specified") %>%
  select(-core_date)

methods <- methods_raw %>%
  mutate(method_id = "single set of methods")

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

depthseries <- depthseries_raw %>%
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA),
         method_id = "single set of methods")

## Citations ####

if(!file.exists("data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_study_citations.csv")){
  doi <- "https://doi.org/10.25573/serc.12043335"
  study_id_value <- "Lagomasino_et_al_2020"
  
  associated_pub <- as.data.frame(associated_pub_raw) %>%
    mutate(doi = "10.1007/s12237-013-9625-0",
           bibliography_id = paste0("Lagomasino_et_al_", year, "_article"),
           publication_type = "associated source") 
  
  data_release_raw <- as.data.frame(GetBibEntryWithDOI(doi))
  
  study_citations <- data_release_raw %>%
    mutate(bibliography_id = paste0("Lagomasino_et_al_", year, "_data"),
           publication_type = "primary dataset") %>%
    bind_rows(associated_pub) %>%
    mutate(study_id = study_id_value) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  ## Format bibliography
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  # write citations
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/lagomasino_et_al_2020/derivative/lagomasino_et_al_2020.bib")
  write_csv(study_citations, "./data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_study_citations.csv")
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

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# reorder cols
depthseries <- reorderColumns("depthseries", depthseries)
cores <- reorderColumns("cores", cores)

# test cols and vars
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_depthseries <- test_numeric_vars(depthseries)

# write data
write_csv(methods, "./data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_methods.csv")
write_csv(cores, "./data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_cores.csv")
write_csv(depthseries, "./data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_depthseries.csv")
write_csv(species, "./data/primary_studies/Lagomasino_et_al_2020/derivative/lagomasino_et_al_2020_species.csv")
