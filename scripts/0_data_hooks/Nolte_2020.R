# Dataset: Does livestock grazing affect sediment deposition and accretion rates in salt marshes?
#   
#   Author: Stefanie  Nolte  <s.nolte@uea.ac.uk>
#   
#   Any use of this dataset must include a citation. The DOI: 10.25573/serc.11958996
# 
# The data release contains wetland soil carbon and dated profiles. The data and metadata is located in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# nolte_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# nolte_2020_cores.csv - Contains positional and descriptive information on core locations.
# nolte_2020_depthseries.csv - Contains raw depth-series information for all cores.
# nolte_2020_species.csv - Contains dominant species information for all site and core locations
# nolte_2020_impacts.csv - Contains anthropogenic impact information for all site and core locations
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# associated_publications.bib - BibTeX style bibliography of publications associated with this dataset

library(tidyverse)
library(RefManageR)
library(lubridate)

cores_raw <- read.csv("./data/primary_studies/nolte_2020/original/nolte_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/nolte_2020/original/nolte_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/nolte_2020/original/nolte_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/nolte_2020/original/nolte_2020_materials_and_methods.csv")
impacts_raw <- read_csv("./data/primary_studies/nolte_2020/original/nolte_2020_impacts.csv")
associated_pub_bib <- ReadBib("./data/primary_studies/nolte_2020/original/associated_publications.bib")

cores <- cores_raw 

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

impacts <- impacts_raw %>%
  mutate(impact_class = recode(impact_class,
                               "livestock grazing" = "farmed"))

methods <- methods_raw %>% mutate(method_id = "single set of methods")

depthseries <- depthseries_raw %>%
  mutate(method_id = "single set of methods",
         cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA),
         pb210_unit = ifelse(is.na(total_pb210_activity) & is.na(excess_pb210_activity), NA, "becquerelsPerKilogram"),
         ra226_unit = ifelse(!is.na(ra226_activity), "becquerelsPerKilogram", NA)) %>%
  select(-c(slice_thickness, kolker_bulk_density, kolker_pb_inventory, fraction_small_grain)) 

## Citations ####

if(!file.exists("data/primary_studies/nolte_2020/derivative/nolte_2020_study_citations.csv")){
  # doi <- "https://doi.org/10.25573/serc.11958996"
  study_id_value <- "Nolte_2020"
  
  data_bib <- GetBibEntryWithDOI("https://doi.org/10.25573/serc.11958996")
  pub_bib <- GetBibEntryWithDOI("10.1016/j.ecss.2013.10.026")
  
  associated_publication <- as.data.frame(pub_bib) %>%
    mutate(bibliography_id = paste0("Nolte_et_al_", year, "_article"),
           study_id = study_id_value,
           publication_type = "associated source")
  # mutate(year = as.numeric(year),
  #        volume = as.numeric(volume))
  
  data_citation <- as.data.frame(data_bib) %>%
    mutate(bibliography_id = paste0("Nolte_", year, "_data"),
           study_id = study_id_value,
           publication_type = "primary dataset")
  
  study_citations <- bind_rows(data_citation, associated_publication) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  ## Format bibliography
  bib_file <- study_citations %>%
    select(-study_id,  -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Nolte_2020/derivative/nolte_2020.bib")
  write_csv(study_citations, "./data/primary_studies/nolte_2020/derivative/nolte_2020_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

updated <- updateTables(table_names)

# save listed tables to objects

impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
result <- test_numeric_vars(depthseries)

# write data to derivative folder
write_csv(cores, "./data/primary_studies/nolte_2020/derivative/nolte_2020_cores.csv")
write_csv(species, "./data/primary_studies/nolte_2020/derivative/nolte_2020_species.csv")
write_csv(impacts, "./data/primary_studies/nolte_2020/derivative/nolte_2020_impacts.csv")
write_csv(methods, "./data/primary_studies/nolte_2020/derivative/nolte_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/nolte_2020/derivative/nolte_2020_depthseries.csv")

  