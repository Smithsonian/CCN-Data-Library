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

cores <- cores_raw %>%
  select(-core_length_flag)

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

impacts <- impacts_raw %>%
  mutate(impact_class = recode(impact_class,
                               "livestock grazing" = "farmed"))

methods <- methods_raw 

depthseries <- depthseries_raw %>%
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA),
         pb210_unit = ifelse(is.na(total_pb210_activity) & is.na(excess_pb210_activity), NA, "becquerelsPerKilogram"),
         ra226_unit = ifelse(!is.na(ra226_activity), "becquerelsPerKilogram", NA)) %>%
  select(-c(slice_thickness, kolker_bulk_density, kolker_pb_inventory, fraction_small_grain)) 

## Citations ####
doi <- "https://doi.org/10.25573/serc.11958996"
study_id_value <- "Nolte_2020"

associated_publication <- as.data.frame(associated_pub_bib) %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study_id_value,
         study_id = study_id_value,
         publication_type = bibtype,
         doi = "10.1016/j.ecss.2013.10.026") %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume))

bib <- GetBibEntryWithDOI(doi)

study_citations <- as.data.frame(bib) %>%
  mutate(year = as.numeric(year),
         study_id = study_id_value,
         bibliography_id = study_id_value,
         publication_type = bibtype,
         key = doi) %>%
  bind_rows(associated_publication)

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Nolte_2020/derivative/nolte_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

write_csv(cores, "./data/primary_studies/nolte_2020/derivative/nolte_2020_cores.csv")
write_csv(species, "./data/primary_studies/nolte_2020/derivative/nolte_2020_species.csv")
write_csv(impacts, "./data/primary_studies/nolte_2020/derivative/nolte_2020_impacts.csv")
write_csv(methods, "./data/primary_studies/nolte_2020/derivative/nolte_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/nolte_2020/derivative/nolte_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/nolte_2020/derivative/nolte_2020_study_citations.csv")

  