# Dataset: Carbon stocks in seagrass meadows, emergent marshes, and forested tidal swamps of the Pacific Northwest
# 
# Authors: J Boone Kauffman, Leila Giovanonni, Kelly J, Dunstan N, Amy Borde, Heida Diefenderfer, Craig Cornu, Christopher Janousek <janousec@oregonstate.edu>, Jude Apple, and Laura Brophy.
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.12640172
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can be joined by the core_id and/or site_id attributes. 
# 
# kauffman_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# kauffman_et_al_2020_site.csv - Contains positional information on site locations.
# kauffman_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# kauffman_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# kauffman_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.

library(tidyverse)
library(RefManageR)
library(lubridate)
# library(anytime)

sites_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_site.csv")
cores_raw <- read.csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_material_and_methods.csv")
study_citations_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_study_citations.csv")

## Trim Data to Library ####

study_id_value <- "Kauffman_et_al_2020"

# site (no change):
sites <- sites_raw

# cores uncontrolled: 
# peat_depth, peat_depth_notes, core_length, pH, porewater_salinity
cores <- cores_raw %>%
  select(-c(peat_depth, peat_depth_notes, core_length, pH, porewater_salinity))

# depthseries uncontrolled: 
depthseries <- depthseries_raw

# methods (no change)
methods <- methods_raw

# species uncontrolled:
# genus species
species <- species_raw %>%
  select(-species_code) %>%
  mutate(study_id = study_id_value,
         species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)


################
## Create citation info  
# associated_bib_doi <- "10.1111/gcb.15248"
# data_release_doi <- "10.25573/serc.12640172"
# 
# paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
# data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
# 
# # Convert this to a dataframe
# paper_biblio <- as.data.frame(paper_bib_raw) %>%
#   rownames_to_column("key") %>%
#   mutate(study_id = study_id_value) %>%
#   mutate(doi = tolower(doi),
#          bibliography_id = study_id_value,
#          key = study_id_value) 
# 
# data_biblio <- as.data.frame(data_bib_raw) %>%
#   rownames_to_column("key") %>%
#   mutate(study_id = study_id_value) %>%
#   mutate(doi = tolower(doi),
#          bibliography_id = study_id_value,
#          key = study_id_value) 
# 
# # Curate biblio so ready to read out as a BibTex-style .bib file
# study_citations <- data_biblio %>%
#   bind_rows(paper_biblio) %>%
#   mutate(publication_type = bibtype) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
#   mutate(year = as.numeric(year),
#          month = "aug")

study_citations <- study_citations_raw %>%
  mutate(key = c("Kauffman_et_al_2020_data", "Kauffman_et_al_2020"))

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Kauffman_et_al_2020/derivative/Kauffman_et_al_2020.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("sites", sites)
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_sites.csv")
write_csv(cores, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_study_citations.csv")

