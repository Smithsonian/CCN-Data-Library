# Dataset: Carbon stocks in seagrass meadows, emergent marshes, and forested tidal swamps of the Pacific Northwest
# 
# Authors: J Boone Kauffman, Leila Giovannoni, Kelly J, Dunstan N, Amy Borde, Heida Diefenderfer, Craig Cornu, Christopher Janousek <janousec@oregonstate.edu>, Jude Apple, and Laura Brophy.
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
cores_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_material_and_methods.csv")
study_citations_raw <- read_csv("./data/primary_studies/Kauffman_et_al_2020/original/kauffman_et_al_2020_study_citations.csv")

## Trim Data to Library ####

study_id_value <- "Kauffman_et_al_2020"

# site 
sites <- sites_raw

# cores uncontrolled: 
# peat_depth, peat_depth_notes, core_length, pH, porewater_salinity
cores <- cores_raw %>%
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK"),
         # patch in missing Blind Slough coords for one core
         core_latitude = ifelse(core_id == "COL-BLI-TS-2", 46.1922, core_latitude),
         core_longitude = ifelse(core_id == "COL-BLI-TS-2", -123.5798, core_longitude),
         core_position_method = ifelse(core_id == "COL-BLI-TS-2", "other low resolution", core_position_method),
         core_position_notes = ifelse(core_id == "COL-BLI-TS-2", "site-level coordinates from associated publication", core_position_notes),
         habitat = case_when(vegetation_class == "emergent" ~ "marsh",
                             vegetation_class == "seagrass" ~ "seagrass",
                             vegetation_class == "forested" ~ "swamp")) %>%
  select(-c(peat_depth, peat_depth_notes, core_length, pH, porewater_salinity))

# depthseries
depthseries <- depthseries_raw %>% mutate(method_id = "single set of methods")

# methods 
methods <- methods_raw %>% mutate(method_id = "single set of methods") %>%
  select(study_id, method_id, everything())

# species uncontrolled:
# genus species
species <- species_raw %>%
  select(-species_code) %>%
  mutate(study_id = study_id_value,
         species_code = trimws(paste(genus, species, sep=" "))) %>%
  select(study_id, site_id, core_id, species_code)

## Create citation info  #######
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
  mutate(publication_type = recode(publication_type,
                                   "primary" = "primary dataset",
                                   "associated" = "associated source"))

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Kauffman_et_al_2020/derivative/Kauffman_et_al_2020.bib")
write_csv(study_citations, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_study_citations.csv")


# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects
sites <- updated$sites
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
testIDs(cores, depthseries, by = "core")

fraction_not_percent(depthseries)
test_depthseries <- test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(sites, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_sites.csv")
write_csv(cores, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/Kauffman_et_al_2020/derivative/kauffman_et_al_2020_depthseries.csv")


