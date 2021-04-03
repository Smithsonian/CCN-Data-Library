# Dataset: Accretion rates of low intertidal salt marshes in the Pacific Northwest
# 
# Authors: Ronald M. Thom <thom.ronald@gmail.com>
#   
#   Any use of this dataset must include a citation. The DOI: 10.25573/data.10046189
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# thom_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# thom_2019_cores.csv - Contains positional and descriptive information on core locations.
# thom_2019_depthseries.csv - Contains raw depth-series information for all cores.
# thom_2019_species.csv - Contains information on the dominant plant species at coring locations.
# thom_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# thom_2019_associated_publication.bib - A citation in bibtex style for associated publications to this data release

library(tidyverse)
library(RefManageR)

depthseries_raw <- read.csv("./data/primary_studies/Thom_2019/original/thom_2019_depthseries.csv")
methods_raw <-read_csv("./data/primary_studies/Thom_2019/original/thom_2019_material_and_methods.csv")
species_raw <- read_csv("./data/primary_studies/Thom_2019/original/thom_2019_species.csv")
cores_raw <- read_csv("./data/primary_studies/Thom_2019/original/thom_2019_cores.csv")
  
# hook data
cores <- cores_raw

depthseries <- depthseries_raw %>%
  mutate(cs137_unit = "countsPerGramDryWeightPerHour")

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(-c(genus, species))

methods <- methods_raw %>%
  mutate(study_id = "Thom_1992")

# Create bibtex file
data_release_doi <- "10.25573/data.10046189"
associated_pub_doi <- "10.1007/BF03160603"
study_id <- "Thom_1992"

data_bib_raw <- GetBibEntryWithDOI(c(data_release_doi, associated_pub_doi))

study_citations <- as.data.frame(data_bib_raw) %>%
  mutate(study_id = study_id) %>%
  mutate(bibliography_id = c("Thom_1992_data", "Thom_1992_article"),
         publication_type = c("primary", "associated")) %>%
  remove_rownames() %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Curate biblio so ready to read out as a BibTex-style .bib file
# study_citations <- bib %>%
#   select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
#   mutate(year = as.numeric(year),
#          volume = as.numeric(volume),
#          number = as.numeric(number))

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Thom_2019/derivative/Thom_2019.bib")

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries", "species"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries", "species"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

# write files
write_csv(depthseries, "data/primary_studies/Thom_2019/derivative/thom_2019_depthseries.csv")
write_csv(cores, "data/primary_studies/Thom_2019/derivative/thom_2019_cores.csv")
write_csv(methods, "data/primary_studies/Thom_2019/derivative/thom_2019_methods.csv")
write_csv(study_citations, "data/primary_studies/Thom_2019/derivative/thom_2019_study_citations.csv")
write_csv(species, "data/primary_studies/Thom_2019/derivative/thom_2019_species.csv")
