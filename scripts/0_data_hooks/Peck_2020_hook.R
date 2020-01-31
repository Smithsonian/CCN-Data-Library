# Dataset: Controls on sediment accretion and blue carbon burial in tidal saline wetlands: Insights from the Oregon coast, U.S.A.
# 
# Authors: Erin Peck <peckerin@oregonstate.edu>; Robert Wheatcroft;  Laura Brophy 
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11317820
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# peck_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# peck_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# peck_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# peck_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
# peck_et_al_2020_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
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
library(anytime)

cores_raw <- read.csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_materials_and_methods.csv")
impacts_raw <- read_csv("./data/primary_studies/peck_2020/original/peck_et_al_2020_impacts.csv")

cores <- cores_raw %>%
  separate(core_date, into=c("month", "day", "core_year"), sep="/") %>%
  mutate(core_year = paste0("20", core_year)) %>%
  mutate(core_date = anydate(paste(core_year, month, day, sep="/")),
         core_notes = ifelse(!is.na(core_notes),
                             paste0("Abbreviated core ID: ", abbreviated_core_id, ". ", core_notes),
                             paste0("Abbreviated core ID: ", abbreviated_core_id))) %>%
  select(-month, -day, -abbreviated_core_id) %>%
  select(study_id, site_id, core_id, core_year, core_date, everything())

depthseries <- depthseries_raw %>%
  select(-fraction_carbon_modeled, -dry_bulk_density_modeled) %>%
  rename(fraction_carbon = fraction_carbon_measured,
         dry_bulk_density = dry_bulk_density_measured) %>%
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         pb214_unit = ifelse(!is.na(pb214_activity), "disintegrationsPerMinutePerGram", NA), 
         cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA))

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

peck_doi <- "https://doi.org/10.25573/serc.11317820.v2"

citation_raw <- as.data.frame(GetBibEntryWithDOI(peck_doi))

study_citations <- citation_raw %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = "Peck_et_al_2020",
         study_id = "Peck_et_al_2020",
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year))

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/peck_2020/derivative/peck_et_al_2020.bib")

write_csv(cores, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_species.csv")
write_csv(methods_raw, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/peck_2020/derivative/peck_et_al_2020_study_citations.csv")


