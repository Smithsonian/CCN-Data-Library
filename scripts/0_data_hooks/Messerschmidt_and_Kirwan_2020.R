# Dataset: Soil properties and accretion rates of C3 and C4 marshes at the Global Change Research Wetland, Edgewater, Maryland
# 
# Authors: Messerschmidt, Tyler C. (Virginia Institute of Marine Science); Kirwan, Matthew L. (Virginia Institute of Marine Science)
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11914140
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# messerschmidt_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# messerschmidt_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# messerschmidt_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# messerschmidt_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
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

cores_raw <- read.csv("./data/primary_studies/messerschmidt_2020/original/messerschmidt_and_kirwan_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/messerschmidt_2020/original/messerschmidt_and_kirwan_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/messerschmidt_2020/original/messerschmidt_and_kirwan_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/messerschmidt_2020/original/messerschmidt_and_kirwan_2020_materials_and_methods.csv")

cores <- cores_raw

depthseries <- depthseries_raw %>%
  mutate(cs137_unit = ifelse(is.na(cs137_activity), NA, "disintegrationsPerMinutePerGram"),
         pb210_unit = ifelse(is.na(total_pb210_activity) & is.na(excess_pb210_activity), NA, "disintegrationsPerMinutePerGram"),
         bi214_unit = ifelse(is.na(bi214_activity), NA, "disintegrationsPerMinutePerGram"),
         pb214_unit = ifelse(is.na(pb214_activity_295keV) & is.na(pb214_activity_352keV), NA, "disintegrationsPerMinutePerGram")) %>%
  select(-sedimentation_rate, -sedimentation_rate_se)

methods <- methods_raw

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

## Citations ####
doi <- "https://doi.org/10.25573/serc.11914140"
study_id_value <- "Messerschmidt_and_Kirwan_2020"

bib <- GetBibEntryWithDOI(doi)

study_citations <- as.data.frame(bib) %>%
  mutate(year = as.numeric(year),
         study_id = study_id_value,
         bibliography_id = study_id_value,
         publication_type = bibtype,
         key = doi)

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020.bib")

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)
methods <- reorderColumns("methods", methods)

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

write_csv(cores, "./data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020_cores.csv")
write_csv(species, "./data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020_species.csv")
write_csv(methods, "./data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/messerschmidt_2020/derivative/messerschmidt_and_kirwan_2020_study_citations.csv")
