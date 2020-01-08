# Dataset: Temporal variability of carbon and nutrient burial, sedient accretion, and mass accumulation over the past century in a carbonate platform mangrove forest of the Florida Everglades
# 
# Authors: Joshua L. Breithaupt  <Josh.Breithaupt@gmail.com>, Joseph M. Smoak, Christian J. Sanders, and Thomas J. Smith, III
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11310926
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# breithaupt_et_al_2014_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# breithaupt_et_al_2014_cores.csv - Contains positional and descriptive information on core locations.
# breithaupt_et_al_2014_depthseries.csv - Contains raw depth-series information for all cores.
# breithaupt_et_al_2014_species.csv - Contains information on the dominant plant species at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# citations.bib - Citation for associated journal article and this data release in .BIB format. 

library(tidyverse)
library(RefManageR)
library(lubridate)

cores_raw <- read_csv("./data/primary_studies/breithaupt_2014/original/breithaupt_et_al_2014_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/breithaupt_2014/original/breithaupt_et_al_2014_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/breithaupt_2014/original/breithaupt_et_al_2014_species.csv")
methods_raw <- read_csv("./data/primary_studies/breithaupt_2014/original/breithaupt_et_al_2014_materials_and_methods.csv")
bib <- ReadBib("./data/primary_studies/breithaupt_2014/original/citations.bib")

# Breithaupt et al 2014 already exists from seagrass synthesis
study_id_value <- "Breithaupt_et_al_2014b"

cores <- cores_raw %>%
  mutate(core_year = year(as_date(core_date, format="%m/%d/%Y", tz="UTC")),
         core_date = as_date(core_date, format="%m/%d/%Y", tz="UTC"),
         study_id = study_id_value) %>%
  select(study_id, site_id, core_id, core_year, core_date, everything()) 

# Species data needs to combine genus and species into one and remove other columns
species <- species_raw %>%
  mutate(study_id = study_id_value,
         species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

depthseries <- depthseries_raw %>%
  mutate(study_id = study_id_value) %>%
  select(-c(cs137_MDA, total_pb210_MDA, ra226_MDA, excess_pb210_inventory,
            excess_pb210_inventory_se, fraction_total_nitrogen, fraction_total_phosphorus,
            delta_N15, gamma_counting_sedmass, cumulative_sedmass_atdepth))

methods <- methods_raw %>%
  mutate(study_id = study_id_value) 

study_citations <- as.data.frame(bib) %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = key,
         study_id = study_id_value,
         bibliography_id = study_id_value,
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume))

bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014.bib")

write_csv(cores, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_cores.csv") 
write_csv(depthseries, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_depthseries.csv")
write_csv(study_citations, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_study_citations.csv")
write_csv(species, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_species.csv")
write_csv(methods, "./data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_methods.csv")
