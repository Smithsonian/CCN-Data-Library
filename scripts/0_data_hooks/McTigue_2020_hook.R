# Dataset: Carbon accumulation rates in a salt marsh over the past two millennia
# 
# Authors: Nathan  McTigue  <mctigue@utexas.edu>, Jenny  Davis, Antonio  Rodriguez, Brent  McKee, Anna  Atencio and Carolyn  Currin
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11421063
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# mctigue_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# mctigue_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# mctigue_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# mctigue_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
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

cores_raw <- read.csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_materials_and_methods.csv")
bib <- ReadBib("./data/primary_studies/mctigue_2020/original/citations.bib")

depthseries <- depthseries_raw %>%
  rename(fraction_carbon = fraction_carbon_measured) %>%
  select(-fraction_carbon_modeled, mass_depth)

cores <- cores_raw %>%
  mutate(core_year = year(core_date))

species <- species_raw  %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

study_citationsA <- as.data.frame(bib) %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = key,
         study_id = "McTigue_et_al_2020a",
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year))

study_citations <- study_citationsA %>%
  mutate(study_id = "McTigue_et_al_2020b") %>%
  bind_rows(study_citationsA) %>%
  mutate(bibliography_id = recode(bibliography_id,
                                  "mctigue2019sea" = "McTigue_et_al_2019",
                                  "mctigue2020DataRelease" = "McTigue_et_al_2020"))
    
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020.bib")

write_csv(cores, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_cores.csv") 
write_csv(depthseries, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_depthseries.csv")
write_csv(study_citations, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_study_citations.csv")
write_csv(species, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_species.csv")
write_csv(methods_raw, "./data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_methods.csv")

