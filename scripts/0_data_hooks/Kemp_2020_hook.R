# Dataset: Use of lead isotopes for developing chronologies in recent salt-marsh sediments
# 
# Authors: Andrew C. Kemp <Andrew.Kemp@tufts.edu>, Christopher K. Sommerfield, Christopher H. Vane, Benjamin P. Horton, Simon Chenery, Shimon Anisfeld, Daria Nikitina
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11569419
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# kemp_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# kemp_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# kemp_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# kemp_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
# kemp_et_al_2020_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - A simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - A map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# citations.bib - Is a text file containing citation information in bibtex style for this data release and the associated publication accompanying this data release.

library(tidyverse)
library(RefManageR)

cores_raw <- read.csv("./data/primary_studies/kemp_2020/original/kemp_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/kemp_2020/original/kemp_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/kemp_2020/original/kemp_et_al_2020_species.csv")
impacts_raw <- read_csv("./data/primary_studies/kemp_2020/original/kemp_et_al_2020_impacts.csv")
methods_raw <- read_csv("./data/primary_studies/kemp_2020/original/kemp_et_al_2020_material_and_methods.csv")
bib <- ReadBib("./data/primary_studies/kemp_2020/original/citations.bib")

depthseries <- depthseries_raw %>%
  select(-cs137_age, -pb210_cic_age, -pb210_crs_age)

cores <- cores_raw %>%
  rename(core_year =core_date)

species <- species_raw  %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

impacts <- impacts_raw %>%
  filter(impact_class != "eutrophic")

study_citations <- as.data.frame(bib) %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = recode(key,
                                  "Kemp_2012" = "Kemp_et_al_2012",
                                  "kemp2020DataRelease" = "Kemp_et_al_2020_Data_Release"),
         study_id = "Kemp_et_al_2020",
         publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume))

bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/kemp_2020/derivative/kemp_et_al_2020.bib")

write_csv(cores, "data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_cores.csv") 
write_csv(depthseries, "data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_depthseries.csv")
write_csv(study_citations, "data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_study_citations.csv")
write_csv(species, "data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_species.csv")
write_csv(impacts, "./data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_impacts.csv")
write_csv(methods_raw, "./data/primary_studies/kemp_2020/derivative/kemp_et_al_2020_methods.csv")
