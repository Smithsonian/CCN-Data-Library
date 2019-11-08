# Dataset: Modeling organic carbon accumulation rates and residence times in coastal vegetated ecosystems.
# 
# Authors: E. Fay Belshe <fbelshe@gmail.com>, Jose Sanjuan, Carmen Leiva-Dueñas, Nerea Piñeiro-Juncal, Oscar Serrano, Paul S. Lavery and Miguel Angel Mateo
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.9856769
# 
# The data release contains seagrass soil carbon and dated profiles. The data and metadata is located in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# belshe_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# belshe_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# belshe_et_al_2019_sites.csv - Contains positional and descriptive information on site locations.
# belshe_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
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

depthseries_raw <- read.csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_depthseries.csv")
methods_raw <-read_csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_material_and_methods.csv")

depthseries <- depthseries_raw %>%
  rename(pb210_crs_age = pb210_age, 
         pb210_crs_age_sd = pb210_age_sd,
         fraction_carbon = fraction_carbon_measured) %>%
  select(-c(depth_max_decompressed, fraction_carbon_modeled, fraction_carbon_density_measured, fraction_carbon_density_modeled))

methods <- methods_raw %>%
  mutate(carbon_measured_or_modeled = "measured")

# Create bibtex file
data_release_doi <- "10.25573/data.9856769"
associated_pub_doi <- "10.1029/2019JG005233"
study_id <- "Belshe_et_al_2019"

data_bib_raw <- GetBibEntryWithDOI(c(data_release_doi, associated_pub_doi))

bib <- as.data.frame(data_bib_raw) %>%
  rownames_to_column("key") %>%
  mutate(study_id = study_id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = study_id,
         key = ifelse(bibtype == "Misc", study_id, key),
         publication_type = bibtype) 

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- bib %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year))

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Belshe_2019/derivative/Belshe_et_al_2019.bib")

write_csv(depthseries, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_depthseries.csv")
write_csv(methods, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_methods.csv")
write_csv(study_citations, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_study_citations.csv")


