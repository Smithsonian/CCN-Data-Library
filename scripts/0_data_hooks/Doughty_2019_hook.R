# Mangroves marching northward: the impacts of rising seas and temperatures on ecosystems at Kennedy Space Center
# 
# Authors: Cheryl Doughty <cdoughty@ucla.edu>; J. Adam Langley;  Wayne Walker; Ilka C. Feller; Ronald Schaub; Samantha  Chapman
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# doughty_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# doughty_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# doughty_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# doughty_et_al_2019_sites.csv - Contains positional information on site locations.
# doughty_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# associated_publications.bib - Is a text file containing citation information in bibtex style for the associated publication accompanying this data release.

## Data has already been curated to CCRCN specs as part of a data release available on figshare ############
library(tidyverse)
library(RefManageR)

species_raw <- read_csv("./data/primary_studies/Doughty_2019/original/doughty_et_al_2019_species.csv")

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code)

## Create citation info  
associated_bib_doi <- "10.1007/s12237-015-9993-8"
data_release_doi <- "10.25573/data.9695918.v1"
study_id_value <- "Doughty_et_al_2016"

paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
data_bib_raw <- GetBibEntryWithDOI(data_release_doi)

# Convert this to a dataframe
paper_biblio <- as.data.frame(paper_bib_raw) %>%
  rownames_to_column("key") %>%
  mutate(study_id = study_id_value) %>%
  mutate(doi = tolower(doi),
         bibliography_id = study_id_value,
         key = "Doughty_et_al_2016") 

data_biblio <- as.data.frame(data_bib_raw) %>%
  rownames_to_column("key") %>%
  mutate(study_id = study_id_value) %>%
  mutate(doi = tolower(doi),
         bibliography_id = study_id_value,
         key = "Doughty_et_al_2019") 

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_biblio %>%
  bind_rows(paper_biblio) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume))

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Doughty_2019/derivative/Doughty_et_al_2019.bib")

write_csv(study_citations, "data/primary_studies/Doughty_2019/derivative/Doughty_et_al_2019_study_citations.csv")
write_csv(species, "data/primary_studies/Doughty_2019/derivative/doughty_et_al_2019_species.csv")
