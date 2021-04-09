# Dataset: Accretion and sediment accumulation in impounded and unimpounded marshes in the Delaware Estuary and Barnegat Bay
# 
# Authors: Brandon Boyd <Brandon.M.Boyd@erdc.dren.mil>; Christopher Sommerfield;  Tracy Quirk ; Viktoria Unger 
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.9747065 
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# boyd_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# boyd_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# boyd_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# boyd_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
# boyd_et_al_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
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
# 
# boyd_et_al_2019_associated_publication.csv - Is a CSV file containing citation information for the associated publication accompanying this data release.

library(tidyverse)
library(RefManageR)

# Change to all files: Make Unger core IDs unique
cores_raw <- read_csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_depthseries.csv")
impacts_raw <-read_csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_impacts.csv")
species_raw <- read_csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_species.csv")
methods_raw <- read_csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_material_and_methods.csv")

# Data Curation ####
methods <- methods_raw

cores <- cores_raw %>%
  mutate(core_id = ifelse(study_id == "Unger_et_al_2016", paste0(core_id, "U"), core_id)) %>%
  rename(core_year = core_date)

impacts <- impacts_raw %>%
  mutate(core_id = ifelse(study_id == "Unger_et_al_2016", paste0(core_id, "U"), core_id))

depthseries <- depthseries_raw %>%
  mutate(core_id = as.character(core_id),
         study_id = as.character(study_id),
         site_id = as.character(site_id)) %>%
  mutate(core_id = ifelse(study_id == "Unger_et_al_2016", paste0(core_id, "U"), core_id)) %>%
  mutate(fraction_carbon = ifelse(fraction_carbon == 0, NA, fraction_carbon)) %>%
  select(-c(fraction_organic_matter_modeled, total_pb210_activity_modeled, cs137_activity_modeled, ra226_activity_modeled,
            th234_activity_modeled, bi214_activity_modeled, refractory_carbon, labile_carbon)) %>%
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA), 
         pb210_unit = ifelse(!is.na(total_pb210_activity), "becquerelsPerKilogram", NA), 
         ra226_unit = ifelse(!is.na(ra226_activity), "becquerelsPerKilogram", NA), 
         pb212_unit = ifelse(!is.na(pb212_activity), "becquerelsPerKilogram", NA), 
         bi214_unit = ifelse(!is.na(bi214_activity), "becquerelsPerKilogram", NA), 
         be7_unit = ifelse(!is.na(be7_activity), "becquerelsPerKilogram", NA),
         am241_unit = ifelse(!is.na(am241_activity), "becquerelsPerKilogram", NA))

# Species data needs to combine genus and species into one and remove other columns
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  mutate(core_id = ifelse(study_id == "Unger_et_al_2016", paste0(core_id, "U"), core_id))

# Create Citation ####

if(!file.exists("data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_study_citations.csv")){
  # Merge associated pubs with data release DOI
  bib <- read_csv("./data/primary_studies/Boyd_2019/original/boyd_et_al_2019_associated_publications.csv")
  data_release_doi <- "10.25573/data.9747065"
  key_value <- "Boyd_et_al_2019"
  
  data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
  
  # study ids will expand data citation to associate with all studies
  study_ids <- bib %>% select(study_id) %>% 
    mutate(bibliography_id = "Boyd_et_al_2019_data")
  
  data_biblio <- as.data.frame(data_bib_raw) %>%
    mutate(bibliography_id = "Boyd_et_al_2019_data", 
           publication_type = "primary") %>%
    full_join(study_ids)
  
  pub_biblio <- bib %>% 
    mutate_all(as.character) %>%
    mutate(bibliography_id = str_c(study_id, "article", sep = "_"),
           publication_type = "associated")
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- bind_rows(pub_biblio, data_biblio) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019.bib")
  write_csv(study_citations, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_study_citations.csv")
}


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries", "species", "impacts"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries", "species", "impacts"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_cores.csv") 
write_csv(depthseries, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_depthseries.csv")
write_csv(impacts, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_impacts.csv")
write_csv(species, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_species.csv")
write_csv(methods, "data/primary_studies/Boyd_2019/derivative/boyd_et_al_2019_methods.csv")
