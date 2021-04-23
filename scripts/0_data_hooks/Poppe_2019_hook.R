# Dataset: Sediment carbon stocks and sequestration rates in the Pacific Northwest region of Washington, US
# 
# Authors: Katrina  Poppe <katrina.poppe@wwu.edu>, John Rybczyk
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.10005248
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in six separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# poppe_and_rybczyk_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# poppe_and_rybczyk_2019_sites.csv - Contains positional and descriptive information on site locations.
# poppe_and_rybczyk_2019_cores.csv - Contains positional and descriptive information on core locations.
# poppe_and_rybczyk_2019_depthseries.csv - Contains raw depth-series information for all cores.
# poppe_and_rybczyk_2019_species.csv - Contains information on the dominant plant species at coring locations.
# poppe_and_rybczyk_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# poppe_and_rybczyk_2019_associated_publication.csv - Is a CSV file containing citation information for the associated publication accompanying this data release.

library(tidyverse)
library(RefManageR)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data
cores_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_species.csv")
impacts_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_impacts.csv")
methods_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_material_and_methods.csv")

## Curate Data ####

id <- "Poppe_et_al_2019_synthesis"

# Rename core year variable since date requires a full date string
cores <- cores_raw %>%
  mutate(study_id = id,
         core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK"),
         core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id)

# Provide unit columns
depthseries <- depthseries_raw %>%
  rename(method_id = study_id) %>%
  mutate(study_id = id,
         pb210_unit = ifelse(!is.na(total_pb210_activity), "becquerelsPerKilogram", NA), 
         pb214_unit = ifelse(!is.na(pb214_activity), "becquerelsPerKilogram", NA))%>%
  mutate(core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id, -fraction_carbon_modeled) %>%
  rename(fraction_carbon = fraction_carbon_measured)

# Format species correctly
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  mutate(species_code = gsub("sp.", "spp", species_code))%>% 
  mutate(study_id = id,
         core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) 

impacts <- impacts_raw %>%
  mutate(study_id = id,
         core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id)

methods <- methods_raw %>%
  rename(method_id = study_id) %>%
  mutate(study_id = id,
         method_id = recode(method_id, 
                            "Poppe_Rybczyk_2018" = "Poppe_and_Rybczyk_2018",
                            "Poppe_Rybczyk_2019" = "Poppe_and_Rybczyk_2019"),
         sediment_sieve_size = as.numeric(gsub(" mm", "", sediment_sieve_size)),
         carbon_profile_notes = "Modeled fraction carbon values available in https://doi.org/10.25573/data.10005248") %>%
  reorderColumns("methods", .)

## Citation ####

if(!file.exists("data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_study_citations.csv")){
  # publications
  pubs <- read_csv("data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_associated_publications.csv")
  
  pub_citations <- pubs %>%
    select(-bibtex_pubs) %>%
    rename(doi = doi_pubs, title = title_pubs, publication_type = `publication type`) %>%
    mutate(bibliography_id = str_c(study_id, publication_type, sep = "_")) %>%
    mutate(bibtype = recode(publication_type, 
                            "article" = "Article",
                            "mastersthesis" = "MastersThesis",
                            "techreport" = "TechReport")) %>%
    mutate(publication_type = "associated",
           institution = ifelse(bibtype == "TechReport", "Western Washington University", NA),
           school = ifelse(bibtype == "MastersThesis", "Western Washington University", NA)) %>%
    mutate_all(as.character)
  
  # data release
  doi <- "10.25573/data.10005248"
  data_bib <- as.data.frame(GetBibEntryWithDOI(doi))
  
  study_ids <- unique(cores$study_id)
  data_citation <-  data.frame(study_id = study_ids, data_bib) %>%
    mutate(publication_type = "synthesis",
           bibliography_id = "Poppe_and_Rybczyk_2018_2019_synthesis") 
  
  study_citations <- bind_rows(pub_citations, data_citation) %>%
    arrange(study_id) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # make corrections for mixed methos
  # raw_citations <- read_csv("data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_study_citations.csv")
  # study_citations <- raw_citations %>% mutate(study_id = id) %>% distinct()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019.bib")
  write_csv(study_citations, "data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_study_citations.csv")
}

## QA/QC ###############

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries", "species", "impacts"))
testTableVars(table_names = c("methods", "cores", "depthseries", "species", "impacts"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## Write files #########
write_csv(cores, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_cores.csv")
write_csv(depthseries, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_depthseries.csv")
write_csv(species, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_species.csv")
write_csv(impacts, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_impacts.csv")
write_csv(methods, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_methods.csv")
