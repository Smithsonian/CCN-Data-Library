# Dataset: Factors influencing blue carbon accumulation across a 32‐year chronosequence of created coastal marshes
# 
# Authors: Katherine M Abbott <kmabbott@umass.edu>, Tracy Quirk, Ronald D DeLaune
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.10005215
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in fiv separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# abbott_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# abbott_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# abbott_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# abbott_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
# abbott_et_al_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# abbott_et_al_2019_associated_publications.bib - A citation in bibtex style for associated publications to this data release

library(tidyverse)
library(RefManageR)

cores_raw <- read_csv("./data/primary_studies/Abbott_2019/original/abbott_et_al_2019_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Abbott_2019/original/abbott_et_al_2019_depthseries.csv")
impacts_raw <-read_csv("./data/primary_studies/Abbott_2019/original/abbott_et_al_2019_impacts.csv")
species_raw <- read_csv("./data/primary_studies/Abbott_2019/original/abbott_et_al_2019_species.csv")
methods_raw <- read_csv("./data/primary_studies/Abbott_2019/original/abbott_et_al_2019_material_and_methods.csv")

## Curate data ####
# Remove uncontrolled vocab
cores <- cores_raw %>%
  mutate(core_length_flag = "core depth represents deposit depth") %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK")) %>%
  select(study_id, site_id, core_id, core_year, core_month, core_day, core_longitude, core_latitude, core_position_method,
         core_elevation, core_elevation_method, salinity_class, vegetation_class)

depthseries <- depthseries_raw %>%
  mutate(method_id = "single set of methods") %>%
  select(-fraction_nitrogen)

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(-c(genus, species))

impacts <- impacts_raw

methods <- methods_raw %>% mutate(method_id = "single set of methods")

# Citation ####
if(!file.exists("data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_study_citations.csv")){
  # Create bibtex file
  data_release_doi <- "10.25573/data.10005215"
  associated_pub_doi <- "10.1002/ecs2.2828"
  study_id <- "Abbott_et_al_2019"
  
  data_bib_raw <- GetBibEntryWithDOI(c(data_release_doi, associated_pub_doi))
  
  study_citations <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = study_id,
           bibliography_id = c("Abbott_et_al_2019_data", "Abbott_et_al_2019_article"),
           publication_type = c("primary dataset", "associated source")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Abbott_2019/derivative/Abbott_et_al_2019.bib")
  write_csv(study_citations, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects

impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_cores.csv") 
write_csv(depthseries, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_depthseries.csv")
write_csv(species, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_species.csv")
write_csv(methods, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_methods.csv")
write_csv(impacts, "data/primary_studies/abbott_2019/derivative/abbott_et_al_2019_impacts.csv")

