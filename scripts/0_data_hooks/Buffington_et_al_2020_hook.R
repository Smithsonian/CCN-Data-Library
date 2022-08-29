# Dataset: Carbon stocks and accretion rates for wetland sediment at Miner Slough, Sacramento-San Joaquin Delta, California
# 
# Author: Kevin  Buffington 
# Christopher  Janousek 
# Karen  Thorne 
# Bruce  Dugger
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11968740
# 
# The data release contains wetland soil carbon and dated profiles. The data and metadata is located in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# buffington_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# buffington_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# buffington_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# buffington_et_al_2020_species.csv - Contains dominant species information for all site and core locations
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

cores_raw <- read.csv("./data/primary_studies/buffington_et_al_2020/original/buffington_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/buffington_et_al_2020/original/buffington_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/buffington_et_al_2020/original/buffington_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/buffington_et_al_2020/original/buffington_et_al_2020_materials_and_methods.csv")

cores <- cores_raw %>%
  rename(core_year = core_date) %>%
  mutate(core_length_flag = "not specified",
         core_position_method = "RTK",
         core_elevation_method = "RTK") %>%
  select(-annual_accretion_rate)

depthseries <- depthseries_raw %>%
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA),
         method_id = "single set of methods") %>% 
  select(-fraction_carbon) # modeled

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) 

methods <- methods_raw %>%
  mutate(method_id = "single set of methods",
         carbonate_removal_method = "carbonates not removed",
         fraction_carbon_method = "Craft regression") %>%
  rename(sediment_sieved_flag = sediment_seived_flag)
# core diameter: 5.2cm
# data release note references Callaway 2012 for the LOI ~ C relationship,
# but that paper references Craft 1991

## Citations ####
doi <- "https://doi.org/10.25573/serc.11968740"
study_id_value <- "Buffington_et_al_2020"

bib <- GetBibEntryWithDOI(doi)

study_citations <- as.data.frame(bib) %>%
  mutate(study_id = study_id_value,
         bibliography_id = "Buffington_et_al_2020_data",
         publication_type = "primary dataset") %>%
  remove_rownames() %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020.bib")
write_csv(study_citations, "./data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020_study_citations.csv")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testConditional(table_names)
testTaxa(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "./data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/buffington_et_al_2020/derivative/buffington_et_al_2020_depthseries.csv")
