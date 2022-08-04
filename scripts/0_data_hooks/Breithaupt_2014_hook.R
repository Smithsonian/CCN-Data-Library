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
methods_raw <- read_csv("./data/primary_studies/breithaupt_2014/intermediate/breithaupt_et_al_2014_materials_and_methods.csv")
bib <- ReadBib("./data/primary_studies/breithaupt_2014/original/citations.bib")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Data Curation ####

study_id_value <- "Breithaupt_et_al_2014"

cores <- cores_raw %>%
  mutate(core_date = as_date(core_date, format="%m/%d/%Y", tz="UTC")) %>%
  mutate(core_year = year(core_date),
         core_month = month(core_date),
         core_day = day(core_date),
         study_id = study_id_value) %>%
  select(-core_date)

# Species data needs to combine genus and species into one and remove other columns
species <- species_raw %>%
  mutate(study_id = study_id_value,
         species_code = paste(genus, species, sep=" "),
         code_type = "Genus species") %>%
  select(-genus, -species)

depthseries <- depthseries_raw %>%
  rename(delta_c13 = delta_C13) %>%
  mutate(study_id = study_id_value,
         method_id = case_when(depth_max-depth_min == 1 ~ "1 cm intervals",
                               depth_max-depth_min == 2 ~ "2 cm intervals",
                               TRUE ~ NA_character_)) %>%
  select(-c(cs137_MDA, total_pb210_MDA, ra226_MDA, excess_pb210_inventory,
            excess_pb210_inventory_se, fraction_total_nitrogen, fraction_total_phosphorus,
            delta_N15, gamma_counting_sedmass, cumulative_sedmass_atdepth))
# 'date' attribute: date at bottom of the depth interval, calculated as coring date minus sample age
# cs137_unit, pb210_unit, ra226_unit missing
# paper indicates excess pb210 is disintegrationsPerMinutePerGram

# visual check
ggplot(depthseries, aes(fraction_organic_matter, fraction_carbon, col = core_id)) + 
  geom_point() +
  ggtitle("Breithaupt et al 2014 fraction carbon ~ LOI")
# ggsave("breithaupt_2014_carbon_loi.jpg")
# I think SH3-5 has modeled data?

methods <- methods_raw %>%
  select(-c(dbd_1_cm_section_sample_volume, dbd_2_cm_section_sample_volume,
            loi_1_cm_section_sample_volume, loi_2_cm_section_sample_volume))

## Citations ####
study_citations <- as.data.frame(bib) %>%
  mutate(bibliography_id = c("Breithaupt_et_al_2014_article","Breithaupt_et_al_2019_data"), 
         study_id = study_id_value,
         publication_type = c("associated source", "primary dataset")) %>%
  mutate(doi = ifelse(bibliography_id == "Breithaupt_et_al_2014_article", 
                      "10.1002/2014JG002715", doi),
         url = paste0("https://doi.org/", doi)) %>% 
  remove_rownames() %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014.bib")
write_csv(study_citations, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_study_citations.csv")

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

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_cores.csv") 
write_csv(depthseries, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_depthseries.csv")
write_csv(species, "data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_species.csv")
write_csv(methods, "./data/primary_studies/breithaupt_2014/derivative/breithaupt_et_al_2014_methods.csv")

