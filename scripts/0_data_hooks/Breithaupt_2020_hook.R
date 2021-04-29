# Dataset: Increasing rates of carbon burial in southwest Florida coastal wetlands
# 
# Authors: Joshua L. Breithaupt  <Josh.Breithaupt@gmail.com> 
# Joseph M. Smoak 
# Thomas S. Bianchi 
# Derrick Vaughn 
# Christian J. Sanders 
# Kara R. Radabaugh 
# Michael J. Osland 
# Laura C. Feher 
# James C. Lynch 
# Donald R. Cahoon 
# Gordon H. Anderson 
# Kevin R.T. Whelan 
# Brad E. Rosenheim 
# Ryan P. Moyer 
# Lisa G. Chambers 
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.9894266
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# breithaupt_et_al_2020_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# breithaupt_et_al_2020_sites.csv - Contains positional and descriptive information on site locations.
# breithaupt_et_al_2020_cores.csv - Contains positional and descriptive information on core locations.
# breithaupt_et_al_2020_depthseries.csv - Contains raw depth-series information for all cores.
# breithaupt_et_al_2020_species.csv - Contains information on the dominant plant species at coring locations.
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
# library(anytime)

cores_raw <- read.csv("./data/primary_studies/breithaupt_2020/original/breithaupt_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/breithaupt_2020/original/breithaupt_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/breithaupt_2020/original/breithaupt_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/breithaupt_2020/original/breithaupt_et_al_2020_materials_and_methods.csv")
study_citations_raw <- read_csv("./data/primary_studies/breithaupt_2020/original/breithaupt_et_al_2020_study_citations.csv")

cores <- cores_raw %>%
  mutate(core_date = as.character(strptime(core_date, format = "%m/%d/%Y"))) %>%
  mutate(core_year = year(core_date),
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(-core_date)

depthseries <- depthseries_raw %>%
  rename(fraction_carbon = fraction_organic_carbon,
         delta_c13 = delta_13C) %>%
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         ra226_unit = ifelse(!is.na(ra226_activity), "disintegrationsPerMinutePerGram", NA), 
         cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA),
         method_id = "single set of methods") %>%
  select(-c(ra226_mda, excess_pb210_mda, date, fraction_CaCO3, fraction_total_nitrogen, fraction_total_phosphorus, fraction_total_lignin,
            delta_15N, ratio_ad_al_vanillyls, ratio_ad_al_syringyls, ratio_syringyls_vanillyls, ratio_p.hydroxyls_vanillyls_plus_syringyls,
            ratio_PON_totalP_phenols, gamma_counting_sedmass, cumulative_sedmass_atdepth, excess_pb210_inventory, excess_pb210_inventory_se))

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" "),
         code_type = "Genus species") %>%
  select(-genus, -species) 

methods <- methods_raw %>%
  mutate(method_id = "single set of methods") %>%
  select(-c(dry_bulk_density_1_cm_section_sample_volume, dry_bulk_density_2_cm_section_sample_volume, 
            loss_on_ignition_1_cm_section_sample_volume, loss_on_ignition_2_cm_section_sample_volume))

## Citations ####

# doi <- "https://doi.org/10.25573/data.9894266"
study_id_value <- "Breithaupt_et_al_2020"

study_citations <- study_citations_raw %>%
  rename(url = key) %>%
  mutate(bibliography_id = "Breithaupt_et_al_2020_data",
         publication_type = "primary dataset") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

## Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020.bib")
write_csv(study_citations, "./data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020_study_citations.csv")

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

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "./data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020_cores.csv")
write_csv(species, "./data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020_methods.csv")
write_csv(depthseries, "./data/primary_studies/breithaupt_2020/derivative/breithaupt_et_al_2020_depthseries.csv")


