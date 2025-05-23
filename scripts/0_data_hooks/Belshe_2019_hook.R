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
library(lubridate)
library(RefManageR)

depthseries_raw <- read.csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_depthseries.csv")
methods_raw <-read_csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_material_and_methods.csv")
sites_raw <-read_csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_sites.csv")
cores_raw <-read_csv("./data/primary_studies/Belshe_2019/original/belshe_et_al_2019_cores.csv")


## Data Curation ####

sites <- sites_raw

cores <- cores_raw %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(-core_date)

depthseries <- depthseries_raw %>%
  rename(age = pb210_age,
         fraction_carbon = fraction_carbon_measured) %>% 
  mutate(age_present = ifelse(!is.na(age), "present", NA),
         c14age_present = ifelse(!is.na(c14_age), "present", NA)) %>% 
  group_by(age_present) %>% 
  mutate(n_age = n(), # there are 89 values for !is.na(age)
         age_se = pb210_age_sd / sqrt(89), # convert SD to SE: SE = SD / sqrt(n)
         method_id = "single set of methods") %>% 
  ungroup() %>%
  group_by(c14age_present) %>% 
  mutate(n_c14age = n(), # 31
         c14_age_se = c14_age_sd / sqrt(31)) %>% 
  ungroup() %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_carbon, age, age_se, 
         c14_age, c14_age_se, c14_material)

methods <- methods_raw %>%
  mutate(carbon_measured_or_modeled = "measured",
         method_id = "single set of methods")

## Create Citation ####

if(!file.exists("data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_study_citations.csv")){
  # Create bibtex file
  data_release_doi <- "10.25573/data.9856769"
  associated_pub_doi <- "10.1029/2019JG005233"
  study_id <- "Belshe_et_al_2019"
  
  data_bib_raw <- GetBibEntryWithDOI(c(data_release_doi, associated_pub_doi))
  
  study_citations <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = study_id,
           bibliography_id = c("Belshe_et_al_2019_data", "Belshe_et_al_2019_article"),
           publication_type = c("primary dataset", "associated source")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Belshe_2019/derivative/Belshe_et_al_2019.bib")
  write_csv(study_citations, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "methods", "cores", "depthseries")

updated <- updateTables(table_names)

# save listed tables to objects
methods <- updated$methods
# depthseries <- updated$depthseries # corrections made and restructured depthseries table to current database format as of 3.26.24
sites <- updated$sites
cores <- updated$cores %>% 
  # add core SM10 position as a site level replicate of core SM05
  mutate(latitude = case_when(core_id == "SM10" ~ 39.1501,
                              T ~ latitude),
         longitude = case_when(core_id == "SM10" ~ 2.94919,
                               T ~ longitude),
         position_notes = case_when(core_id == "SM10" ~ "site level replicate of core SM05's position",
                                    T ~ NA_character_))  


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(depthseries, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_depthseries.csv")
write_csv(methods, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_methods.csv")
write_csv(cores, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_cores.csv")
write_csv(sites, "data/primary_studies/Belshe_2019/derivative/belshe_et_al_2019_sites.csv")


