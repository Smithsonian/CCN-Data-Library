## CCRCN Data Library Hook Script ####
## contact: Jaxine Wolfe, wolfejax@si.edu

# Dataset: Tidal and nontidal estuarine wetland restoration: a trade-off between carbon sequestration, methane emissions and soil accretion
# Authors: Ariane Arias-Ortiz <aariasortiz@berkeley.edu>, Pere Masqué, Adina Paytan, and Dennis D. Baldocchi.
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.15127743

## DATA INGESTED UNDER GUIDANCE V2.1.1

library(tidyverse)
library(RefManageR)
# library(lubridate)
# library(anytime)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in datasets
raw_cores <- read_csv("./data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_cores.csv")
raw_depthseries <- read_csv("./data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_depthseries.csv")
raw_methods <- read_csv("./data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_material_and_methods.csv")
raw_impacts <- read_csv("./data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_impacts.csv")
raw_species <- read_csv("./data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_species.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

# cores
cores <- raw_cores %>%
  rename(year = core_year, month = core_month, day = core_day, 
         latitude = core_latitude, longitude = core_longitude,
         position_method = core_position_method, position_notes = core_position_notes, 
         elevation_notes = core_elevation_notes)

# depthseries
depthseries <- raw_depthseries %>%
  rename(fraction_carbon = fraction_organic_carbon,
         total_pb210_activity_se = total_pb210_activity_sd,
         ra226_activity_se = ra226_activity_sd, 
         excess_pb210_activity_se = excess_pb210_activity_sd) %>% 
  mutate(method_id = "single set of methods",
         compaction_notes = paste0("compression corrected max depth: ", decompressed_depth_max)) %>% 
  select(-c(core_diameter, decompressed_depth_max, wet_weight, dry_weight, accumulated_mass,
            mid_section_accumulated_mass, cn_ratio, delta_n15, k40_activity, k40_activity_sd,
            fraction_estimated_organic_carbon, fraction_inorganic_carbon, fraction_nitrogen,  fraction_h2o)) %>% 
  reorderColumns("depthseries", .)

# methods
# ra226_counting_method not in guidance
# custom box model and selected samples for ra226 assumption
methods <- raw_methods %>% 
  mutate(method_id = "single set of methods", 
         age_depth_model_notes = "custom box model used", 
         excess_pb210_model = "CRS") %>% 
  select(-ra226_counting_method)

# impacts (no change)
impacts <- raw_impacts 

# species
species <- raw_species %>% 
  mutate(species_code = str_c(genus, species, sep = " "),
         code_type = "Genus") %>% 
  # resolveTaxa(.) %>% 
  select(-genus, -species)

################

## Create citation info 

id <- "Arias-Ortiz_et_al_2021"

  paper_bib_raw <- as.data.frame(ReadBib("data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_associated_publication.bib"))
  data_bib_raw <- as.data.frame(ReadBib("data/primary_studies/Arias-Ortiz_et_al_2021/original/arias_ortiz_et_al_2021_data_publication.bib"))
  
  # Convert this to a dataframe
  # paper_biblio <- as.data.frame(paper_bib_raw) %>%
  #   mutate(study_id = id,
  #          bibliography_id = "Arias-Ortiz_et_al_2015_article",
  #          publication_type = "associated source") %>%
  #   remove_rownames()
  # 
  # data_biblio <- as.data.frame(data_bib_raw) %>%
  #   mutate(study_id = id,
  #          bibliography_id = "Arias-Ortiz_et_al_2021_data",
  #          publication_type = "primary dataset") %>%
  #   remove_rownames()
  # 
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- bind_rows(data_bib_raw, paper_bib_raw) %>%
    mutate(study_id = id,
           bibliography_id = c("Arias-Ortiz_et_al_2021_data", "Arias-Ortiz_et_al_2021_article"),
           publication_type = c("primary dataset", "associated source")) %>%
    remove_rownames() %>% 
    # mutate(month = ifelse(is.na(month), "jan", month)) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  # write files
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021.bib")
  write_csv(study_citations, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_study_citations.csv")

## QA/QC ###############

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_cores.csv")
write_csv(methods, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_methods.csv")
write_csv(depthseries, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_depthseries.csv")
write_csv(impacts, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_impacts.csv")
write_csv(species, "./data/primary_studies/Arias-Ortiz_et_al_2021/derivative/Arias-Ortiz_et_al_2021_species.csv")


