## CCRCN Data Library Hook Script ####

## Prepare workspace ####

library(tidyverse)
library(readxl)
library(RefManageR)

# Appending cs137 data to carbon stocks data that was already prepared in the Holmquist synthesis
cores_raw <- read_csv("./data/primary_studies/Craft_2007/intermediate/craft_CCRCN_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Craft_2007/intermediate/craft_CCRCN_depthseries.csv")
methods_raw <- read_csv(("./data/primary_studies/Craft_2007/original/craft_2007_methods.csv"))
species_raw <- read_csv("./data/primary_studies/Craft_2007/original/craft_2007_species.csv")

## Curate data ####

id <- "Craft_2007"

methods <- methods_raw %>% mutate(fraction_carbon_type = "total carbon",
                                  method_id = "single set of methods")

species <- species_raw %>% mutate(code_type = "Genus species")

# Digitized table from Craft 2007
age_depth_raw <- read_xlsx("./data/primary_studies/Craft_2007/original/Craft2007_cs137_raw.xlsx")

age_depth <- age_depth_raw %>%
  mutate(core_id = paste(site_id, gsub("Site", "Site_", core_id), sep="_")) %>%
  rename(cs137_activity = `cs137_activity_Bqkg-1`) %>%
  select(-study_id)

depthseries <- depthseries_raw %>%
  merge(age_depth, by=c("core_id", "depth_min", "depth_max"), all.x=TRUE, all.y=TRUE) %>%
  mutate(depth_interval_notes = ifelse(cs137_activity == 0, "cs137 activity below detection limits", NA),
         cs137_unit = "becquerelsPerKilogram",
         method_id = "single set of methods",
         study_id = id)

cores <- cores_raw %>%
  mutate(core_year = 2001)

## Citations ####

bib <- ReadBib("./data/primary_studies/Craft_2007/original/Craft_2007.bib")

study_citations <- as.data.frame(bib) %>%
  mutate(bibliography_id = "Craft_2007_article",
         study_id = "Craft_2007",
         publication_type = "associated source") %>%
  remove_rownames() %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Craft_2007/Craft_2007.bib")
write_csv(study_citations, "./data/primary_studies/Craft_2007/derivative/craft_2007_study_citations.csv")

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
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# write files
write_csv(depthseries, "./data/primary_studies/Craft_2007/derivative/craft_2007_depthseries.csv")
write_csv(cores, "./data/primary_studies/Craft_2007/derivative/craft_2007_cores.csv")
write_csv(methods, "./data/primary_studies/Craft_2007/derivative/craft_2007_methods.csv")
write_csv(species, "./data/primary_studies/Craft_2007/derivative/craft_2007_species.csv")
