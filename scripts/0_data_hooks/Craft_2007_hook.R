library(tidyverse)
library(readxl)

# Appending cs137 data to carbon stocks data that was already prepared in the Holmquist synthesis
cores_raw <- read_csv("./data/primary_studies/Craft_2007/intermediate/craft_CCRCN_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Craft_2007/intermediate/craft_CCRCN_depthseries.csv")

# Digitized table from Craft 2007
age_depth_raw <- read_xlsx("./data/primary_studies/Craft_2007/original/Craft2007_cs137_raw.xlsx")

age_depth <- age_depth_raw %>%
  mutate(core_id = paste(site_id, gsub("Site", "Site_", core_id), sep="_")) %>%
  rename(cs137_activity = `cs137_activity_Bqkg-1`) %>%
  select(-study_id)

depthseries <- depthseries_raw %>%
  merge(age_depth, by=c("core_id", "depth_min", "depth_max"), all.x=TRUE, all.y=TRUE) %>%
  mutate(dating_notes = ifelse(cs137_activity == 0, "cs137 activity below detection limits", NA))

cores <- cores_raw %>%
  mutate(core_year = 2001)

# QA ###########
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)
results <- test_numeric_vars(depthseries)

write_csv(depthseries, "./data/primary_studies/Craft_2007/derivative/craft_2007_depthseries.csv")
write_csv(cores, "./data/primary_studies/Craft_2007/derivative/craft_2007_cores.csv")
