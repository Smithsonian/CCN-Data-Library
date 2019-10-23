library(tidyverse)

# Appending c14 data to carbon stocks data that was already prepared in the Holmquist synthesis
cores <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Gerlach_2017/intermediate/gerlach_CCRCN_depthseries.csv")

# Digitized table from Gerlach et al 2017
age_depth_raw <- read_csv("./data/primary_studies/Gerlach_2017/original/gerlach_2017_c14.csv")

# Digitized extra horizon data
horizons_raw <- read_csv("data/primary_studies/Gerlach_2017/original/Gerlach_DepthSeries_HorizonData.csv")

# Interpreted extra elevation data from paper SI
extra_elevations <- read_csv("data/primary_studies/Gerlach_2017/original/gerlach_extra_elevations_191023.csv")

# Core ID for c14 data
core_id_value <- "LMR_14_9B"
study_id_value <- "Gerlach_et_al_2017"

age_depth <- age_depth_raw %>%
  mutate(core_id = core_id_value,
         study_id = study_id_value,
         depth_min = Depth_in_core,
         depth_max = Depth_in_core,
         sample_id = paste("OS", NOSAMS_lab_number, sep="-")) %>%
  separate(calibrated_age, into=c("age_min", "age_max"), "-") %>%
  mutate(age_min = as.numeric(age_min),
         age_max = as.numeric(age_max)) %>%
  select(-c(Depth_in_core, NOSAMS_lab_number)) %>%
  rename(c14_age_sd=c14_age_error)

depthseries <- depthseries_raw %>%
  select(-X1) %>%
  bind_rows(age_depth) %>%
  full_join(horizons_raw) %>%
  arrange(core_id, depth_min) %>%
  select(-site_id)

# QA ###########
source("./scripts/1_data_formatting/qa_functions.R")

depthseries <- reorderColumns("depthseries", depthseries)

cores <- cores %>%
  full_join(extra_elevations)
cores <- reorderColumns("cores", cores)


# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)
results <- test_numeric_vars(depthseries)

write_csv(depthseries, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_depthseries.csv")
write_csv(cores, "./data/primary_studies/Gerlach_2017/derivative/gerlach_et_al_2017_cores.csv")

