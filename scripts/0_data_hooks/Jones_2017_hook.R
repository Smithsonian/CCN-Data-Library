# Coastal Carbon Research Coordination Network
# This script prepares Jones et al 2017 for use by
#   the Soil Carbon working group.
# Contact: klingesd@si.edu

## 1. Workspace prep ########################

library(tidyverse)

# Site data
sites <- read_csv("data/Jones_2017/derivative/intermediate/Jones_2017_HandEnteredSiteCoreDataFromNeotoma.csv")

# Downloaded geochrono datasets
geochron_oligo <- read_csv("data/Jones_2017/derivative/intermediate/dataset25338.csv")
geochron_heavy_salt <- read_csv("data/Jones_2017/derivative/intermediate/dataset25345.csv")
geochron_mod_salt <- read_csv("data/Jones_2017/derivative/intermediate/dataset25364.csv")

# Downloaded LOI datasets
LOI_oligo_raw <- read_csv("data/Jones_2017/original/dataset25339.csv")
LOI_heavy_salt_raw  <- read_csv("data/Jones_2017/original/dataset25346.csv")
LOI_mod_salt_raw  <- read_csv("data/Jones_2017/original/dataset25365.csv")

# Depthseries data digitized from figure
depthseries_figure <- read_csv("./data/Jones_2017/original/Jones_2017_depthseries.csv",
                            col_names = FALSE)



## 2. Curate Site data ############

# Imported file is already curated to CCRCN standards

## 3. Core data ############

core_data <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, site_id, core_id)

## 4. Depthseries data ############

## ....4a. Geochronlogy data ############
geochron_oligo <- geochron_oligo %>%
  mutate(core_id = "Oligohaline_Marsh")

geochron_heavy_salt <- geochron_heavy_salt %>%
  mutate(core_id = "Heavily_Salt_Impacted_Swamp")

geochron_mod_salt <- geochron_mod_salt %>%
  mutate(core_id = "Moderately_Salt_Impacted")

source("./scripts/1_data_formatting/curation_functions.R")  
geochron <- geochron_oligo %>%
  bind_rows(geochron_heavy_salt) %>%
  bind_rows(geochron_mod_salt) %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  mutate(age_depth_model_reference = "YBP") %>%
  mutate(depth_min = Depth - (Thickness/2)) %>%
  mutate(depth_max = Depth + (Thickness/2)) %>%
  rename(sample_id = SampleID, c14_age = Age, material_dated = MaterialDated, age_depth_model_notes = Notes,
         c14_age_sd = ErrorOlder) %>%
  mutate(age_depth_model_notes = ifelse(!is.na(age_depth_model_notes), 
      "c14 age yielded greater than modern day", age_depth_model_notes)) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd, 
         material_dated, age_depth_model_reference, age_depth_model_notes)

## ....4b. LOI data ############

# Coerce into matrix and transpose
LOI_oligo <- t(as.matrix(LOI_oligo_raw))
LOI_oligo <- as.data.frame(LOI_oligo)
LOI_oligo <- as_tibble(LOI_oligo, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "Oligohaline_Marsh")

# Coerce into matrix and transpose
LOI_heavy_salt <- t(as.matrix(LOI_heavy_salt_raw))
LOI_heavy_salt <- as.data.frame(LOI_heavy_salt)
LOI_heavy_salt <- as_tibble(LOI_heavy_salt, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "Heavily_Salt_Impacted_Swamp")


# Coerce into matrix and transpose
LOI_mod_salt <- t(as.matrix(LOI_mod_salt_raw))
LOI_mod_salt <- as.data.frame(LOI_mod_salt)
LOI_mod_salt <- as_tibble(LOI_mod_salt, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "Moderately_Salt_Impacted")


# Join LOI datasets
LOI <- LOI_oligo %>%
  bind_rows(LOI_heavy_salt) %>%
  bind_rows(LOI_mod_salt) %>%
  separate(`V1`, into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(depth_max = as.double(gsub(" cm", "", depth_max))) %>%
  separate("V6", into = c("error_max", "c14_age_modeled", "error_min"), sep = "/") %>%
  mutate(error_max = as.double(error_max), c14_age_modeled = as.double(c14_age_modeled),
         error_min = as.double(error_min)) %>%
  mutate(c14_age_sd_modeled = ((error_max - c14_age_modeled) + (c14_age_modeled - error_min)) / 2) %>%
  rename(sample_id = "V5", dry_bulk_density = "V7", loss_on_ignition = "V8") %>%
  # Change from numeric to character because it's a tag
  mutate(sample_id = as.double(sample_id)) %>%
  mutate(depth_min = as.double(depth_min)) %>%
  mutate(depth_max = as.double(depth_max)) %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, dry_bulk_density, loss_on_ignition, 
         c14_age_modeled, c14_age_sd_modeled)

## ....4c. Combine depthseries datasets ##############

depthseries <- geochron %>%
  full_join(LOI, by = c("study_id", "core_id", "sample_id", "depth_min", "depth_max")) %>%
  arrange(core_id, depth_min)

# Data digitized from depthseries figure...still not sure what core(s) this 
#   corresponds to
depthseries_figure <- depthseries_figure %>%
  rename(depth = X1, age = X2)


## Write data ###########

write_csv(sites, "./data/Jones_2017/derivative/final/Jones_2017_sites.csv")

write_csv(core_data, "./data/Jones_2017/derivative/final/Jones_2017_cores.csv")

write_csv(depthseries, "./data/Jones_2017/derivative/final/Jones_2017_depthseries.csv")

write_csv(depthseries_figure, "./data/Jones_2017/derivative/final/Jones_2017_depthseries_from_figure.csv")
