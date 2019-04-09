# Coastal Carbon Research Coordination Network
# This script prepares Jones et al 2017 for use by
#   the Soil Carbon working group.
# Contact: klingesd@si.edu

## 1. Workspace prep ########################

library(tidyverse)

# Site data
sites <- read_csv("./data/Jones_2017/derivative/intermediate/Jones_2017_HandEnteredSiteCoreDataFromNeotoma.csv")

# Downloaded geochrono datasets
geochron_oligo <- read_csv("./data/Jones_2017/derivative/intermediate/dataset25338.csv")
geochron_heavy_salt <- read_csv("./data/Jones_2017/derivative/intermediate/dataset25345.csv")
geochron_mod_salt <- read_csv("./data/Jones_2017/derivative/intermediate/dataset25364.csv")

# Downloaded LOI datasets
LOI_oligo_raw <- read_csv("./data/Jones_2017/original/dataset25339.csv")
LOI_heavy_salt_raw  <- read_csv("./data/Jones_2017/original/dataset25346.csv")
LOI_mod_salt_raw  <- read_csv("./data/Jones_2017/original/dataset25365.csv")

# Depthseries data digitized from figure
depthseries_figure <- read_csv("./data/Jones_2017/original/Jones_2017_depthseries.csv",
                            col_names = FALSE)



## 2. Curate Site data ############

# Imported file is already curated to CCRCN standards
sitesOutput <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  group_by(study_id, site_id) %>%
  summarise()

## 3. Core data ############

# A lot of this information was just scattered throughout the Neotoma portal.
#   Easiest to just input manually from there

core_data <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, site_id, core_id, core_longitude, core_latitude, salinity_class, vegetation_class) %>%
  mutate(vegetation_class = ifelse(vegetation_class == "tidal freshwater forest", "forested", vegetation_class))

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
  mutate(depth_min = Depth - (Thickness/2)) %>%
  mutate(depth_max = Depth + (Thickness/2)) %>%
  rename(sample_id = SampleID, c14_age = Age, c14_material = MaterialDated,
         c14_age_sd = ErrorOlder) %>%
  mutate(c14_age = ifelse(c14_age == 0, NA, c14_age)) %>%
  mutate(c14_notes = ifelse(is.na(c14_age), "c14 age yielded greater than modern day", NA)) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd, 
         c14_notes, c14_material)

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
  separate("V6", into = c("age_max", "age", "age_min"), sep = "/") %>%
  mutate(age_max = as.double(age_max), age = as.double(age),
         age_min = as.double(age_min), 
         age_depth_model_reference = "YBP") %>%
  rename(sample_id = "V5", dry_bulk_density = "V7", fraction_organic_matter = "V8") %>%
  mutate(fraction_organic_matter = as.double(fraction_organic_matter) / 100) %>%
  # Change from numeric to character because it's a tag
  mutate(sample_id = as.double(sample_id)) %>%
  mutate(depth_min = as.double(depth_min)) %>%
  mutate(depth_max = as.double(depth_max)) %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, 
         age, age_min, age_max, age_depth_model_reference)

## ....4c. Combine depthseries datasets ##############

depthseries <- geochron %>%
  full_join(LOI, by = c("study_id", "core_id", "sample_id", "depth_min", "depth_max")) %>%
  arrange(core_id, depth_min) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, 
         c14_age, c14_age_sd, c14_material, c14_notes,
         age, age_min, age_max, age_depth_model_reference)

# Data digitized from depthseries figure...still not sure what core(s) this 
#   corresponds to
depthseries_figure <- depthseries_figure %>%
  rename(depth = X1, age = X2)

## 5. Species data ###########

species <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, site_id, core_id, species_code) %>%
  separate_rows(species_code, sep="; ")


## 6. Impact data ############

impacts <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, site_id, core_id, impact_class)


## 7. Site data ################

sites <- sites %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  select(study_id, site_id)

## QA/QC ##############

source("./scripts/1_data_formatting/qa_functions.R")
fraction_not_percent(depthseries)

# Re-order according to database
#core_data <- reorder_columns(core_data, "core_level")
#depthseries <- reorder_columns(depthseries, "depthseries") 
#species <- reorder_columns(species, "species_definitions")


## Write data ###########

write_csv(sitesOutput, "./data/Jones_2017/derivative/final/Jones_2017_sites.csv")

write_csv(core_data, "./data/Jones_2017/derivative/final/Jones_2017_cores.csv")

write_csv(depthseries, "./data/Jones_2017/derivative/final/Jones_2017_depthseries.csv")

write_csv(species, "./data/Jones_2017/derivative/final/Jones_2017_species.csv")

write_csv(impacts, "./data/Jones_2017/derivative/final/Jones_2017_impact.csv")

# write_csv(depthseries_figure, "./data/Jones_2017/derivative/final/Jones_2017_depthseries_from_figure.csv")
