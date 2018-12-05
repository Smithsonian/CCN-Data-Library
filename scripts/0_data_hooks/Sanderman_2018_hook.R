# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)
library(RCurl)
library(readxl)

## Add Sanderman mangrove sites ############


# Rename and recode attributes to be congruent with core data
Sanderman_2018 <- Sanderman_2018 %>%
  rename(study_id = Source) %>%
  rename(core_latitude = Latitude) %>%
  rename(core_longitude = Longitude) %>%
  rename(site_id = Country) %>%
  mutate(organic_carbon_density = (SOC..mg.cm.3.)/1000) %>%
  select(-SOC..mg.cm.3.)


# Recode what Sanderman's study is called
Sanderman_2018$study_id <- recode(Sanderman_2018$study_id, 
                                       "This study" = "Sanderman et al 2018")

# Create core IDs for each core
source("./scripts/1_data_formatting/curation_functions.R")
Sanderman_2018 <- Sanderman_2018 %>%
  create_core_IDs("study_id")

# Save as output data
write.csv(Sanderman_2018, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")


## Curate Sanderman USA mangroves ############

sanderman_2018_USA <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
                              sheet = 2)


# Rename attributes
sanderman_2018_USA <- sanderman_2018_USA %>%
  rename(study_id = "Source") %>%
  mutate(study_id = gsub(" ", "_", study_id)) %>%
  rename(site_id = "Site name") %>%
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  rename(core_id = "Site #") %>%
  rename(site_description = "Location") %>%
  rename(country = "Country") %>%
  rename(core_latitude = "Latitude") %>%
  rename(core_longitude = "Longitude") %>%
  rename(core_position_accuracy_flag = "accuracy_flag") %>%
  rename(core_position_accuracy = "approx_acc") %>%
  mutate(vegetation_class = "forested") %>%
  mutate(vegetation_notes = "mangrove") %>%
  rename(landscape_position = "Landscape position") %>%
  rename(core_date = "Years_collected") %>%
  rename(core_length = "total_depth") %>%
  rename(core_length_flag = "full_profile") %>%
  select(-`Landscape Unsure`, -`HGM Unsure`, -`Dominant species`, -`Mangrove type`)

sanderman_2018_USA$core_length_flag <- recode(sanderman_2018_USA$core_length_flag, 
                                     Y = "core depth represents deposit depth",
                                     N = "core depth limited by length of corer")

## Site level data ##############
site_data <- sanderman_2018_USA %>%
  select(study_id, site_id, site_description, country, vegetation_class, 
         vegetation_notes, landscape_position)
  
## Core level data ##############

core_data <- sanderman_2018_USA %>%
  select(study_id, site_id, core_id, country, core_latitude, core_longitude, 
         core_position_accuracy_flag, core_position_accuracy, core_date, core_length, 
         core_length_flag)

## Species data #############

species_data <- site_data %>%
  select(study_id, site_id, core_id, `Dominant species`) %>%
  rename(species_code = "Dominant species")

# Species were input with multiple species per row. Split up so that each species
#   entry gets a row
species_data<- species_data %>% 
  mutate(species_code = strsplit(as.character(species_code), ",")) %>% 
  unnest(species_code)

## Study level metadata ###############
study_metadata <- site_data %>%
  rename(email = "Data_owner")

## Depth series data ##############

# Read in the "horizon" sheet of "mangroves for USA" excel book
Sanderman_2018_USA_horizon <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
                                 sheet = 3)

# Removed first row, which displays units
colnames(Sanderman_2018_USA_horizon) <- Sanderman_2018_USA_horizon[1, ] # the first row will be the header
Sanderman_2018_USA_horizon <- Sanderman_2018_USA_horizon[-1, ]

# Remove last few columns, which are dedicated to notes. The only note is 
#   "No %OC given" for row 238 (M0340, deepest depth), which is already apparent
#   (the OC column is empty)

Sanderman_2018_USA_horizon <- Sanderman_2018_USA_horizon[, 1:18]


# Rename and recalculate attributes
depthseries_data <- Sanderman_2018_USA_horizon %>%
  rename(site_id = "Site name") %>%
  rename(core_id = "Site #") %>%
  rename(depth_min = "U_depth") %>%
  mutate(depth_min = 100 * as.numeric(depth_min)) %>%
  rename(depth_max = "L_depth") %>%
  mutate(depth_max = 100 * as.numeric(depth_max)) %>%
  rename(BD_est = "BD_new est")

# Sanderman reports the measured BD (if measured), the modeled BD, and then the 
#   "final" BD-- the value chosen. We'll just use the final, and create a flag
#   describing whether it was measured or modeled
  
depthseries_data_measured <- depthseries_data %>%
  filter(BD_final != BD_est) %>%
  mutate(DBD_measured_or_modeled = "measured")

depthseries_data_modeled <- depthseries_data %>%
  filter(BD_final == BD_est) %>%
  mutate(DBD_measured_or_modeled = "modeled")

depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
depthseries_data <- depthseries_data %>%
  # assumption: bulk density stands for dry bulk density
  rename(dry_bulk_density = "BD_final") %>%
  # convert from [Mg m-3] to [g cm-3]...which ends up being just x 1
  mutate(dry_bulk_density = as.numeric(dry_bulk_density) * 10^6 / 10^6) %>%
  select(-BD_reported, -BD_est) %>%
  rename(fraction_organic_matter = "SOM") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100)
  
 
# Similar as bulk density...with organic carbon this time
depthseries_data_measured <- depthseries_data %>%
  filter(OC_final == OC) %>%
  mutate(OC_measured_or_modeled = "measured")

depthseries_data_modeled <- depthseries_data %>%
  filter(OC_final != OC | is.na(OC)) %>%
  mutate(OC_measured_or_modeled = "modeled")

depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
depthseries_data <- depthseries_data %>%
  rename(fraction_carbon = "OC_final") %>%
  mutate(fraction_carbon = as.numeric(fraction_carbon) / 100) %>%
  # assumption: I don't know what TN means, but this appears to be some form of
  #   nitrogren...
  rename(fraction_nitrogen = "TN") %>%
  mutate(fraction_nitrogen = as.numeric(fraction_nitrogen) / 100)

# Similar as bulk density and organic carbon...carbon density this time
depthseries_data_measured <- depthseries_data %>%
  filter(CD_calc == CD_reported) %>%
  mutate(CD_measured_or_modeled = "measured")

depthseries_data_modeled <- depthseries_data %>%
  filter(CD_calc != CD_reported | is.na(CD_reported)) %>%
  mutate(CD_measured_or_modeled = "modeled")

depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
depthseries_data <- depthseries_data %>%
  rename(carbon_density = "CD_calc") %>%
  rename(core_date = "Year_sampled")

# Assumption: I don't know what TC and TOC are, so just leaving as is

# Remove unwanted attributes
depthseries_data <- depthseries_data %>%
  select(-OC, -CD_reported, -OC_stock_reported, -Age)

## Write data ###############

write.csv(site_data, "./data/Sanderman_2018/derivative/Sanderman_2018_site_data_US.csv")
  
write.csv(core_data, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data_US.csv")

write.csv(species_data, "./data/Sanderman_2018/derivative/Sanderman_2018_species_data_US.csv")

write.csv(depthseries_data, "./data/Sanderman_2018/derivative/Sanderman_2018_depthseries_data_US.csv")

