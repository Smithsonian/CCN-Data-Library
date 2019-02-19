# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)
library(RCurl)
library(readxl)

## Curate Sanderman international data ############


# This excel document was handed off in a personal communication. It corresponds
#   to some of the data held here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OCYUIT

# This sheet ("horizon data") mirrrs much of the content that can be downloaded here:
"https://raw.githubusercontent.com/whrc/Mangrove-Soil-Carbon/master/depth_model_comparison/soil_profiles.csv"

internatl_depthseries_data_raw <- read_excel("./data/Sanderman_2018/original/WHRC-TNC mangrove soc database 100718.xlsx", 
                                             sheet = 4)

# Another sheet on the same excel document
internatl_core_data_raw <- read_excel("./data/Sanderman_2018/original/WHRC-TNC mangrove soc database 100718.xlsx",
                                      sheet = 3)

#m Other core level data found here (but not necessary):
"https://raw.githubusercontent.com/whrc/Mangrove-Soil-Carbon/master/R_code/41558_2018_162_MOESM2_ESM.csv"

# Jon sent me some updated data as a few sites were inaccurate.
# We'll need to insert in the new data for these sites
internatl_depthseries_data_new <- read_excel("./data/Sanderman_2018/original/RC sites.xlsx")

# Delete sites with incorrect data
internatl_depthseries_data_raw <- internatl_depthseries_data_raw %>%
  filter(`Site #` != "M0018" & `Site #` != "M0019" & `Site #` != "M0020" & 
           `Site #` != "M0021") %>%
  bind_rows(internatl_depthseries_data_new)


# The below code demonstrates evidence for the issue raised in 
# https://github.com/Smithsonian/CCRCN-Data-Library/issues/4
# Can be ignored for now
soil_profiles <- read.csv("./data/Sanderman_2018/original/soil_profiles.csv")
  
soil_profiles <- soil_profiles %>%
  rename(site_name = Site.name)

int_depth_data_test <- internatl_depthseries_data_raw %>%
  rename(site_name = `Site name`) 

absent_cores <- soil_profiles %>%
  anti_join(int_depth_data_test, by = "site_name")
## * international study level metadata ################


## * international core data ###############

# Rename and recode attributes to be congruent with core data
internatl_core_data <- internatl_core_data_raw %>%
  rename(study_id = Source) %>%
  rename(core_latitude = Latitude) %>%
  rename(core_longitude = Longitude) %>%
  rename(country = Country)

# Save as output data
write.csv(internatl_core_data, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")

## internatl depthseries data ####################

internatl_depthseries_data

## Curate Sanderman USA data ############

# This excel document was handed off in a personal communication
Sanderman_2018_USA_core_data <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
                              sheet = 2)

# Rename attributes
Sanderman_2018_USA_core_data <- Sanderman_2018_USA_core_data %>%
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
  select(-`Landscape Unsure`, -`HGM Unsure`, -`Mangrove type`)

Sanderman_2018_USA_core_data$core_length_flag <- recode(Sanderman_2018_USA_core_data$core_length_flag, 
                                     Y = "core depth represents deposit depth",
                                     N = "core depth limited by length of corer")

## Site level data ##############

site_data <- Sanderman_2018_USA_core_data %>%
  select(study_id, site_id, site_description, country, vegetation_class, 
         vegetation_notes, landscape_position, `Dominant species`)
  
## Core level data ##############

core_data <- Sanderman_2018_USA_core_data %>%
  select(study_id, site_id, core_id, country, core_latitude, core_longitude, 
         core_position_accuracy_flag, core_position_accuracy, core_date, core_length, 
         core_length_flag)

## Species data #############

species_data <- Sanderman_2018_USA_core_data %>%
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
dpethseries_data <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
                                 sheet = 3)

# Removed first row, which displays units
colnames(dpethseries_data) <- dpethseries_data[1, ] # the first row will be the header
dpethseries_data <- dpethseries_data[-1, ]

# Remove last few columns, which are dedicated to notes. The only note is 
#   "No %OC given" for row 238 (M0340, deepest depth), which is already apparent
#   (the OC column is empty)

dpethseries_data <- dpethseries_data[, 1:18]


# Rename and recalculate attributes
depthseries_data <- dpethseries_data %>%
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

