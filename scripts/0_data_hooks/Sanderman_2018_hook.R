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


# Recode
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


## Write data ###############

write.csv(site_data, "./data/Sanderman_2018/derivative/Sanderman_2018_site_data_US.csv")
  
write.csv(core_data, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data_US.csv")

write.csv(species_data, "./data/Sanderman_2018/derivative/Sanderman_2018_species_data_US.csv")


