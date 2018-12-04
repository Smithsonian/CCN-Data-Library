# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)
library(RCurl)

## Add Sanderman mangrove sites ############


soil_profiles <- read.csv(text=getURL("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/master/data/CCRCN_synthesis/CCRCN_core_data.csv"))

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

