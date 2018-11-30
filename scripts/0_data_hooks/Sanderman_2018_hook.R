# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)

## Add Sanderman mangrove sites ############

Sanderman_2018 <- read.csv("./data/Sanderman_2018/original/41558_2018_162_MOESM2_ESM.csv")

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
Sanderman_2018 <- Sanderman_2018 %>%
  rowid_to_column("ID") %>%
  mutate(core_id = paste0(study_id, "_", ID)) %>%
  select(-ID)

# Save as output data
write.csv(Sanderman_2018, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")

