# Coastal Carbon Research Coordination Network
# This script joins all CCRCN datasets into one synthesis
# Contact: klingesd@si.edu

## Prep Workspace ##############

library(tidyverse)

## Read in all data sources #################

# Holmquist 2018
Holmquist_2018_coredata <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_core_data.csv")
Holmquist_2018_depthseriesdata <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_depth_series_data.csv")
Holmquist_2018_impactdata <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_impact_data.csv")
Holmquist_2018_methodsdata <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_methods_data.csv")
Holmquist_2018_speciesdata <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_species_data.csv")

# Gonneaa 2018
Gonneea_2018_coredata <- read.csv("./data/Gonneea_2018/derivative/Gonneea_2018_core_data.csv")
Gonneea_2018_depthseriesdata <- read.csv("./data/Gonneea_2018/derivative/Gonneea_2018_depth_series_data.csv")

# Osland 2018
Osland_2018_coredata <- read.csv("./data/Osland_2018/derivative/Osland_2018_core_data.csv")
Osland_2018_depthseriesdata <- read.csv("./data/Osland_2018/derivative/Osland_2018_depth_series_data.csv")
Osland_2018_speciesdata <- read.csv("./data/Osland_2018/derivative/Osland_2018_species_data.csv")
Osland_2018_sitedata <- read.csv("./data/Osland_2018/derivative/Osland_2018_site_data.csv")

# Sanderman 2018
Sanderman_2018_coredata <- read.csv("./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")

# Schile-Beers and Megonigal 2017
Schile_2017_depthseriesdata <- read.csv("./data/Schile-Beers_etal_2017/derivative/Schile-Beers_etal_2017_depth_series_data.csv")
Schile_2017_coredata <- read.csv("./data/Schile-Beers_etal_2017/derivative/Schile-Beers_etal_2017_core_data.csv")
Schile_2017_sitedata <- read.csv("./data/Schile-Beers_etal_2017/derivative/Schile-Beers_etal_2017_site_data.csv")


## Join datasets ######################

# Core data
CCRCN_coredata <- Holmquist_2018_coredata %>%
  bind_rows(Gonneea_2018_coredata) %>%
  bind_rows(Osland_2018_coredata) %>%
  bind_rows(Sanderman_2018_coredata) %>%
  bind_rows(Schile_2017_coredata)

# Depth series data
# The Osland core IDs are initiatlized as numeric, as they're just numbers.
#   Switch the core IDs to factor to match all other datasets
Osland_2018_depthseriesdata$core_id <- as.factor(Osland_2018_depthseriesdata$core_id)
CCRCN_depthseriesdata <- Holmquist_2018_depthseriesdata %>%
  bind_rows(Gonneea_2018_depthseriesdata) %>%
  bind_rows(Osland_2018_depthseriesdata) %>%
  bind_rows(Schile_2017_depthseriesdata)

# Impact data
CCRCN_impactdata <- Holmquist_2018_impactdata


# Methods data
CCRCN_methodsdata <- Holmquist_2018_methodsdata

# Species data
CCRCN_speciesdata <- Holmquist_2018_speciesdata %>%
  bind_rows(Osland_2018_speciesdata)

## Write datasets #############

write.csv(CCRCN_coredata, "./data/CCRCN_synthesis/CCRCN_core_data.csv")
write.csv(CCRCN_depthseriesdata, "./data/CCRCN_synthesis/CCRCN_depthseries_data.csv")
write.csv(CCRCN_impactdata, "./data/CCRCN_synthesis/CCRCN_impact_data.csv")
write.csv(CCRCN_methodsdata, "./data/CCRCN_synthesis/CCRCN_methods_data.csv")
write.csv(CCRCN_speciesdata, "./data/CCRCN_synthesis/CCRCN_species_data.csv")

