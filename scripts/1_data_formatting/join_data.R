# Coastal Carbon Research Coordination Network
# This script joins all CCRCN datasets into one synthesis
# Contact: klingesd@si.edu

## Prep Workspace ##############

library(tidyverse)

## Read in all data sources #################

# Holmquist 2018
Holmquist_2018_coredata <- read.csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
Holmquist_2018_depthseriesdata <- read.csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
Holmquist_2018_impactdata <- read.csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
Holmquist_2018_methodsdata <- read.csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
Holmquist_2018_speciesdata <- read.csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")

# Gonneaa 2018
Gonneea_2018_coredata <- read.csv("./data/Gonneea_2018/derivative/Gonneea_et_al_2018_cores.csv")
Gonneea_2018_depthseriesdata <- read.csv("./data/Gonneea_2018/derivative/Gonneea_et_al_2018_depthseries.csv")

# Osland 2016
Osland_2016_coredata <- read.csv("./data/Osland_2016/derivative/Osland_et_al_2016_cores.csv")
Osland_2016_depthseriesdata <- read.csv("./data/Osland_2016/derivative/Osland_et_al_2016_depthseries.csv")
Osland_2016_speciesdata <- read.csv("./data/Osland_2016/derivative/Osland_et_al_2016_species.csv")
Osland_2016_sitedata <- read.csv("./data/Osland_2016/derivative/Osland_et_al_2016_sites.csv")

# Sanderman 2018
Sanderman_2018_coredata <- read.csv("./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")

# Schile-Beers and Megonigal 2017
Schile_2017_depthseriesdata <- read.csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_depthseries.csv")
Schile_2017_coredata <- read.csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_cores.csv")
Schile_2017_sitedata <- read.csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_sites.csv")

# Deegan et al. 2012
Deegan_2012_coredata <- read.csv("./data/Deegan_2012/derivative/Deegan_et_al_2012_cores.csv")
Deegan_2012_sitedata <- read.csv("./data/Deegan_2012/derivative/Deegan_et_al_2012_sites.csv")
# because there is no guidance for biomass yet, I will remove the above ground core-level biomass data 
# Deegan_2012_depthseriesdata <- read.csv("./data/Deegan_2012/derivative/Deegan_et_al_2012_depthseries.csv")
Deegan_2012_coredata <- Deegan_2012_coredata %>%
  select(-aboveground_stem_length, -aboveground_mass)

# Giblin and Forbrich 2018
Giblin_2018_coredata <- read.csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_cores.csv")
Giblin_2018_depthseriesdata <- read.csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_depthseries.csv")
Giblin_2018_sitedata <- read.csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_sites.csv")
Giblin_2018_speciesdata <- read.csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_species.csv")

## Join datasets ######################

# Core data
CCRCN_coredata <- Holmquist_2018_coredata %>%
  bind_rows(Gonneea_2018_coredata) %>%
  bind_rows(Osland_2016_coredata) %>%
  bind_rows(Sanderman_2018_coredata) %>%
  bind_rows(Schile_2017_coredata) %>%
  bind_rows(Deegan_2012_coredata) %>%
  bind_rows(Giblin_2018_coredata) %>%
  select(-X)

# Depth series data
# The Osland core IDs are initiatlized as numeric, as they're just numbers.
#   Switch the core IDs to factor to match all other datasets
Osland_2016_depthseriesdata$core_id <- as.factor(Osland_2016_depthseriesdata$core_id)
CCRCN_depthseriesdata <- Holmquist_2018_depthseriesdata %>%
  bind_rows(Gonneea_2018_depthseriesdata) %>%
  bind_rows(Osland_2016_depthseriesdata) %>%
  bind_rows(Schile_2017_depthseriesdata) %>%
  bind_rows(Giblin_2018_depthseriesdata) %>%
  select(-X)

# Impact data
CCRCN_impactdata <- Holmquist_2018_impactdata


# Methods data
CCRCN_methodsdata <- Holmquist_2018_methodsdata

# Species data
CCRCN_speciesdata <- Holmquist_2018_speciesdata %>%
  bind_rows(Osland_2016_speciesdata) %>%
  bind_rows(Giblin_2018_speciesdata) %>%
  select(-X)

## QA #################
source("./scripts/1_data_formatting/qa_functions.R")

# Ensure all core_ids are unique
results <- test_unique_cores(CCRCN_coredata)

## Write datasets #############

write.csv(CCRCN_coredata, "./data/CCRCN_synthesis/CCRCN_core_data.csv")
write.csv(CCRCN_depthseriesdata, "./data/CCRCN_synthesis/CCRCN_depthseries_data.csv")
write.csv(CCRCN_impactdata, "./data/CCRCN_synthesis/CCRCN_impact_data.csv")
write.csv(CCRCN_methodsdata, "./data/CCRCN_synthesis/CCRCN_methods_data.csv")
write.csv(CCRCN_speciesdata, "./data/CCRCN_synthesis/CCRCN_species_data.csv")

