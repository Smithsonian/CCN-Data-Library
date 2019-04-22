# Coastal Carbon Research Coordination Network
# This script joins all CCRCN datasets into one synthesis
# Contact: klingesd@si.edu

## 1. Prep Workspace ##############

library(tidyverse)

## ....1a. Read in all data sources #################

# Deegan 2012
Deegan_2012_coredata <- read_csv( "./data/Deegan_2012/derivative/Deegan_et_al_2012_cores.csv")
Deegan_2012_depthseriesdata <- read_csv( "./data/Deegan_2012/derivative/Deegan_et_al_2012_depthseries.csv")
Deegan_2012_sitedata <- read_csv( "./data/Deegan_2012/derivative/Deegan_et_al_2012_sites.csv")
Deegan_2012_citationdata <- read_csv( "./data/Deegan_2012/derivative/Deegan_et_al_2012_study_citations.csv")

# Fourqurean 2012
Fourqurean_2012_coredata <- read_csv( "./data/Fourqurean_2012/derivative/Fourqurean_2012_core_data.csv")
Fourqurean_2012_depthseriesdata <- read_csv( "./data/Fourqurean_2012/derivative/Fourqurean_2012_depthseries_data.csv")
Fourqurean_2012_sitedata <- read_csv( "./data/Fourqurean_2012/derivative/Fourqurean_2012_site_data.csv")
Fourqurean_2012_speciesdata <- read_csv( "./data/Fourqurean_2012/derivative/Fourqurean_2012_species_data.csv")
Fourqurean_2012_citationdata <- read_csv("./data/Fourqurean_2012/derivative/Fourqurean_2012_study_citations.csv")

# Giblin and Forbrich 2018
Giblin_2018_coredata <- read_csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_cores.csv")
Giblin_2018_depthseriesdata <- read_csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_depthseries.csv")
Giblin_2018_sitedata <- read_csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_sites.csv")
Giblin_2018_speciesdata <- read_csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_species.csv")
Giblin_2018_citationdata <- read_csv("./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_study_citations.csv")

# Gonneaa 2018
Gonneea_2018_coredata <- read_csv("./data/Gonneea_2018/derivative/Gonneea_et_al_2018_cores.csv")
Gonneea_2018_depthseriesdata <- read_csv("./data/Gonneea_2018/derivative/Gonneea_et_al_2018_depthseries.csv")
Gonneea_2018_citationdata <- read_csv("./data/Gonneea_2018/derivative/Gonneea_et_al_2018_study_citations.csv")

# Holmquist 2018
Holmquist_2018_coredata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
Holmquist_2018_depthseriesdata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
Holmquist_2018_impactdata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
Holmquist_2018_methodsdata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
Holmquist_2018_speciesdata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
Holmquist_2018_citationdata <- read_csv("./data/Holmquist_2018/derivative/V1_Holmquist_2018_study_citations.csv")

# Osland 2016
Osland_2016_coredata <- read_csv("./data/Osland_2016/derivative/Osland_et_al_2016_cores.csv")
Osland_2016_depthseriesdata <- read_csv("./data/Osland_2016/derivative/Osland_et_al_2016_depthseries.csv")
Osland_2016_speciesdata <- read_csv("./data/Osland_2016/derivative/Osland_et_al_2016_species.csv")
Osland_2016_sitedata <- read_csv("./data/Osland_2016/derivative/Osland_et_al_2016_sites.csv")
Osland_2016_citationdata <- read_csv("./data/Osland_2016/derivative/Osland_et_al_2016_study_citations.csv")

# Sanderman 2018
Sanderman_2018_coredata <- read_csv("./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")
Sanderman_2018_speciesdata <- read_csv("./data/Sanderman_2018/derivative/Sanderman_2018_species_data.csv")
Sanderman_2018_depthseriesdata <- read_csv("./data/Sanderman_2018/derivative/Sanderman_2018_depthseries_data.csv")
Sanderman_2018_citationdata <- read_csv("./data/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")

# Schile-Beers and Megonigal 2017
Schile_2017_depthseriesdata <- read_csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_depthseries.csv")
Schile_2017_coredata <- read_csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_cores.csv")
Schile_2017_sitedata <- read_csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_sites.csv")
Schile_2017_citationdata <- read_csv("./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_study_citations.csv")

# Smith et al. 2015
Smith_2015_coredata <- read_csv("./data/Smith_2015/derivative/Smith_et_al_2015_cores.csv")
Smith_2015_sitedata <- read_csv("./data/Smith_2015/derivative/Smith_et_al_2015_sites.csv")
Smith_2015_depthseriesdata <- read_csv("./data/Smith_2015/derivative/Smith_et_al_2015_depthseries.csv")
Smith_2015_citationdata <- read_csv("./data/Smith_2015/derivative/Smith_et_al_2015_study_citations.csv")

# Trettin et al. 2017
Trettin_2017_coredata <- read_csv("./data/Trettin_2017/derivative/Trettin_et_al_2017_cores.csv")
Trettin_2017_sitedata <- read_csv("./data/Trettin_2017/derivative/Trettin_et_al_2017_sites.csv")
Trettin_2017_depthseriesdata <- read_csv("./data/Trettin_2017/derivative/Trettin_et_al_2017_depthseries.csv")
Trettin_2017_citationdata <- read_csv("./data/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")

# Thorne et al. 2015
Thorne_2015_coredata <- read_csv( "./data/Thorne_2015_a/derivative/Thorne_et_al_2015_core_data.csv")
Thorne_2015_depthseriesdata <- read_csv("./data/Thorne_2015_a/derivative/Thorne_et_al_2015_depthseries_data.csv")
Thorne_2015_citationdata <- read_csv("./data/Thorne_2015_a/derivative/Thorne_et_al_2015_study_citations.csv")

## 2. Join datasets ######################

## ....2a. Core data ################

# Bind
CCRCN_coredata <- Holmquist_2018_coredata %>%
  bind_rows(Fourqurean_2012_coredata) %>%
  bind_rows(Gonneea_2018_coredata) %>%
  bind_rows(Osland_2016_coredata) %>%
  bind_rows(Sanderman_2018_coredata) %>%
  bind_rows(Schile_2017_coredata) %>%
  bind_rows(Deegan_2012_coredata) %>%
  bind_rows(Giblin_2018_coredata) %>%
  bind_rows(Smith_2015_coredata) %>%
  bind_rows(Trettin_2017_coredata) %>%
  bind_rows(Thorne_2015_coredata)

## ....2b. Depth series data ###############
# The Osland core IDs are initiatlized as numeric, as they're just numbers.
#   Switch the core IDs to factor to match all other datasets
Osland_2016_depthseriesdata$core_id <- as.factor(Osland_2016_depthseriesdata$core_id)

CCRCN_depthseriesdata <- Holmquist_2018_depthseriesdata %>%
  bind_rows(Fourqurean_2012_depthseriesdata) %>%
  bind_rows(Gonneea_2018_depthseriesdata) %>%
  bind_rows(Osland_2016_depthseriesdata) %>%
  bind_rows(Sanderman_2018_depthseriesdata) %>%
  bind_rows(Schile_2017_depthseriesdata) %>%
  bind_rows(Giblin_2018_depthseriesdata) %>%
  bind_rows(Smith_2015_depthseriesdata) %>%
  bind_rows(Trettin_2017_depthseriesdata) %>%
  bind_rows(Thorne_2015_depthseriesdata)

# Commenting out aggregated data for now
# # Add a column for aggregated fraction carbon and carbon density per core
# aggregate_carbon <- CCRCN_depthseriesdata %>%
#   select(core_id, fraction_carbon, dry_bulk_density) %>%
#   group_by(core_id) %>%
#   summarize_all(mean)
# 
# # Add aggregated data to core level data
# CCRCN_coredata <- CCRCN_coredata %>%
#   left_join(aggregate_carbon)
# 
# CCRCN_coredata <- CCRCN_coredata %>%
#   rename(mean_fraction_carbon = fraction_carbon,
#          mean_dry_bulk_density = dry_bulk_density)

## ....2c. Impact data ##################
CCRCN_impactdata <- Holmquist_2018_impactdata

## ....2d. Methods data #################
CCRCN_methodsdata <- Holmquist_2018_methodsdata 

## ....2e. Species data #################
CCRCN_speciesdata <- Holmquist_2018_speciesdata %>%
  bind_rows(Fourqurean_2012_speciesdata) %>%
  bind_rows(Osland_2016_speciesdata) %>%
  bind_rows(Giblin_2018_speciesdata) %>%
  bind_rows(Sanderman_2018_speciesdata)

## ....2f. Bind citation tables ###############
CCRCN_study_citations <- Holmquist_2018_citationdata %>%
  bind_rows(Fourqurean_2012_citationdata) %>%
  bind_rows(Gonneea_2018_citationdata) %>%
  bind_rows(Osland_2016_citationdata) %>%
  bind_rows(Sanderman_2018_citationdata) %>%
  bind_rows(Schile_2017_citationdata) %>%
  bind_rows(Deegan_2012_citationdata) %>%
  bind_rows(Giblin_2018_citationdata) %>%
  bind_rows(Smith_2015_citationdata) %>%
  bind_rows(Trettin_2017_citationdata) %>%
  bind_rows(Thorne_2015_citationdata)

## 3. QA #################
source("./scripts/1_data_formatting/qa_functions.R")

# Ensure all core_ids are unique
results_unique_core <- test_unique_cores(CCRCN_coredata)

# There almost 100 sets of coordinates that have two or more cores associated with them: 
write_csv(test_unique_coords(CCRCN_coredata), "./data/QA/duplicate_cores.csv")

## 4. Write datasets #############

write_csv(CCRCN_coredata, "./data/CCRCN_synthesis/CCRCN_core_data.csv")
write_csv(CCRCN_depthseriesdata, "./data/CCRCN_synthesis/CCRCN_depthseries_data.csv")
write_csv(CCRCN_impactdata, "./data/CCRCN_synthesis/CCRCN_impact_data.csv")
write_csv(CCRCN_methodsdata, "./data/CCRCN_synthesis/CCRCN_methods_data.csv")
write_csv(CCRCN_speciesdata, "./data/CCRCN_synthesis/CCRCN_species_data.csv")
write_csv(CCRCN_study_citations, "./data/CCRCN_synthesis/CCRCN_study_citations.csv")

