# Coastal Carbon Research Coordination Network
# This script joins all CCRCN datasets into one synthesis
# Contact: wolfejax@si.edu

## 1. Prep Workspace ##############

library(tidyverse)
library(RefManageR)

## ....1a. Read in all data sources #################

# Deegan 2012
# Biomass data no cuurently supported
# Deegan_2012_coredata <- read_csv( "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_cores.csv")
# The following file doesn't exist - only a biomass_depthseries file
# Deegan_2012_depthseriesdata <- read_csv( "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_depthseries.csv")
# Deegan_2012_sitedata <- read_csv( "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_sites.csv")
# Deegan_2012_citationdata <- read_csv( "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_study_citations.csv")

# Drexler 2009
Drexler_2009_coredata <- read_csv( "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_cores.csv")
Drexler_2009_depthseriesdata <- read_csv( "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_depthseries.csv")
Drexler_2009_speciesdata <- read_csv( "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_species_data.csv")
Drexler_2009_methodsdata <- read_csv( "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_methods_data.csv")
Drexler_2009_citationdata <- read_csv( "./data/primary_studies/Drexler_2009/derivative/Drexler_et_al_2009_study_citations.csv")

# Fourqurean 2012
Fourqurean_2012_coredata <- read_csv( "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_core_data.csv")
Fourqurean_2012_depthseriesdata <- read_csv( "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_depthseries_data.csv")
Fourqurean_2012_sitedata <- read_csv( "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_site_data.csv")
Fourqurean_2012_speciesdata <- read_csv( "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_species_data.csv")
Fourqurean_2012_citationdata <- read_csv("./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_study_citations.csv")

# Giblin and Forbrich 2018
Giblin_2018_coredata <- read_csv("./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_cores.csv")
Giblin_2018_depthseriesdata <- read_csv("./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_depthseries.csv")
Giblin_2018_sitedata <- read_csv("./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_sites.csv")
Giblin_2018_speciesdata <- read_csv("./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_species.csv")
Giblin_2018_citationdata <- read_csv("./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_study_citations.csv")

# Gonneaa 2018
Gonneea_2018_coredata <- read_csv("./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_cores.csv")
Gonneea_2018_depthseriesdata <- read_csv("./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_depthseries.csv")
Gonneea_2018_citationdata <- read_csv("./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_study_citations.csv")

# Holmquist 2018
Holmquist_2018_coredata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
Holmquist_2018_depthseriesdata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
Holmquist_2018_impactdata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
Holmquist_2018_methodsdata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
Holmquist_2018_speciesdata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
Holmquist_2018_citationdata <- read_csv("./data/primary_studies/Holmquist_2018/derivative/V1_Holmquist_2018_study_citations.csv")

# Osland 2016
Osland_2016_coredata <- read_csv("./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_cores.csv")
Osland_2016_depthseriesdata <- read_csv("./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_depthseries.csv")
Osland_2016_speciesdata <- read_csv("./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_species.csv")
Osland_2016_sitedata <- read_csv("./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_sites.csv")
Osland_2016_citationdata <- read_csv("./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_study_citations.csv")

# Sanderman 2018
Sanderman_2018_coredata <- read_csv("./data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")
Sanderman_2018_speciesdata <- read_csv("./data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_species_data.csv")
Sanderman_2018_depthseriesdata <- read_csv("./data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_depthseries_data.csv")
Sanderman_2018_citationdata <- read_csv("./data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")

# Schile-Beers and Megonigal 2017
Schile_2017_depthseriesdata <- read_csv("./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_depthseries.csv")
Schile_2017_coredata <- read_csv("./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_cores.csv")
Schile_2017_sitedata <- read_csv("./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_sites.csv")
Schile_2017_citationdata <- read_csv("./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_study_citations.csv")

# Smith et al. 2015
Smith_2015_coredata <- read_csv("./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_cores.csv")
Smith_2015_sitedata <- read_csv("./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_sites.csv")
Smith_2015_depthseriesdata <- read_csv("./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_depthseries.csv")
Smith_2015_citationdata <- read_csv("./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_study_citations.csv")

# Trettin et al. 2017
Trettin_2017_coredata <- read_csv("./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_cores.csv")
Trettin_2017_sitedata <- read_csv("./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_sites.csv")
Trettin_2017_depthseriesdata <- read_csv("./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_depthseries.csv")
Trettin_2017_citationdata <- read_csv("./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")

# Thorne et al. 2015
Thorne_2015_coredata <- read_csv( "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_core_data.csv")
Thorne_2015_depthseriesdata <- read_csv("./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_depthseries_data.csv")
Thorne_2015_citationdata <- read_csv("./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_study_citations.csv")

# Krauss et al. 2018
Krauss_2018_coredata <- read_csv("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_cores.csv")
Krauss_2018_depthseriesdata <- read_csv("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_depthseries.csv",
                                        col_types = cols(
                                          sample_id = col_character()
                                        ))
Krauss_2018_sitedata <- read_csv("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_sites.csv")
Krauss_2018_citationdata <- read_csv("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_study_citations.csv")
Krauss_2018_methodsdata <- read_csv("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_methods.csv")

# Weis et al. 2001
Weis_2001_coredata <- read_csv("./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_cores.csv")
Weis_2001_speciesdata <- read_csv("./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_species.csv")
Weis_2001_depthseriesdata <- read_csv("./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_depthseries.csv")
Weis_2001_citationdata <- read_csv("./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_study_citations.csv")
Weis_2001_methodsdata <- read_csv("./data/primary_studies/Weis_et_al_2001/derivative/Weis_et_al_2001_methods.csv")

# Johnson et al 2007
Johnson_2007_coredata <- read_csv("./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_cores.csv")
Johnson_2007_speciesdata <- read_csv("./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_species.csv")
Johnson_2007_depthseriesdata <- read_csv("./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_depthseries.csv")
Johnson_2007_citationdata <- read_csv("./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_study_citations.csv")
Johnson_2007_methodsdata <- read_csv("./data/primary_studies/Johnson_2007/derivative/Johnson_et_al_2007_methods.csv")

# Drexler et al 2013
Drexler_2013_coredata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_cores.csv") 
Drexler_2013_speciesdata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_species.csv")
Drexler_2013_depthseriesdata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_depthseries.csv")
Drexler_2013_citationdata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_study_citations.csv")
Drexler_2013_methodsdata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_material_and_methods.csv")
Drexler_2013_impactdata <- read_csv("./data/primary_studies/Drexler_2013/derivative/Drexler_et_al_2013_impacts.csv")

# Drexler et al 2019
Drexler_2019_coredata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_cores.csv") 
Drexler_2019_speciesdata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_species.csv")
Drexler_2019_depthseriesdata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_depthseries.csv")
Drexler_2019_citationdata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_study_citations.csv")
Drexler_2019_methodsdata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_material_and_methods.csv")
Drexler_2019_impactdata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_impacts.csv")
Drexler_2019_sitedata <- read_csv("./data/primary_studies/Drexler_2019/derivative/Drexler_et_al_2019_sites.csv")

# Noe et al 2016
Noe_2016_coredata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_cores.csv") 
Noe_2016_speciesdata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_species.csv")
Noe_2016_depthseriesdata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_depthseries.csv")
Noe_2016_citationdata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_study_citations.csv")
Noe_2016_methodsdata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_material_and_methods.csv")
Noe_2016_impactdata <- read_csv("./data/primary_studies/Noe_2016/derivative/Noe_et_al_2016_impacts.csv")

## 2. Join datasets ######################

## ....2a. Core data ################

# Bind
CCRCN_coredata <- Holmquist_2018_coredata %>%
  bind_rows(Drexler_2009_coredata) %>%
  bind_rows(Fourqurean_2012_coredata) %>%
  bind_rows(Gonneea_2018_coredata) %>%
  bind_rows(Osland_2016_coredata) %>%
  bind_rows(Sanderman_2018_coredata) %>%
  bind_rows(Schile_2017_coredata) %>%
  bind_rows(Giblin_2018_coredata) %>%
  bind_rows(Smith_2015_coredata) %>%
  bind_rows(Trettin_2017_coredata) %>%
  bind_rows(Thorne_2015_coredata) %>% 
  bind_rows(Krauss_2018_coredata) %>%
  bind_rows(Weis_2001_coredata) %>%
  bind_rows(Johnson_2007_coredata) %>%
  bind_rows(Drexler_2013_coredata) %>%
  bind_rows(Drexler_2019_coredata) %>%
  bind_rows(Noe_2016_coredata)

## ....2b. Depth series data ###############

CCRCN_depthseriesdata <- Holmquist_2018_depthseriesdata %>%
  bind_rows(Drexler_2009_depthseriesdata) %>%
  bind_rows(Fourqurean_2012_depthseriesdata) %>%
  bind_rows(Gonneea_2018_depthseriesdata) %>%
  bind_rows(Osland_2016_depthseriesdata) %>%
  bind_rows(Sanderman_2018_depthseriesdata) %>%
  bind_rows(Schile_2017_depthseriesdata) %>%
  bind_rows(Giblin_2018_depthseriesdata) %>%
  bind_rows(Smith_2015_depthseriesdata) %>%
  bind_rows(Trettin_2017_depthseriesdata) %>%
  bind_rows(Thorne_2015_depthseriesdata) %>% 
  bind_rows(Krauss_2018_depthseriesdata) %>%
  bind_rows(Weis_2001_depthseriesdata) %>%
  bind_rows(Johnson_2007_depthseriesdata) %>%
  bind_rows(Drexler_2013_depthseriesdata) %>%
  bind_rows(Drexler_2019_depthseriesdata) %>%
  bind_rows(Noe_2016_depthseriesdata)

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

## ....2c. site data ###############
CCRCN_sitedata <- Fourqurean_2012_sitedata %>% 
  bind_rows(Giblin_2018_sitedata) %>% 
  bind_rows(Osland_2016_sitedata) %>% 
  bind_rows(Schile_2017_sitedata) %>% 
  bind_rows(Smith_2015_sitedata) %>% 
  bind_rows(Trettin_2017_sitedata) %>% 
  bind_rows(Krauss_2018_sitedata) %>%
  bind_rows(Drexler_2019_sitedata)

  # Removing vegetation_method attribute because redunant with same attribute
  #   in core level. DK thinks this should live in the materials and methods 
  #   anyways.
if (length(select(CCRCN_sitedata, contains("vegetation_method"))) > 0) {
  CCRCN_sitedata <- CCRCN_sitedata %>% 
    select(-vegetation_method)
}
  
## ....2d. Impact data ##################
CCRCN_impactdata <- Holmquist_2018_impactdata %>%
  bind_rows(Drexler_2013_impactdata) %>%
  bind_rows(Drexler_2019_impactdata) %>%
  bind_rows(Noe_2016_impactdata)

## ....2e. Methods data #################
CCRCN_methodsdata <- Holmquist_2018_methodsdata %>%
  bind_rows(Drexler_2009_methodsdata) %>%
  bind_rows(Weis_2001_methodsdata) %>%
  bind_rows(Johnson_2007_methodsdata) %>%
  bind_rows(Drexler_2013_methodsdata) %>%
  bind_rows(Drexler_2019_methodsdata) %>%
  bind_rows(Noe_2016_methodsdata) %>%
  bind_rows(Krauss_2018_methodsdata)

## ....2f. Species data #################
CCRCN_speciesdata <- Osland_2016_speciesdata %>%
  bind_rows(Sanderman_2018_speciesdata) %>%
  bind_rows(Fourqurean_2012_speciesdata) %>%
  bind_rows(Giblin_2018_speciesdata) %>%
  bind_rows(Holmquist_2018_speciesdata) %>%
  bind_rows(Drexler_2009_speciesdata) %>%
  bind_rows(Weis_2001_speciesdata) %>%
  bind_rows(Johnson_2007_speciesdata) %>%
  bind_rows(Drexler_2013_speciesdata) %>%
  bind_rows(Drexler_2019_speciesdata) %>%
  bind_rows(Noe_2016_speciesdata)

## ....2g. Bind citation tables ###############
CCRCN_study_citations <- Holmquist_2018_citationdata %>%
  bind_rows(Fourqurean_2012_citationdata) %>%
  bind_rows(Gonneea_2018_citationdata) %>%
  bind_rows(Osland_2016_citationdata) %>%
  bind_rows(Sanderman_2018_citationdata) %>%
  bind_rows(Schile_2017_citationdata) %>%
  bind_rows(Giblin_2018_citationdata) %>%
  bind_rows(Smith_2015_citationdata) %>%
  bind_rows(Trettin_2017_citationdata) %>%
  bind_rows(Thorne_2015_citationdata) %>%
  bind_rows(Drexler_2009_citationdata) %>% 
  bind_rows(Krauss_2018_citationdata) %>%
  bind_rows(Weis_2001_citationdata) %>%
  bind_rows(Johnson_2007_citationdata) %>%
  bind_rows(Drexler_2013_citationdata) %>%
  bind_rows(Drexler_2019_citationdata) %>%
  bind_rows(Noe_2016_citationdata)

bib_file <- CCRCN_study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

## 3. QA #################
source("./scripts/1_data_formatting/qa_functions.R")

# Ensure all core_ids are unique
results_unique_core <- test_unique_cores(CCRCN_coredata)

# There almost 100 sets of coordinates that have two or more cores associated with them: 
write_csv(test_unique_coords(CCRCN_coredata), "./data/QA/duplicate_cores.csv")

# Test column names
test_colnames("core_level", CCRCN_coredata) 
test_colnames("depthseries", CCRCN_depthseriesdata)
test_colnames("site_level", CCRCN_sitedata)
test_colnames("species", CCRCN_speciesdata) 
test_colnames("impact", CCRCN_impactdata) 

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(CCRCN_coredata, CCRCN_depthseriesdata)

test_numeric_vars(CCRCN_depthseriesdata)

## 4. Write datasets #############

write_csv(CCRCN_coredata, "./data/CCRCN_synthesis/CCRCN_core_data.csv")
write_csv(CCRCN_depthseriesdata, "./data/CCRCN_synthesis/CCRCN_depthseries_data.csv")
write_csv(CCRCN_sitedata, "./data/CCRCN_synthesis/CCRCN_site_data.csv")
write_csv(CCRCN_impactdata, "./data/CCRCN_synthesis/CCRCN_impact_data.csv")
write_csv(CCRCN_methodsdata, "./data/CCRCN_synthesis/CCRCN_methods_data.csv")
write_csv(CCRCN_speciesdata, "./data/CCRCN_synthesis/CCRCN_species_data.csv")
write_csv(CCRCN_study_citations, "./data/CCRCN_synthesis/CCRCN_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "data/CCRCN_synthesis/CCRCN_bibliography.bib")

