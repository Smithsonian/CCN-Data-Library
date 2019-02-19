## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Holmquist et al 2018 Scientific Reports
#   data release. 

# Holmquist, James R., Windham-Myers, Lisamarie, Bliss, Norman, Crooks, Stephen, Morris, James T., Megonigal, 
# J. Patrick, Troxler, Tiffany, Weller, Donald, Callaway, John, Drexler, Judith, Ferner, Matthew C., Gonneea, 
# Meagan E., Kroeger, Kevin D., Schile-Beers, Lisa, Woo, Isa, Buffington, Kevin, Boyd, Brandon M., Breithaupt, 
# Joshua, Brown, Lauren N., Dix, Nicole, Hice, Lyndie, Horton, Benjamin P., MacDonald, Glen M., Moyer, 
# Ryan P., Reay, William et al. 2018. 
# [Dataset] "Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States: 
# Public Soil Carbon Data Release." Distributed by Smithsonian Research Online.


## 1. Download data ################
# Load RCurl, a package used to download files from a URL
library(RCurl)
library(tidyverse)

# Create a list of the URLs for each data file
url_list <- list("https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_core_data.csv?sequence=7&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_depth_series_data.csv?sequence=8&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_impact_data.csv?sequence=9&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_methods_data.csv?sequence=10&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_species_data.csv?sequence=11&isAllowed=y"
)

# Apply a function, which downloads each of the data files, over url_list
lapply(url_list, function(x) {
  # Extract the file name from each URL
  filename <- as.character(x)
  filename <- substring(filename, 56)
  filename <- gsub("\\..*","", filename)
  # Now download the file into the "data" folder
  download.file(x, paste0(getwd(), "./data/Holmquist_2018/", filename, ".csv"))
})

## 2. Import data to convert codes to common plain language ####

cores <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_core_data.csv")
depthseries <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_depth_series_data.csv")
impacts <-read.csv("./data/Holmquist_2018/V1_Holmquist_2018_impact_data.csv")
species <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_species_data.csv")
methods <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_methods_data.csv")

## 3. Recode and rename factors #################

# Pull from curation functions script
source("./scripts/1_data_formatting/curation_functions.R")

cores <- cores %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  rename(vegetation_class = "vegetation_code",
         salinity_class = "salinity_code")

depthseries <- depthseries %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"))

impacts <- impacts %>%
  rename(impact_class = "impact_code") %>%
  # The Crooks study ID should be 2014, not 2013. 
  recode_impact(impact_class = impact_class)

species <- species %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"))

methods <- methods %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"))

## 4. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores) 
test_colnames("depthseries", depthseries)
test_colnames("species", species) 
test_colnames("impacts", impacts) # impact_code should be impact_class as per CCRCN guidance

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 5. Write to folder ########
write.csv(cores, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
write.csv(depthseries, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
write.csv(impacts, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
write.csv(species, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
write.csv(methods, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
