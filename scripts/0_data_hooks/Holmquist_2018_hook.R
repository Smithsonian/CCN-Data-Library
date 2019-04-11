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

## NOTE: this section commented out, but kept, because the data for the Holmquist
# 2018 Sci Reports data release does not include site IDs. If and when this data
# release is fixed we'll re-incorporate these lines, until then using internal
# data files

# # Create a list of the URLs for each data file
# url_list <- list("https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_core_data.csv?sequence=7&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_depth_series_data.csv?sequence=8&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_impact_data.csv?sequence=9&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_methods_data.csv?sequence=10&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_species_data.csv?sequence=11&isAllowed=y"
# )
# 
# # Apply a function, which downloads each of the data files, over url_list
# lapply(url_list, function(x) {
#   # Extract the file name from each URL
#   filename <- as.character(x)
#   filename <- substring(filename, 56)
#   filename <- gsub("\\..*","", filename)
#   # Now download the file into the "data" folder
#   download.file(x, paste0(getwd(), "./data/Holmquist_2018/", filename, ".csv"))
# })

## 2. Import data to convert codes to common plain language ####

cores <- read.csv("./data/Holmquist_2018/original/V1_Holmquist_2018_core_data.csv")
depthseries <- read.csv("./data/Holmquist_2018/original/V1_Holmquist_2018_depth_series_data.csv")
impacts <-read.csv("./data/Holmquist_2018/original/V1_Holmquist_2018_impact_data.csv")
species <- read.csv("./data/Holmquist_2018/original/V1_Holmquist_2018_species_data.csv")
methods <- read.csv("./data/Holmquist_2018/original/V1_Holmquist_2018_methods_data.csv")

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
         salinity_class = "salinity_code",
         core_position_notes= "position_code") %>%
  # recode core position notes 
  mutate(core_position_notes = recode(core_position_notes, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure"), 
         inundation_class = tolower(inundation_class)) %>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  # Add underscores to site IDs
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  recode_salinity(salinity_class = salinity_class) %>%
  recode_vegetation(vegetation_class = vegetation_class) %>%
  # There's a typo with Galveston Bay sites
  mutate(site_id = recode(site_id, "Gavelston_Bay" = "Galveston_Bay"))

depthseries <- depthseries %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(study_id != "Gonneea_et_al_2018")

# Fraction carbon type should be in the methods metadata, not depthseries level 
fraction_carbon_type_metadata <- depthseries %>%
  group_by(study_id) %>%
  summarize(fraction_carbon_type = first(fraction_carbon_type)) 
depthseries <- select(depthseries, -fraction_carbon_type)

impacts <- impacts %>%
  rename(impact_class = "impact_code") %>%
  # The Crooks study ID should be 2014, not 2013. 
  recode_impact(impact_class = impact_class)%>%
  filter(study_id != "Gonneea_et_al_2018")

species <- species %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  recode_species(species_code = species_code)

methods <- methods %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"))%>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  select(-n) %>%
  merge(fraction_carbon_type_metadata, by="study_id")

## 4. Create study-level data ######
# import the CCRCN bibliography 
library(bib2df)
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Holmquist et al. 2018)
synthesis_doi <- "10.25572/ccrcn/10088/35684"
synthesis_study_id <- "Holmquist_et_al_2018"

# link each study to the synthesis 
study_data <- cores %>%
  group_by(study_id) %>%
  summarize(study_type = "synthesis",
            bibliography_id = synthesis_study_id, 
            doi = synthesis_doi)

# link each study to primary citation and join with synthesis table
studies <- unique(cores$study_id)

study_data_primary <- CCRCN_bib %>%
  select(BIBTEXKEY, CATEGORY, DOI) %>%
  rename(bibliography_id = BIBTEXKEY,
         study_type = CATEGORY,
         doi = DOI) %>%
  filter(bibliography_id %in% studies) %>%
  mutate(study_id = bibliography_id, 
         study_type = tolower(study_type)) %>%
  select(study_id, study_type, bibliography_id, doi) %>%
  bind_rows(study_data)

## 5. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores) 
test_colnames("depthseries", depthseries)
test_colnames("species", species) 
test_colnames("impact", impacts) 

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 6. Write to folder ########
write_csv(cores, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
write_csv(depthseries, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
write_csv(impacts, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
write_csv(species, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
write_csv(methods, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
write_csv(study_data_primary, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_study_citations.csv")
