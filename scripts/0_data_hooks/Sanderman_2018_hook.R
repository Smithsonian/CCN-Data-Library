# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)
library(RCurl)
library(readxl)
library(lubridate)

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
internatl_depthseries_data_new <- read_excel("./data/Sanderman_2018/original/RC_sites.xlsx")

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

## * International study level metadata ################
internatl_study_metadata <- internatl_core_data_raw %>%
  rename(email = "Data_owner") %>%
  rename(study_id = Source) %>%
  mutate(study_id = gsub(" ", "_",  study_id)) %>%
  select(study_id, email)

## * International core data ###############

# Rename attributes
internatl_core_data <- internatl_core_data_raw %>%
  rename(study_id = Source) %>%
  mutate(study_id = gsub(" ", "_",  study_id)) %>%
  rename(site_id = Location) %>%
  rename(core_id = `Site #`) %>%
  rename(country = Country) %>%
  rename(core_latitude = Latitude) %>%
  rename(core_longitude = Longitude) %>%
  rename(core_position_accuracy_flag = "accuracy_flag") %>%
  rename(core_position_accuracy = "approx_acc") %>%
  mutate(vegetation_class = "forested") %>%
  mutate(vegetation_notes = "mangrove") %>%
  rename(landscape_position = "Landscape position") %>%
  rename(core_date = Years_collected) %>%
  mutate(core_date = as.Date(core_date, format = "%Y")) %>%
  rename(core_length = "total_depth") %>%
  rename(core_length_flag = "full_profile")

# Work up year data
# For some reason the 'N/A' coding is breaking recode(), so looks like I'll need
#   to loop through instead
for (i in 1:nrow(internatl_core_data)) {
  if (internatl_core_data$core_date[[i]] == "N/A") {
    internatl_core_data$core_date[[i]] <- NA
  }
}

internatl_core_data <- internatl_core_data %>%
  select(-`Landscape Unsure`, -`HGM Unsure`, -`Mangrove type`) %>%
  mutate(core_length = as.numeric(core_length)) %>%
  select(- `Site name`) %>%
  select(study_id, country, site_id, core_id, core_latitude, core_longitude, core_date, everything())

internatl_core_data$core_length_flag <- recode(internatl_core_data$core_length_flag, 
                                                        "Y" = "core depth represents deposit depth",
                                                        "N" = "core depth limited by length of corer",
                                                        "unsure" = "not specified",
                                                        "unknown" = "not specified",
                                                        "Not sure" = "not specified")

## * International species data ##############
internatl_species_data <- internatl_core_data %>%
  select(study_id, site_id, core_id, `Dominant species`) %>%
  rename(species_code = "Dominant species")

## * International methods data ############

internatl_methods_data <- internatl_core_data %>%
  group_by(study_id) %>%
  summarize(country = first(country), n=n())

## * international depthseries data ####################
internatl_depthseries_data <- internatl_depthseries_data_raw %>%
  rename(site_id = "Site name") %>%
  rename(core_id = "Site #") %>%
  rename(depth_min = "U_depth") %>%
  mutate(depth_min = 100 * as.numeric(depth_min)) %>%
  rename(depth_max = "L_depth") %>%
  mutate(depth_max = 100 * as.numeric(depth_max)) %>%
  rename(BD_est = "BD_estimated")

# Add in study IDs from international core data
internatl_depthseries_data <- internatl_core_data %>%
  select(study_id, core_id) %>%
  right_join(internatl_depthseries_data)

# Sanderman reports the measured BD (if measured), the modeled BD, and then the 
#   "final" BD-- the value chosen. We'll just use the final, and create a flag
#   describing whether it was measured or modeled
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(BD_final != BD_est) %>%
  mutate(DBD_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(BD_final == BD_est) %>%
  mutate(DBD_measured_or_modeled = "modeled")

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
internatl_depthseries_data <- internatl_depthseries_data %>%
  # assumption: bulk density stands for dry bulk density
  rename(dry_bulk_density = "BD_final") %>%
  # convert from [Mg m-3] to [g cm-3]...which ends up being just x 1
  mutate(dry_bulk_density = as.numeric(dry_bulk_density) * 10^6 / 10^6) %>%
  select(-BD_reported, -BD_est) %>%
  rename(fraction_organic_matter = "SOM (%LOI)") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100)

# Similar as bulk density...organic carbon this time
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(`TOC (%)` == OC_final) %>%
  mutate(OC_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(`TOC (%)` != OC_final | is.na(`TOC (%)`)) %>%
  mutate(OC_measured_or_modeled = "modeled")

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Similar as bulk density and organic carbon...carbon density this time
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(CD_calc == CD_reported) %>%
  mutate(CD_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(CD_calc != CD_reported | is.na(CD_reported)) %>%
  mutate(CD_measured_or_modeled = "modeled")

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
internatl_depthseries_data <- internatl_depthseries_data %>%
  rename(carbon_density = "CD_calc") %>%
  rename(fraction_carbon = OC_final) %>%
  mutate(fraction_carbon = as.numeric(fraction_carbon) / 100) %>%
  rename(fraction_carbon_type = "TC instead of TOC?") %>%
  mutate(carbon_profile_notes = "uses OC data if reported, otherwise used 0.5*LOI data")

# Recode fraction carbon type from yes/no to more descriptive
internatl_depthseries_data$fraction_carbon_type <- recode(internatl_depthseries_data$fraction_carbon_type,
                                                          Y = "total carbon",
                                                          N = "organic carbon")

# NOTE ASSUMPTION: "total carbon" rows include both those studies that designate that 
#   total carbon was measured, AND those studies that did not designate whether
#   total carbon or organic carbon was measured (i.e. did not mention whether
#   carbonates were removed)

# Remove unwanted attributes
internatl_depthseries_data <- internatl_depthseries_data %>%
  select(-mid_point, -`TOC (%)`)
  
## Curate Sanderman USA data SEE NOTE BELOW: US DATA NOT UNIQUE WITH INTERNATIONAL ############

# It looks like apparently all of the US cores are represented in the international
#   data...so I'm not going to bind the international and US below and am going to
#   comment out all US processing
# 
# ## * US total data ###############
# # This excel document was handed off in a personal communication
# US_total_data_raw <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
#                               sheet = 2)
# 
# # Rename attributes
# US_total_data <- US_total_data_raw %>%
#   rename(study_id = Source) %>%
#   mutate(study_id = gsub(" ", "_", study_id)) %>%
#   rename(site_id = Location) %>%
#   mutate(site_id = gsub(" ", "_", site_id)) %>%
#   rename(core_id = "Site #") %>%
#   rename(country = Country) %>%
#   rename(core_latitude = Latitude) %>%
#   rename(core_longitude = Longitude) %>%
#   rename(core_position_accuracy_flag = accuracy_flag) %>%
#   rename(core_position_accuracy = approx_acc) %>%
#   mutate(vegetation_class = "forested") %>%
#   mutate(vegetation_notes = "mangrove") %>%
#   rename(landscape_position = "Landscape position") %>%
#   rename(core_date = Years_collected) %>%
#   mutate(core_date = as.factor(core_date)) %>%
#   rename(core_length = total_depth) %>%
#   rename(core_length_flag = full_profile) %>%
#   select(-`Landscape Unsure`, -`HGM Unsure`, -`Mangrove type`)
# 
# US_total_data$core_length_flag <- recode(US_total_data$core_length_flag, 
#                                      Y = "core depth represents deposit depth",
#                                      N = "core depth limited by length of corer")
# 
# ## * US core level data ##############
# 
# US_core_data <- US_total_data %>%
#   select(study_id, site_id, core_id, country, core_latitude, core_longitude, 
#          core_position_accuracy_flag, core_position_accuracy, core_date, core_length, 
#          core_length_flag, vegetation_class, vegetation_notes, landscape_position, 
#          `Dominant species`)
# 
# ## * US species data #############
# 
# US_species_data <- US_total_data %>%
#   select(study_id, site_id, core_id, `Dominant species`) %>%
#   rename(species_code = "Dominant species")
# 
# # Species were input with multiple species per row. Split up so that each species
# #   entry gets a row
# species_data<- species_data %>% 
#   mutate(species_code = strsplit(as.character(species_code), ",")) %>% 
#   unnest(species_code)
# 
# ## * US methods data #############
# 
# US_methods_data <- US_core_data %>%
#   select(study_id, country, site_id, core_id, core_latitude, core_longitude, core_date, 
#          core_position_accuracy_flag)
# 
# ## * US study level metadata ###############
# 
# US_study_metadata <- US_total_data %>%
#   rename(email = "Data_owner") %>%
#   select(study_id, email)
# 
# ## * US depth series data ##############
# 
# # Read in the "horizon" sheet of "mangroves for USA" excel book
# US_depthseries_data_raw <- read_excel("./data/Sanderman_2018/original/mangroves for USA.xlsx",
#                                  sheet = 3)
# 
# # Removed first row, which displays units
# colnames(US_depthseries_data_raw) <- US_depthseries_data_raw[1, ] # the first row will be the header
# US_depthseries_data_raw <- US_depthseries_data_raw[-1, ]
# 
# # Remove last few columns, which are dedicated to notes. The only note is 
# #   "No %OC given" for row 238 (M0340, deepest depth), which is already apparent
# #   (the OC column is empty)
# 
# US_depthseries_data_raw <- US_depthseries_data_raw[, 1:18]
# 
# 
# # Rename and recalculate attributes
# US_depthseries_data <- US_depthseries_data_raw %>%
#   rename(site_id = "Site name") %>%
#   rename(core_id = "Site #") %>%
#   rename(depth_min = "U_depth") %>%
#   mutate(depth_min = 100 * as.numeric(depth_min)) %>%
#   rename(depth_max = "L_depth") %>%
#   mutate(depth_max = 100 * as.numeric(depth_max)) %>%
#   rename(BD_est = "BD_new est")
# 
# # Add in study IDs from international core data
# US_depthseries_data <- US_total_data %>%
#   select(study_id, core_id) %>%
#   right_join(US_depthseries_data)
# 
# # Sanderman reports the measured BD (if measured), the modeled BD, and then the 
# #   "final" BD-- the value chosen. We'll just use the final, and create a flag
# #   describing whether it was measured or modeled
#   
# US_depthseries_data_measured <- US_depthseries_data %>%
#   filter(BD_final != BD_est) %>%
#   mutate(DBD_measured_or_modeled = "measured")
# 
# US_depthseries_data_modeled <- US_depthseries_data %>%
#   filter(BD_final == BD_est) %>%
#   mutate(DBD_measured_or_modeled = "modeled")
# 
# US_depthseries_data <- US_depthseries_data_measured %>%
#   bind_rows(US_depthseries_data_modeled)
# 
# # Back to renaming and recalculating attributes
# US_depthseries_data <- US_depthseries_data %>%
#   # assumption: bulk density stands for dry bulk density
#   rename(dry_bulk_density = "BD_final") %>%
#   # convert from [Mg m-3] to [g cm-3]...which ends up being just x 1
#   mutate(dry_bulk_density = as.numeric(dry_bulk_density) * 10^6 / 10^6) %>%
#   select(-BD_reported, -BD_est) %>%
#   rename(fraction_organic_matter = "SOM") %>%
#   mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100)
#   
#  
# # Similar as bulk density...with organic carbon this time
# US_depthseries_data_measured <- US_depthseries_data %>%
#   filter(OC_final == OC) %>%
#   mutate(OC_measured_or_modeled = "measured")
# 
# US_depthseries_data_modeled <- US_depthseries_data %>%
#   filter(OC_final != OC | is.na(OC)) %>%
#   mutate(OC_measured_or_modeled = "modeled")
# 
# US_depthseries_data <- US_depthseries_data_measured %>%
#   bind_rows(US_depthseries_data_modeled)
# 
# # Back to renaming and recalculating attributes
# US_depthseries_data <- US_depthseries_data %>%
#   rename(fraction_carbon = "OC_final") %>%
#   mutate(fraction_carbon = as.numeric(fraction_carbon) / 100) %>%
#   # assumption: I don't know what TN means, but this appears to be some form of
#   #   nitrogren...
#   rename(fraction_nitrogen = "TN") %>%
#   mutate(fraction_nitrogen = as.numeric(fraction_nitrogen) / 100)
# 
# # Similar as bulk density and organic carbon...carbon density this time
# US_depthseries_data_measured <- US_depthseries_data %>%
#   filter(CD_calc == CD_reported) %>%
#   mutate(CD_measured_or_modeled = "measured")
# 
# US_depthseries_data_modeled <- US_depthseries_data %>%
#   filter(CD_calc != CD_reported | is.na(CD_reported)) %>%
#   mutate(CD_measured_or_modeled = "modeled")
# 
# US_depthseries_data <- US_depthseries_data_measured %>%
#   bind_rows(US_depthseries_data_modeled)
# 
# # Back to renaming and recalculating attributes
# US_depthseries_data <- US_depthseries_data %>%
#   rename(carbon_density = "CD_calc") %>%
#   rename(core_date = "Year_sampled") %>%
#   rename(fraction_carbon_type = "TC instead of TOC?") %>%
#   mutate(carbon_profile_notes = "uses OC data if reported, otherwise used 0.5*LOI data")
# 
# # Recode fraction carbon type from yes/no to more descriptive
# US_depthseries_data$fraction_carbon_type <- recode(US_depthseries_data$fraction_carbon_type,
#                                                           Y = "total carbon",
#                                                           N = "organic carbon")
# 
# # NOTE ASSUMPTION: "total carbon" rows include both those studies that designate that 
# #   total carbon was measured, AND those studies that did not designate whether
# #   total carbon or organic carbon was measured (i.e. did not mention whether
# #   carbonates were removed)
# 
# # Remove unwanted attributes
# US_depthseries_data <- US_depthseries_data %>%
#   select(-OC, -CD_reported, -OC_stock_reported, -Age, -mid_point)
# 
# # Change data types where necessary
# US_depthseries_data <- US_depthseries_data %>%
#   mutate(carbon_density = as.numeric(carbon_density))
#   

## Write data ###############

write_csv(internatl_study_metadata, "./data/Sanderman_2018/derivative/Sanderman_2018_study_metadata.csv")

write_csv(internatl_core_data, "./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")

write_csv(internatl_species_data, "./data/Sanderman_2018/derivative/Sanderman_2018_species_data.csv")

write_csv(internatl_methods_data, "./data/Sanderman_2018/derivative/Sanderman_2018_methods_data.csv")

write_csv(internatl_depthseries_data, "./data/Sanderman_2018/derivative/Sanderman_2018_depthseries_data.csv")

## DOCUMENT AND FILTER UNPUBLISHED OR UN-CITED STUDIES ###################
# read back in hooked and curated Sanderman data
cores <- read.csv("./data/Sanderman_2018/derivative/Sanderman_2018_core_data.csv")
depthseries <- read.csv("./data/Sanderman_2018/derivative/Sanderman_2018_depthseries_data.csv")
species <- read.csv("./data/Sanderman_2018/derivative/Sanderman_2018_species_data.csv")

## ... Determine which studies are cited in biblio ###########

# Import CCRCN bibliography and create a list of all study IDs within Sanderman synthesis 
# that are cited within the bibliography 

library(bib2df)
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

studies_cited <- cores %>%
  group_by(study_id) %>%
  summarize(n=n()) %>%
  filter(study_id %in% CCRCN_bib$BIBTEXKEY)

# Exactly 100 studies are cited in the bibliography

studies_not_cited <- cores %>%
  group_by(study_id) %>%
  summarize(n=n()) %>%
  filter(!(study_id %in% CCRCN_bib$BIBTEXKEY))

# 55 studies are not cited in the bibliography 

## ... Filter out un-cited studies from depthseries ########## 

cleaned_depthseries <- depthseries %>%
  filter(study_id %in% studies_cited$study_id)

write_csv(cleaned_depthseries, "./data/Sanderman_2018/derivative/Sanderman_2018_depthseries_data.csv")

## ... Create vector of un-cited studies ###########
studies_Sanderman_unavailable <- as.character(studies_not_cited$study_id)

# This list of studies is listed as unavailable in "carbon stocks" within the map interface
# Modify them within the "2_datatype_processing.R" script in the map repository scripts folder

## Generate study citation table ###################
# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Sanderman 2018)
synthesis_doi <- "10.7910/dvn/ocyuit"
synthesis_study_id <- "Sanderman_2018"

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
  select(study_id, study_type, bibliography_id, doi) 

study_data <- study_data %>%
  filter(study_id %in% study_data_primary$study_id) %>%
  bind_rows(study_data_primary)

# write 
write_csv(study_data, "./data/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")
