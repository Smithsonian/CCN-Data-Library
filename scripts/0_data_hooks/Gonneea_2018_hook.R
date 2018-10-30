## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Gonneea et al 2018

## Assumptions made about data ###############

# that lat and long is in WGS84


## Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(RCurl)
library(tidyverse)
library(lubridate)

## Download data ########################


# The Gonneea et al (2018) data release features a diverse suite of file types:
#   a .jpg, a .xlsx, a .csv, and a .xml
# So we'll need to have a custom hook for each file

url_list <- list("https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__6f%2F73%2F4b%2F6f734b0239c27f78c7f347dcf277c491a4a47903",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__7d%2Fe4%2Fdc%2F7de4dc002db596e1d7fbe8254de9ccc3af05ae3b",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__70%2F1e%2F99%2F701e99829e5860c1a0dc512056e5d71ff292dc19",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__3e%2F2d%2Ff5%2F3e2df544c537a35007214d1fe595b45499df2f4a")

# Extract Saltmarsh_AR.jpg
download.file(url_list[[1]], paste0(getwd(), "./data/Gonneea_2018/original/Saltmarsh_AR.jpg"),
              mode = "wb")

# Extract Waquoit_Core_data_release.xlsx
download.file(url_list[[2]], paste0(getwd(), "./data/Gonneea_2018/original/Waquoit_Core_data_release.xlsx"), 
              mode = "wb")

# Extract Waquoit_Core_data_release.csv
download.file(url_list[[3]], paste0(getwd(), "./data/Gonneea_2018/original/Waquoit_Core_data_release.csv"),
              mode = "wb")

# Extract Waquoit_Core_data_release_meta.xml
download.file(url_list[[4]], paste0(getwd(), "./data/Gonneea_2018/original/Waquoit_Core_data_release_meta.xml"),
              mode = "wb")


## Curate data to CCRCN Structure ########################

# Import data file into R
Gonneea_2018 <- read_csv("./data/Gonneea_2018/original/Waquoit_Core_data_release.csv", 
                         col_names = TRUE)

# Change column names to values of first row
# Why? Because the top 2 rows were both dedicated to column headers
new_colnames <- c(Gonneea_2018 %>%
  slice(1))
colnames(Gonneea_2018) <- new_colnames
Gonneea_2018 <- Gonneea_2018 %>%
  slice(2:561)

# Change all no data values to "NA"
Gonneea_2018 <- Gonneea_2018 %>%
  na_if(-99999) # Changes all "-99999" values to "NA"

# Rename attributes that are already properly formatted
Gonneea_2018 <- Gonneea_2018 %>%
  rename(core_id = "ID") %>%
  rename(core_date = "Date") %>%
  rename(core_latitude = "Lat") %>%
  rename(core_longitude = "Lon") %>%
  rename(dry_bulk_density = "DBD") %>%
  rename(age = "Age") %>%
  rename(total_pb210_activity = "210Pb") %>%
  rename(ra226_activity = "226Ra") %>%
  rename(excess_pb210_activity = "210Pbex") %>%
  rename(cs137_activity = "137Cs") %>%
  rename(be7_activity = "7Be")
  
# Add study_id
Gonneea_2018$study_id <- "Gonneea_2018"

# Some columns are characters when they should be numeric
Gonneea_2018 <- Gonneea_2018 %>%
  mutate(core_latitude = as.numeric(core_latitude)) %>%
  mutate(core_longitude = as.numeric(core_longitude)) %>%
  mutate(dry_bulk_density = as.numeric(dry_bulk_density)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(total_pb210_activity = as.numeric(total_pb210_activity)) %>%
  mutate(ra226_activity = as.numeric(ra226_activity)) %>%
  mutate(excess_pb210_activity = as.numeric(excess_pb210_activity)) %>%
  mutate(cs137_activity = as.numeric(cs137_activity)) %>%
  mutate(be7_activity = as.numeric(be7_activity))
  
## Curate attributes that need some a'fixin'

# Change core_date column to date objects
Gonneea_2018$core_date <- as.Date(as.numeric(Gonneea_2018$core_date), 
                                  origin = "1899-12-30")

# Convert mean interval depth to min and max interval depth
source("./scripts/1_data_formatting/curation_functions.R") # Call functions from
# curation_functions script
Gonneea_2018 <- convert_mean_depth_to_min_max(Gonneea_2018, Gonneea_2018$Depth)

# Convert dpm/g to becquerel/kg
Gonneea_2018 <- Gonneea_2018 %>%
  mutate(total_pb210_activity = convert_dpm_g_to_bec_kg(total_pb210_activity)) %>%
  mutate(ra226_activity = convert_dpm_g_to_bec_kg(ra226_activity)) %>%
  mutate(excess_pb210_activity = convert_dpm_g_to_bec_kg(excess_pb210_activity)) %>%
  mutate(cs137_activity = convert_dpm_g_to_bec_kg(cs137_activity))
    
# Convert percent weights to fractions
Gonneea_2018 <- Gonneea_2018 %>%
  mutate(fraction_carbon = convert_percent_to_fraction(wtC)) %>%
  mutate(fraction_nitrogen = convert_percent_to_fraction(wtN))
  
## Parcel data into separate files according to data level #################
  
# Core data

# Gonneea elevation is calculated for each depth interval. We only want elevation
#   at the top of the core
core_elevation <- Gonneea_2018 %>%
  group_by(core_id) %>%
  summarize(core_elevation = max(as.numeric(Elevation)))
  
  Gonneea_2018_core_Data <- Gonneea_2018 %>%
    group_by(study_id, core_id, core_date) %>%
    summarize_at(c("core_latitude","core_longitude"), mean) %>%
    left_join(core_elevation)
  
  # Depth Series data
  Gonneea_2018_depth_series_data <- Gonneea_2018 %>%
    select(study_id, core_id, depth_min, depth_max, dry_bulk_density, 
           fraction_carbon, cs137_activity, total_pb210_activity, ra226_activity,
           excess_pb210_activity, be7_activity, age) %>%
    filter(depth_min != depth_max)

## Export files ##############################
  
# Export core data
write_csv(Gonneea_2018_core_Data, "./data/Gonneea_2018/derivative/Gonneea_2018_core_Data.csv")

# Export depth series data
write_csv(Gonneea_2018_depth_series_data, "./data/Gonneea_2018/derivative/Gonneea_2018_depth_series_data.csv")
  
# Export master data
write_csv(Gonneea_2018, "./data/Gonneea_2018/derivative/Gonneea_2018.csv")


