## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

# Data citation: 
# Schile, Lisa M. and Megonigal, J. Patrick. 2017. [Dataset] 
# "Abu Dhabi Blue Carbon Demonstration Project." Distributed by Smithsonian Environmental Research Center. https://doi.org/10.5479/data_serc/10088/31949


# Publication citation: 
# Schile, L. M., Kauffman, J. B., Crooks, S., Fourqurean, J. W., Glavan, J. and Megonigal, J. P. (2017), Limits on carbon sequestration in
# arid blue carbon ecosystems. Ecol Appl, 27: 859–874. doi:10.1002/eap.1489

## INSTRUCTIONS ####################

# 1. Designate the target webpage to scrape for data
#   Paste the url of the target webpage here, wrapped in quotation marks

URL <- "https://repository.si.edu/handle/10088/31949"


# 2. Name the file
#   Add the names for each file into this list, wrapped in quotation marks, IN 
#   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
#   extension (.csv, .xlsx, etc.) in the name of the file as well.

FILE_NAME <- "Megonigal_J_Patrick-20170103-Abu_Dhabi_Blue_Carbon_Project_Ecological_Applications.xlsx"

# 3. Designate file path of where these data files will go in the CCRCN library
#   Paste the file path here, wrapped in quotation marks. The getwd() function 
#   will automatically detect the working directory of the R project (in the case 
#   of the CCRCN Data library, the location of where this repository is stored on 
#   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
#   with whatever you include within the quotation marks.

FILE_PATH <- paste0(getwd(), "/data/Schile-Beers_2017/original/" )

## Assumptions made about data ###############


## Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(rvest)
library(stringr)
library(RCurl)
library(tidyverse)
library(lubridate)
library(readxl)

## Download data ########################

# The stem of the url should always be the same
BASE_URL <- "https://repository.si.edu"

# Extract and save the url path for each data file embedded on the webpage
  # if using Chrome, html_node and html_attr data can be obtained 
  # using an extension such as "SelectorGadget" (html_node) and the "inspect" option 
  # when right-clicking an element on a webpage (html_attr). 
page <- read_html(URL)
page <- page %>%
  html_nodes('.file-link a') %>% 
  html_attr("href")

# Download the data to your file path 
download.file(paste0(BASE_URL, page), paste0(FILE_PATH, FILE_NAME),mode = "wb")

## Curate data to CCRCN Structure #################

# Read data in
plot_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="plot information")
raw_depthseries_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="soil carbon data")

## Depth series data ###################

# Issues: 
# 1. Two sites have multi-year entries for cores that either do not have matching depth series data 
# or have depth series data but no clear core-level metadata (and therefore no location data) 
# The specific site_ids are: Eastern Mangrove 10 yr, Eastern Mangrove 7 yr, Eastern Mangrove 3 yr,
# Jubail Is. 10 yr, Jubail Is. 7 yr, Jubail Is. 3 yr
# 2. According to the methods in the publication, core depth was to either 3 m (the corer was 1 m long) or until parent material
# There is no clear core_depth_flag code for the former, and I have coded it as "core depth limited by length of corer"

depthseries_data <- raw_depthseries_data %>%
  rename(site_id = "Site", 
         core_length = "total core length (cm)") %>%
  # I will remove the following cores that have no or conflicting core-level entries: 
  filter(site_id != "Jubail Is. 10 yr", site_id != "Jubail Is. 7 yr", site_id != "Jubail Is. 3 yr") %>%
  
  # there are inconsistencies in the spelling and abbreviation of site names between the core- and depth-level data
  # I need to make them consistent to create core_ids that match 
  mutate(site_id = gsub("Thimiriya", "Thumayriyah", site_id)) %>%
  mutate(site_id = gsub("Is.", "Island", site_id)) %>%
  mutate(site_id = gsub("Al Zorah", "Ajman Al Zorah", site_id)) %>%
  mutate(site_id = gsub(" Al ", " al ", site_id)) %>%
  
  # Core IDs are expressed as factor not numeric
  # Paste site, ecosystem and plot values to create a unique core ID 
  mutate(core_id = as.factor(gsub(" ", "_", paste(site_id, paste(Ecosystem, plot, sep="_"), sep="_")))) %>%
  # I will remove the following core that has no core-level entry: 
  filter(core_id != "Al_Dabiya_1_seagrass_2") %>%
  
  rename(dry_bulk_density = "dry bulk density (g/cm3)") %>%
  rename(fraction_organic_matter = "% organic carbon (OC)") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100) %>%
  separate(col="depth (cm)", into=c("depth_min", "depth_max"), sep="-") %>%
  mutate(study_id = "Schile-Beers_and_Megonigal_2017") %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, core_length) %>%
  mutate(depth_min = ifelse(is.na(depth_max==TRUE),100,depth_min)) %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  
  # The depth (cm) category does not provide depth_max if it's entered as >100. 
  # However, the first entry for each core provides a core length.
  # I'll group by core_id to acquire the core_length for each core and associate it with depth_max
  group_by(core_id) %>%
  mutate(core_length = ifelse(is.na(core_length), 0, core_length)) %>%
  mutate(core_length = sum(core_length)) %>%
  mutate(depth_max = ifelse(is.na(depth_max)==TRUE,core_length, depth_max)) %>%
  select(-core_length)

# The depth (cm) category does not provide depth_max if it's entered as >100. 
# However, the first entry for each core provides a core length. I'll summarize the data to get that information, 
# then join it back 


## Core data ####################

core_data <- plot_data %>%
  rename(site_id = "Site") %>%
  # I will remove the following cores that have no or conflicting depth series-level entries: 
  filter(site_id != "Eastern Mangrove 10 yr", site_id != "Eastern Mangrove 7 yr", site_id != "Eastern Mangrove 3 yr") %>%
  # There is a typo in the ecosystem column: plated mangrove should be planted mangrove
  mutate(Ecosystem = ifelse(Ecosystem=="plated mangrove", "planted mangrove", Ecosystem)) %>%
  # there are inconsistencies in the spelling and abbreviation of site names between the core- and depth-level data
  # I need to make them consistent to create core_ids that match 
  mutate(site_id = gsub("Jaoub", "Janoub", site_id)) %>%
  mutate(site_id = gsub("Bazam", "Basm", site_id)) %>%
  mutate(site_id = gsub("Kalba", "Khalba", site_id)) %>%
  mutate(site_id = gsub(" Al ", " al ", site_id)) %>%
  # Core IDs are expressed as factor not numeric
  # Paste site, ecosystem, and plot values to create a unique core ID 
  mutate(core_id = as.factor(gsub(" ", "_", paste(site_id, paste(Ecosystem, Plot, sep="_"), sep="_")))) %>%
  mutate(core_date = as.Date(as.numeric(Date), origin="1899-12-30")) %>%
  rename(vegetation_notes = "Ecosystem") %>%
  rename(XYZ = "XYZ source") %>%
  mutate(core_position_method = ifelse(XYZ == "RTK GPS", "RTK", 
                                ifelse(XYZ == "Garmin GPS", "handheld", NA))) %>%
  mutate(core_elevation_method = ifelse(XYZ == "RTK GPS", "RTK", NA)) %>%
  rename(core_latitude = "Latitude", core_longitude = "Longitude") %>%
  rename(core_elevation = "elevation") %>%
  mutate(study_id = "Schile-Beers_and_Megonigal_2017") %>%
  rename(core_depth = "core depth (cm)") %>%
  mutate(core_depth_flag = ifelse(core_depth<300, "core depth represents deposit depth", 
                                  ifelse(core_depth==300, "core depth limited by length of corer", NA))) %>%
  mutate(salinity_class = ifelse(salinity > 50, "brine", 
                                 ifelse(salinity < 51 & salinity > 29, "saline", "brackish"))) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, 
        core_position_method, core_elevation, core_elevation_method, vegetation_notes)


## Site level data #############

# We're going to need to add a few new columns, and aggregate out core level
#   data up to the site level

# Change value of digits so we don't have too many for the next step
options(digits=6)

# Rename and curate
site_data <- core_data %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude, core_elevation, vegetation_notes) 
  
# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_data_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_data_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)
# remove NAs before aggregation
site_data <- na.omit(site_data)

# Now aggeregate data to the site level
site_data <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id),  
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min),
            country = "United Arab Emirates")


## QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", core_data)
test_colnames("sites", site_data) # country is not in the CCRCN guidelines
test_colnames("depthseries", depthseries_data)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(core_data, depthseries_data)

# Write data
write.csv(site_data, "./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_sites.csv")
write.csv(core_data, "./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_cores.csv")
write.csv(depthseries_data, "./data/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_depthseries.csv")
