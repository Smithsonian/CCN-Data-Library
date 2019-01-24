## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

# Data citation: 
# Schile, Lisa M. and Megonigal, J. Patrick. 2017. [Dataset] 
# "Abu Dhabi Blue Carbon Demonstration Project." Distributed by Smithsonian Environmental Research Center. https://doi.org/10.5479/data_serc/10088/31949


# Publication citation: 
# Schile, L. M., Kauffman, J. B., Crooks, S., Fourqurean, J. W., Glavan, J. and Megonigal, J. P. (2017), Limits on carbon sequestration in
# arid blue carbon ecosystems. Ecol Appl, 27: 859–874. doi:10.1002/eap.1489

# This is a template web scraper for Smithsonian DSpace data files

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

FILE_PATH <- paste0(getwd(), "/data/Schile-Beers_etal_2017/original/" )

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
Schile_2017_plot_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="plot information")
Schile_2017_depth_series_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="soil carbon data")

## Depth series data ###################

# Call functions from curation_functions script
source("./scripts/1_data_formatting/curation_functions.R") 

# Issues: 
# 1. Two sites have multi-year entries for cores that either do not have matching depth series data 
# or have depth series data but no clear core-level metadata (and therefore no location data) 
# The specific site_ids are: Eastern Mangrove 10 yr, Eastern Mangrove 7 yr, Eastern Mangrove 3 yr,
# Jubail Is. 10 yr, Jubail Is. 7 yr, Jubail Is. 3 yr
# 2. According to the methods in the publication, core depth was to either 3 m (the corer was 1 m long) or until parent material
# There is no clear core_depth_flag code for the former, and I have coded it as "core depth limited by length of corer"

Schile_2017_depth_series_data <- Schile_2017_depth_series_data %>%
  rename(site_id = "Site") %>%
  # I will remove the following cores that have no or conflicting core-level entries: 
  filter(site_id != "Jubail Is. 10 yr", site_id != "Jubail Is. 7 yr", site_id != "Jubail Is. 3 yr") %>%
  # Core IDs are expressed as factor not numeric
  # Paste site, ecosystem and plot values to create a unique core ID 
  mutate(core_id = as.factor(gsub(" ", "_", paste(site_id, paste(Ecosystem, plot, sep="_"), sep="_")))) %>%
  rename(dry_bulk_density = "dry bulk density (g/cm3)") %>%
  rename(fraction_organic_matter = "% organic carbon (OC)") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100) %>%
  separate(col="depth (cm)", into=c("min_depth", "max_depth"), sep="-") %>%
  mutate(study_id = "Schile-Beers_etal_2017") %>%
  select(study_id, site_id, core_id, min_depth, max_depth, dry_bulk_density, fraction_organic_matter)

# Read out depth series data
write.csv(Schile_2017_depth_series_data, "./data/Schile-Beers_etal_2017/derivative/Schile-Beers_etal_2017_depth_series_data.csv")

## Core data ####################

Schile_2017_core_data <- Schile_2017_plot_data %>%
  rename(site_id = "Site") %>%
  # I will remove the following cores that have no or conflicting depth series-level entries: 
  filter(site_id != "Eastern Mangrove 10 yr", site_id != "Eastern Mangrove 7 yr", site_id != "Eastern Mangrove 3 yr") %>%
  # There is a typo in the ecosystem column: plated mangrove should be planted mangrove
  mutate(Ecosystem = ifelse(Ecosystem=="plated mangrove", "planted mangrove", Ecosystem)) %>%
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
  mutate(study_id = "Schile-Beers_etal_2017") %>%
  rename(core_depth = "core depth (cm)") %>%
  mutate(core_depth_flag = ifelse(core_depth<300, "core depth represents deposit depth", 
                                  ifelse(core_depth==300, "core depth limited by length of corer", NA))) %>%
  mutate(salinity_class = ifelse(salinity > 50, "brine", 
                                 ifelse(salinity < 51 & salinity > 29, "saline", "brackish"))) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, 
        core_position_method, core_elevation, core_elevation_method, vegetation_notes)
  
write.csv(Schile_2017_core_data, "./data/Schile-Beers_etal_2017/derivative/Schile-Beers_etal_2017_core_data.csv")


## Site level data #############

# We're going to need to add a few new columns, and aggregate out core level
#   data up to the site level

# Change value of digits so we don't have too many for the next step
# options(digits=6)
# 
# # Change all attributes to numeric. we'll need this later for aggregate the data
# #   to the site level
# Osland_2018_site_data <- sapply(Osland_2018_land_climate, as.numeric)
# Osland_2018_site_data <- as.data.frame(Osland_2018_site_data) # Turn back to df
# 
# # This turns site and core ID attributes to NA, so we'll need to add those back.
# #   We'll pull them from the core level data
# # NOTE: this is a dangerous cbind. If the data was re-ordered, the data will be
# #   joined incorrectly
# Osland_2018_site_data <- cbind(Osland_2018_site_data, site_id = Osland_2016_core_data$site_id,
#                                core_id = Osland_2016_core_data$core_id)
# 
# # Rename and curate
# Osland_2018_site_data <- Osland_2018_site_data %>%
#   select(-ID, -tran, -plot, -estuary, -core_id, -dist) %>%
#   rename(core_longitude = "lon") %>%
#   rename(core_latitude = "lat") %>%
#   rename(mean_annual_precip = "MAP") %>%
#   rename(aridity_index = "AI") %>%
#   rename(potential_evapotrans = "PET") %>%
#   rename(min_temp = "Tmin") %>%
#   mutate(study_id = "Osland_2018")
# 
# # Find min and max lat/long for each site
# source("./scripts/1_data_formatting/curation_functions.R") 
# Osland_2018_site_data_boundaries <- create_multiple_geographic_coverages(Osland_2018_site_data)
# Osland_2018_site_data <- Osland_2018_site_data %>%
#   left_join(Osland_2018_site_data_boundaries) %>% # Add site bounds in
#   select(-core_latitude, -core_longitude)
# # remove NAs before aggregation
# Osland_2018_site_data <- na.omit(Osland_2018_site_data) 
# 
# # Now aggeregate data to the site level
# Osland_2018_site_data <- Osland_2018_site_data %>%
#   group_by(site_id) %>%
#   summarize_all(mean)
# 
# # Write data
# write.csv(Osland_2018_site_data, "./data/Osland_2018/derivative/Osland_2018_site_data.csv")
# 
# ## Species data ##################
# 
# # Select only the columns that correspond to the species presence in a 1-m2
# #   plot surrounding each core, plus the necessary other data
# Osland_2018_species_data <- Osland_2018_veg %>%
#   select(1:182) %>%
#   select("estuary", "plot", 16:182)
# 
# # Rename site and core IDs
# Osland_2018_species_data <- Osland_2018_species_data %>%
#   rename(site_id = "estuary") %>%
#   # current core IDs are not unique, so concatenate with site IDs
#   mutate(core_id = paste0(site_id, "_", plot)) %>%
#   select(-plot) %>%
#   mutate(study_id = "Osland_2018")
# 
# # The species presence data is currently  wide-form, with each column
# #   corresponding to the fraction area occupied by each plant species.
# # Let's write a function that can be applied to each row to change the data to
# #   long-form.
# find_max <- function(df, first_col, last_col) {
#   df <- df %>%
#     # Gather the species columns into one column and select just the most dominant
#     #   species present per core.
#     gather(colname, value, first_col:last_col) %>%
#     mutate(value = value) %>%
#     # Choose just the most dominant plant species
#     filter(value == max(value))
#   
#   # If all of the values for the species presence columns are 0, then change the
#   #   values to NA and only use the first row. The species presence value for
#   #   the given core will now just be NA.
#   if (df$value[[1]] == 0) {
#     df$value <- NA
#     df <- slice(df, 1)
#   }
#   df
# }
# 
# for (i in 1:nrow(Osland_2018_species_data)) {
#   
#   df <- slice(Osland_2018_species_data, i)
#   if (i == 1) {
#     out <- find_max(df, 5, 168)
#   } else {
#     
#     out <- rbind(out, find_max(df, 5, 168))
#   }
# }
# 
# colname <- out$colname
# 
# Osland_2018_species_data <- out %>%
#   rename(species_code = "colname") %>%
#   rename(fraction_coverage = "value") %>%
#   mutate(fraction_coverage = as.numeric(fraction_coverage)) %>%
#   select(site_id, core_id, species_code, fraction_coverage)
# 
# # Remove the '1c' string from the species_code entries
# Osland_2018_species_data$species_code <- 
#   sapply(Osland_2018_species_data$species_code,
#          function(x) {
#            gsub("1c", "", x) # Replace '1c' with ''                                           
#          })
# 
# # Write out data
# write.csv(Osland_2018_species_data, "./data/Osland_2018/derivative/Osland_2018_species_data.csv")
