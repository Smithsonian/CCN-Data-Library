## CCRCN Data Library
# contact: klingesd@si.edu

# This is a template web scraper for USGS ScienceBase.gov data files

## INSTRUCTIONS ####################

# 1. Designate the target webpage to scrape for data
#   Paste the url of the target webpage here, wrapped in quotation marks

# Note: because Osland_2018 has multiple data release DOIs, we'll just run this
#   script separately for each one. Easier to read than looping through each url

URL_1 <- "https://www.sciencebase.gov/catalog/item/57b24094e4b00148d3982cce"
URL_2 <- "https://www.sciencebase.gov/catalog/item/57b240fce4b00148d3982cd0"
URL_3 <- "https://www.sciencebase.gov/catalog/item/57aa11efe4b05e859be06932"

                 
# 2. Name the files
#   Add the names for each file into this list, wrapped in quotation marks, IN 
#   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
#   extension (.csv, .xlsx, etc.) in the name of the file as well.
  
FILE_NAMES_1 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Landscape_and_Climate_Data_2013_2014_.xml",
                   "Dataset_03_macroclimate_landscape_and_climate_data_2_22_2016.xlsx"
)

FILE_NAMES_2 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Soil_Data_2013_2014_.xml",
                     "Dataset_02_macroclimate_soil_data_2_22_2016.xlsx"
)

FILE_NAMES_3 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Vegetation_Data_Section_1_2013_2014_.xml",
                     "Dataset_01_macroclimate_vegetation_data_all_2_24_2016.xlsx"
)

# 3. Designate file path of where these data files will go in the CCRCN library
#   Paste the file path here, wrapped in quotation marks. The getwd() function 
#   will automatically detect the working directory of the R project (in the case 
#   of the CCRCN Data library, the location of where this repository is stored on 
#   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
#   with whatever you include within the quotation marks.
  
FILE_PATH <- paste0(getwd(), "./data/Osland_2018/original/" )
  
## Assumptions made about data ###############

# that lat and long is in WGS84

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
BASE_URL <- "https://www.sciencebase.gov"

# Because Osland 2016 has multiple urls, we'll need to run this loop multiple times
 
page <- read_html(URL_3)
  
# Extract the url paths for each data file embedded on the webpage, and save
#   those paths to a list
url_list <- page %>%
  html_nodes('.sb-download-link') %>% 
  html_attr("data-url")
  
# For each data file path on the webpage....
for (i in 1:length(url_list)) {
  
  # ...extract and download file
  download.file(paste0(BASE_URL, url_list[[i]]), paste0(FILE_PATH, FILE_NAMES_3[[i]]),
                  mode = "wb")
}

## Curate data to CCRCN Structure #################

# Read data in
Osland_2018_soil <- read_excel(paste0(FILE_PATH, "Dataset_02_macroclimate_soil_data_2_22_2016.xlsx"))
Osland_2018_land_climate <- read_excel(paste0(FILE_PATH, "Dataset_03_macroclimate_landscape_and_climate_data_2_22_2016.xlsx"))
Osland_2018_veg <- read_excel(paste0(FILE_PATH, "Dataset_01_macroclimate_vegetation_data_all_2_24_2016.xlsx"))

# Remove instructions at the top of sheets
Osland_2018_depth_series_data <- Osland_2018_soil %>%
  slice(-1:-9)
# the first row will be the header
colnames(Osland_2018_depth_series_data) <- Osland_2018_depth_series_data[1, ] 
Osland_2018_depth_series_data <- Osland_2018_depth_series_data[-1, ]

Osland_2018_veg <- Osland_2018_veg %>%
  slice(-1:-9)
# the first row will be the header
colnames(Osland_2018_veg) <- Osland_2018_veg[1, ] 
Osland_2018_veg <- Osland_2018_veg[-1, ]

Osland_2018_land_climate <- Osland_2018_land_climate %>%
  slice(-1:-9)
# the first row will be the header
colnames(Osland_2018_land_climate) <- Osland_2018_land_climate[1, ] 
Osland_2018_land_climate <- Osland_2018_land_climate[-1, ]

## Depth series data ###################

# From Osland_2018_soil

# Call functions from curation_functions script
source("./scripts/1_data_formatting/curation_functions.R") 

Osland_2018_depth_series_data <- Osland_2018_depth_series_data %>%
  rename(site_id = "estuary") %>%
  rename(core_id = "plot") %>%
  rename(dry_bulk_density = "bd") %>%
  mutate(fraction_organic_matter = convert_percent_to_fraction(som)) %>%
  select(-som) %>%
  # CCRCN does not have standards for soil moisture content yet, but this approach
  #   most closely aligns with other attributes
  mutate(fraction_moisture_content = convert_percent_to_fraction(moist)) %>%
  select(-moist)

# The legend dictates that only one soil depth interval was sampled, 01-5 cm.
# So we'll add a single set of min and max depths for each core

Osland_2018_depth_series_data <- Osland_2018_depth_series_data %>%
  mutate(depth_min = 0) %>%
  mutate(depth_max = 15)

# Read out depth series data
write.csv(Osland_2018_depth_series_data, "./data/Osland_2018/derivative/Osland_2018_depth_series_data.csv")


## Core data ####################

# from Osland_2018_veg

# There's an unwieldy amount of columns, so we'll select them down
Osland_2018_core_data <- Osland_2018_veg %>%
  select(1:13)

Osland_2018_core_data <- Osland_2018_core_data %>%
rename(site_id = "estuary") %>%
  # current core IDs are not unique, so concatenate with site IDs
  mutate(core_id = paste0(site_id, "_", plot)) %>%
  # concatenate date attributes, then remove
  mutate(core_date = as_date(paste0(year, "-", month, "-", day))) %>%
  select(-year, -month, -day) %>%
  # We don't need the transect number. Transects can be derived from coordinates
  select(-tran) %>%
  rename(core_notes = "criteria") %>%
  rename(core_time = "time") %>%
  rename(core_elevation = "elev") %>%
  mutate(core_elevation_datum = "NAVD88")


# Transform the projection
Osland_2018_core_data_coords <- convert_UTM_to_latlong(Osland_2018_core_data$easting, 
                                                       Osland_2018_core_data$northing, Osland_2018_core_data$zone, Osland_2018_core_data$core_id)
Osland_2018_core_data <-  Osland_2018_core_data %>%
  left_join(Osland_2018_core_data_coords)

# Now we can move easting, northing, and zone attributes
Osland_2018_core_data <- Osland_2018_core_data %>%
  select(-easting, -northing, -zone, -ID)


## Species data ##################

# Gather the species columns into one column and select just the most dominant
#   species present per core.
find_max <- function(df, first_col, last_col) {
  df <- df %>%
    gather("colname", "value", first_col:last_col) %>%
    mutate(value = as.numeric(value)) %>%
    filter(value == max(value))

  if (df$value[[1]] == 0) {
    df$value <- NA
    df <- slice(df, 1)
  }
  df
}

# Create veg data

Osland_2018_species_data <- Osland_2018_veg %>%
  select(1:182)
for (i in 1:nrow(Osland_2018_species_data)) {
  
  df <- slice(Osland_2018_species_data, i)
  if (i == 1) {
    out <- find_max(df, 19, 181)
  } else {
    
    out <- rbind(out, find_max(df, 19, 181))
  }
}

test <- Osland_2018_veg %>%
  slice(2)

test_out <- find_max(test, 19, 181)
test_out$value
test_out$colname


veg <- apply(Osland_2018_veg, FUN = find_max(x, 19, 181))



# create a data frame of the column headers and values for each column
# find greatest value
# select that, bind it to og data in  a new column
