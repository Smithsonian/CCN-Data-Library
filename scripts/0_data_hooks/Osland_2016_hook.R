## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from Osland et al 2016 data release

## Assumptions made about data ###############

# that lat and long is in WGS84


## Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(rvest)
library(stringr)
library(RCurl)
library(tidyverse)
library(lubridate)

## Download data ########################


# The Gonneea et al (2018) data release features a diverse suite of file types:
#   a .jpg, a .xlsx, a .csv, and a .xml
# So we'll need to have a custom hook for each file

URL <- "https://www.sciencebase.gov/catalog/item/57b24094e4b00148d3982cce"
BASE_URL <- "https://www.sciencebase.gov"

page <- read_html(url)

url_list <- page %>%
  html_nodes('.sb-download-link') %>% 
  html_attr("data-url")

# Extract .xml metadata
download.file(paste0(BASE_URL, url_list[[1]]), paste0(getwd(), "./data/Osland_2016/original/U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Landscape_and_Climate_Data_2013_2014_.xml"),
              mode = "wb")

# Extract macroclimate, landscape and climate data
download.file(paste0(BASE_URL, url_list[[2]]), paste0(getwd(), "./data/Osland_2016/original/Dataset_03_macroclimate_landscape_and_climate_data_2_22_2016.xlsx"),
              mode = "wb")

