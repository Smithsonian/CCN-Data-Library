## CCRCN Data Library
# contact: klingesd@si.edu

# This is a template web scraper for USGS ScienceBase.gov data files

## INSTRUCTIONS ####################

# 1. Designate the target webpage to scrape for data
#   Paste the url of the target webpage here, wrapped in quotation marks

URL <- ""

# 2. Name the files
#   Add the names for each file into this list, wrapped in quotation marks, IN 
#   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
#   extension (.csv, .xlsx, etc.) in the name of the file as well.
  
FILE_NAMES <- list(# Name_1, Name_2, Name_3, etc.
)

# 3. Designate file path of where these data files will go in the CCRCN library
#   Paste the file path here, wrapped in quotation marks. The getwd() function 
#   will automatically detect the working directory of the R project (in the case 
#   of the CCRCN Data library, the location of where this repository is stored on 
#   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
#   with whatever you include within the quotation marks.
  
FILE_PATH <- paste0(getwd(), "", "/" )
  
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

# The stem of the url should always be the same
BASE_URL <- "https://www.sciencebase.gov"

page <- read_html(URL)

# Extract the url paths for each data file embedded on the webpage, and save
#   those paths to a list
url_list <- page %>%
  html_nodes('.sb-download-link') %>% 
  html_attr("data-url")

# For each data file path on the webpage....
for (i in 1:length(url_list)) {
  
  # ...extract and download file
  download.file(paste0(BASE_URL, url_list[[i]]), paste0(FILE_PATH, FILE_NAMES[[i]]),
                mode = "wb")
}


