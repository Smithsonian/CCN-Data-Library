## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

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

FILE_PATH <- paste0(getwd(), "/data/Schile_2017/original/" )

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
