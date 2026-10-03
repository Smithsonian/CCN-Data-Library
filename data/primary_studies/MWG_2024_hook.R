## CCN Data Library ####

## Hook script for Methane working group data (Arias-Ortiz et al 2024)
## contact: Jaxine Wolfe, wolfejax@si.edu

# load necessary libraries
library(tidyverse)
library(lubridate)
library(leaflet)

# load in helper functions
# source("scripts/1_data_formatting/curation_functions.R") # For curation
# source("scripts/1_data_formatting/qa_functions.R") # For QAQC


#assign study id
id <- "Arias-Ortiz_et_al_2024"

## use the following to read in the tables
# site <- read_csv("https://ndownloader.figshare.com/files/48290971", guess_max = 20000)
# chamber <- read_csv("https://ndownloader.figshare.com/files/48290959", guess_max = 20000)
# timeseries <- read_csv("https://ndownloader.figshare.com/files/48290962", guess_max = 20000)
# porewater <- read_csv("https://ndownloader.figshare.com/files/48290968", guess_max = 20000)
# species <- read_csv("https://ndownloader.figshare.com/files/48290974", guess_max = 20000)

