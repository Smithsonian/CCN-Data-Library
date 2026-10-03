## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data ingestion

# load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(fs)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_mmc1 <- read_csv("data/primary_studies/Sjogersten_et_al_2021/intermediate/1-s2.0-S0016706121002536-mmc1.csv")
raw_mmc2 <- read_xlsx("data/primary_studies/Sjogersten_et_al_2021/original/1-s2.0-S0016706121002536-mmc2.xlsx")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

# # Read in core locations from KMZ file
# # https://mitchellgritts.com/posts/load-kml-and-kmz-files-into-r/
# 
# kmz_files <- list.files("data/primary_studies/Sjogersten_et_al_2021/original", 
#                         pattern = "mmc5", full.names = T)
# 
# i <- 3
# 
# for (kmz in kmz_files) {
#   input_file <- kmz
#   read_sf(input_file)
#   
#   # workaround
#   target_file <- 'data/primary_studies/Sjogersten_et_al_2021/original/.temp.kml.zip'
#   fs::file_copy(input_file, target_file)
#   unzip(target_file, )
#   
#   # read as kml now
#   coords_kml <- read_sf('doc.kml')
#   
#   # cleanup the temp files
#   fs::file_delete(target_file)
#   fs::file_delete('doc.kml')
#   
#   # correct coords for GC cores
#   coords <- coords_kml %>%
#     # extract lat lon from geometry
#     extract(geometry, into = c('longitude', 'latitude', 'z'), '\\((.*),(.*),(.*)\\)',
#             convert = T) %>%
#     select(Name, longitude, latitude) %>%
#     mutate(core_id = paste0("GC", Name)) %>%
#     select(core_id, longitude, latitude)
#   
#   write_csv(coords, paste0("data/primary_studies/Sjogersten_et_al_2021/intermediate/core_locations_mmc", i, ".csv"))
#   
#   i <- i + 1
# }

## 1. Curation ####

id <- "Sjogersten_et_al_2021"

ds_data <- raw_mmc1 %>% 
  mutate(study_id = id, 
         habitat = tolower(Vegetation)) %>% 
  # depth ranges are messed up
  # some coerced to date, some backwards, others...
  separate(`Depth (cm)`, into = c("depth_min", "depth_max"), sep = "-")

## ... Depthseries
