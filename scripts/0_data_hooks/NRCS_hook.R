## CCN Data Library ########

## Data hook script for Morrissette et al 2023
## contact: Jaxine Wolfe; wolfejax@si.edu 

# load necessary libraries
library(tidyverse)
# library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
# library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# load in data 
nrcs_sites <- readxl::read_xlsx("data/primary_studies/NRCS_NJ/original/NJTWMN_SoilSampling.xlsx", sheet = 1, skip = 1)
nrcs_ds <- readxl::read_xlsx("data/primary_studies/NRCS_NJ/original/NJTWMN_SoilSampling.xlsx", sheet = 2, skip = 1)

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "NRCS_NJ"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases


## ... Depthseries ####

depthseries <- nrcs_ds %>% 
  fill(Pedon) %>% drop_na(Horizon) %>% 
  rename(depth_min = `Top Depth, cm`, 
         depth_max = `Bot. Depth, cm`,
         dry_bulk_density = `OD Db, g/cc`) %>% 
  separate(Pedon, into = c("core_id", "site_id"), sep = " - ") %>% 
  mutate(study_id = id, 
         core_id = recode(core_id, "S2019011001" = "S2019NJ011001"),
         fraction_carbon = `Total C`/100) %>% 
  reorderColumns("depthseries",.)

## ... Cores ####

cores <- nrcs_sites %>% 
  drop_na(Lat) %>% 
  rename(latitude = Lat, longitude = Long,
         core_id = `Ped. ID`) %>% 
  mutate(study_id = id,
         latitude = as.numeric(latitude), longitude = as.numeric(longitude),
         year = year(as.Date(`Description / Sampling Date`)),
         month = month(as.Date(`Description / Sampling Date`)),
         day = day(as.Date(`Description / Sampling Date`)),
         salinity_class = recode(tolower(`Fresh / Salt`), "salt" = "estuarine")) %>% 
  full_join(depthseries %>% distinct(site_id, core_id)) %>% 
  reorderColumns("cores", .)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, label = ~site_id)


# Bibliography include KSSL Analyses Links?
# How to cite?

## ... Methods ####
# these cores are from different sources across different years, and probably used different methods?
# methods <- data.frame(study_id = id, 
#                       coring_method = "vibracore",
#                       roots_flag = "roots and rhizomes included",
#                       sediment_sieved_flag = "sediment not sieved",
#                       compaction_flag = "not specified",
#                       dry_bulk_density_temperature = 60,
#                       dry_bulk_density_flag = "to constant mass",
#                       carbon_measured_or_modeled = "measured", 
#                       fraction_carbon_method = "EA", 
#                       fraction_carbon_type = "organic carbon")

# Test workflow to webscrape information for each pedon ID from the lab data mart
# https://stackoverflow.com/questions/55092329/extract-table-from-webpage-using-r
# library(rvest)
# 
# url <-  "https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=72316&r=1&"
# 
# df <- url %>% 
#   read_html() %>% 
#   html_nodes("table") %>% 
#   html_table(fill = T)









