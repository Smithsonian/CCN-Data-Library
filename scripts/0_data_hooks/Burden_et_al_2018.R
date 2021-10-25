## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for Burden et al 2018
# https://doi.org/10.5285/0b1faab4-3539-457f-9169-b0b1fbd59bc2

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
# library(sf)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_data_2010_2017 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2010_2017.csv")
raw_data_2011 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2011.csv")
# locations extracted from the .rtf file
site_info <- read_xls("data/primary_studies/Burden_et_al_2018/intermediate/Burden_2018_site_locations_w_lat_lon.xls", na = "NA")
raw_methods <- read_csv("data/primary_studies/Burden_et_al_2018/intermediate/Burden_2018_materials_and_methods.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Burden_et_al_2018"

## ... Methods ####

sample_volume <- pi*(2^2)*30

# curate materials and methods
methods <- raw_methods %>% 
  select_if(function(x) {!all(is.na(x))})

## ... Core Depthseries ####

ds_2010_2017 <- raw_data_2010_2017 %>%
  mutate(year = ifelse(SiteName == "Tollesbury Field", 2010, 2017),
         month = ifelse(SiteName == "Tollesbury Field", 7, 4),
         SiteCode = as.character(SiteCode))

ds_2011 <- raw_data_2011 %>%
  mutate(year = 2011, month = 10,
         SiteCode = as.character(SiteCode))
  
# curate depthseries-level data
depthseries_data <- bind_rows(ds_2010_2017, ds_2011) %>% 
  rename(site_id = SiteName,
         core_id = SiteCode,
         dry_bulk_density = BulkDensity) %>% 
  mutate(study_id = id,
         # make the core ID more unique
         core_id = str_c(substr(site_id, 1, 5), core_id, sep = "_"),
         method_id = "single set of methods",
         depth_min = 0,
         depth_max = 30,
         fraction_organic_matter = OrganicMatter/100,
         fraction_carbon = Carbon/100,
         core_id = gsub("[.]", "_", core_id)) %>% 
  select(-c(MoistureContent, pH, Conductivity, `NO3-N`,
            `NH4-N`, OrganicMatter, BelowGround, Carbon, Nitrogen))

depthseries <- reorderColumns("depthseries", depthseries_data) %>%
  select(-year, -month)

ggplot(depthseries) +
  geom_point(aes(dry_bulk_density, fraction_carbon))

## ... Core-Level ####

# curate core-level data
core_data <- depthseries_data %>% 
  distinct(study_id, site_id, core_id, year, month) %>% 
  left_join(site_info) %>% # 	Barrow Hill Field doesn't exist in the depthseries
  rename(latitude = core_latitude, longitude = core_longitude) %>%
  mutate(vegetation_class = "emergent",
         core_length_flag = "core depth limited by length of corer",
         position_notes = "coordinates converted from spatial reference system OSGB 1936 / British National Grid",
         elevation_notes = "All salt marsh samples were taken from permanently vegetated marsh above 1.5 m ordnance datum (OD)",
         core_notes = ifelse(!is.na(year_of_breach), paste0("year of tidal breach: ", year_of_breach), NA)) 

# UTM_to_DD <- function(df, zone){
#   
#   # define UTM and latlong projections
#   # not sure if custom projections should be defined or use the ESPG codes...
#   utm_prj <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   dd_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   
#   # isolate UTM coordinates
#   # utm_coords <- data.frame(core_id = "Chelmsford", easting = 325205.77, northing = 5734662.8) # test
#   utm_coords <- df %>% select(core_id, easting, northing)
#   
#   # create spatial object and assign coords utm coordinate system
#   utm_coords <- st_as_sf(utm_coords, coords = c("easting", "northing"), crs = 27700) 
#   # st_crs(utm_coords) # check projection
#   
#   dd_coords <- st_transform(utm_coords, crs = 4326) %>% 
#     # extract lat lon from geometry
#     extract(geometry, into = c('longitude', 'latitude'), '\\((.*),(.*)\\)', 
#             convert = T) %>%
#     select(core_id, longitude, latitude)
#   
#   # join back to the dataframe
#   df_decimal <- left_join(df, dd_coords)
#   
#   return(df_decimal)
# }
# 
# # convert UTM to decimal degrees
# cores_decimal <- UTM_to_DD(core_data, "31")


cores <- core_data %>%
  select(-c(Easting, Northing, impact_class, year_of_breach, Notes, OBJECTID)) %>% 
  reorderColumns('cores', .)
# position_method? position_notes?

## ... Impacts ####

# Classes:
# natural salt marsh => natural
# restored and accidentally breached => restored
# fields on former salt marsh => farmed

impacts <- core_data %>% 
  select(contains("_id"), impact_class)

## 2. QAQC ####

## Mapping
leaflet(core_data) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names) # no position method

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

if(!file.exists("data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_study_citations.csv")){
  # Create bibtex file
  data_doi <- '10.5285/0b1faab4-3539-457f-9169-b0b1fbd59bc2'
  pub_doi <- "10.1098/rsbl.2018.0773"
  
  data_bib <- as.data.frame(GetBibEntryWithDOI(data_doi))
  pub_bib <- as.data.frame(GetBibEntryWithDOI(pub_doi))
  
  study_citations <- bind_rows(data_bib, pub_bib) %>%
    mutate(study_id = id,
           bibliography_id = c("Burden_et_al_2018_data", "Burden_et_al_2019_article"),
           publication_type = c("primary dataset", "associated source")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018.bib")
  write_csv(study_citations, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_study_citations.csv")
}

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_cores.csv") 
write_csv(depthseries, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_depthseries.csv")
write_csv(methods, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_methods.csv")
write_csv(impacts, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_impacts.csv")
# write_csv(species, "data/primary_studies/Burden_et_al_2018/derivative/Burden_et_al_2018_species.csv")
