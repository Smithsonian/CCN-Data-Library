## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

# Data citations:
# BELOW GROUND BIOMASS
# Deegan L., D. FitzGerald, S. Fagherazzi. 2012. 2009 LENS belowground biomass core results. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/bc041b3546ba4a3730fd391852741620. 
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-pie.216.2

# ABOVE GROUND BIOMASS
# Deegan L., D. FitzGerald, S. Fagherazzi. 2012. 2009 LENS aboveground biomass results. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/6830381e663fedc52bbdb3c501fdf3ee. 
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-pie.217.2

# Publication: 
# Deegan, Linda A., David Samuel Johnson, R. Scott Warren, Bruce J. Peterson, John W. Fleeger, 
# Sergio Fagherazzi, and Wilfred M. Wollheim. 2012. “Coastal Eutrophication as a Driver of Salt Marsh Loss.” 
# Nature 490 (7420): 388–92. https://doi.org/10.1038/nature11533.


## Prep workspace and scrape data #######################
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)

# download and save below ground biomass
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/216/2/e2c8e7c0b2338c593f00ab988d0e9774" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Location",     
                 "Site.Number",     
                 "Collection.Date",     
                 "Latitude",     
                 "Longitude",     
                 "Biomass.Core.Segment",     
                 "Live.Rhizomes",     
                 "Live.Roots",     
                 "Detritus"    ), check.names=TRUE)

write.csv(dt1, "./data/Deegan_2012/original/LTE-TIDE-LENS-2009-below-bio.csv")

# download above ground biomass data
infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/217/2/79e90861144b45ea7c229ca40cdaba40" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Location",     
                 "Site.Number",     
                 "Collection.Date",     
                 "Latitude",     
                 "Longitude",     
                 "Aboveground.Stem.Length",     
                 "Aboveground.mass"    ), check.names=TRUE)

write.csv(dt2, "./data/Deegan_2012/original/LTE-TIDE-LENS-2009-above-bio.csv")

## Prep biomass data #########
# Warning: There is no guidance yet on biomass data 
biomass_depthseries <- dt1


## Prep core-level data #########
core_data <- dt1 %>%
  rename(site_id = Location, 
         core_latitude = Latitude, 
         core_longitude = Longitude) %>%
  mutate(core_id = paste0(site_id,Site.Number)) %>%
  mutate(Collection.Date = gsub("Sep","09", Collection.Date)) %>%
  mutate(Collection.Date = gsub("-","/", Collection.Date)) %>%
  mutate(core_date = as.Date(Collection.Date, format="%d/%m/%Y")) %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id), core_latitude = first(core_latitude), core_longitude = first(core_longitude), 
            core_date = first(core_date)) %>%
  mutate(core_length_flag = "core depth limited by length of corer", 
         vegetation_class = "seagrass", 
         study_id = "Deegan_et_al_2012")

# merge in above ground biomass data 
core_data_biomass <- dt2 %>%
  rename(site_id = Location) %>%
  mutate(core_id = paste0(site_id,Site.Number)) %>%
  select(core_id, Aboveground.Stem.Length, Aboveground.mass) %>%
  rename(aboveground_stem_length = Aboveground.Stem.Length, 
         aboveground_mass = Aboveground.mass)

core_data <- merge(core_data,core_data_biomass)

write.csv(core_data, "./data/Deegan_2012/derivative/Deegan_et_al_2012_core_data.csv")

## Prep site-level data ##########
site_data <- core_data %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude)

# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)
# remove NAs before aggregation
site_data <- na.omit(site_data)

# Now aggeregate data to the site level
site_data <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  mutate(site_description = "Plum Island Sound Estuary, Massachusetts, USA", 
         vegetation_class = "seagrass")

# Write data
write.csv(site_data, "./data/Deegan_2012/derivative/Deegan_et_al_2012_site_data.csv")

