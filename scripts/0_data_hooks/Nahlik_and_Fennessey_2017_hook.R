# Author: Jaxine Wolfe

# Hook script for carbon stock dataset referenced in Nahlik and Fennessey (2017)

library(tidyverse)
library(lubridate)
# library(here)

# Download data ####

# scrape data from web
# condition <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")
# condition_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")

# site information
siteinfo <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo.csv")
siteinfo_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo-meta.txt")

# soil chemistry
soilchem <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem.csv")
soilchem_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem-meta.txt")

# vegetation
plants <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr.csv")
plants_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr-meta.txt")

# write original data
write_csv(soilchem, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_soilchem.csv")
write_csv(siteinfo, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_siteinfo-meta.csv")
write_csv(plants, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_plant_pres_cvr.csv")

# Data Curation #### 

# Site ####
site_marine <- siteinfo %>%
  filter(SANDT_RSTUDY == "Coastal Watersheds" &
           !is.na(AA_CENTER_LAT)) %>%
  select(SITE_ID, DATE_COL, AA_CENTER_LAT, AA_CENTER_LON, ANALYSIS_LAT, ANALYSIS_LON,
         COE_REGION, CLASS_FIELD_HGM, HUC10_NAME, MAJ_RIVER_BASIN, SANDT_RSTUDY, SANDT_COAST_REG) %>%
  rename(site_id = SITE_ID)

site_marine$DATE_COL <- dmy(site_marine$DATE_COL)  

# Cores/Depthseries? ####
soil_marine <- soilchem %>%
  filter(SITE_ID %in% unique(site_marine$SITE_ID)) %>%
  select(SITE_ID, DATE_COL, SAMPLE_ID, DEPTH, DEPTH_FLAG, TOT_CARBON, EC) %>%
  rename(site_id = SITE_ID, 
         core_date = DATE_COL,
         sample_id = SAMPLE_ID)

soil_marine$core_date <- as.Date(soil_marine$core_date, format = "%m/%d/%Y")

# Species ####
species_marine <- plants %>%
  filter(SITE_ID %in% unique(site_marine$SITE_ID)) %>%
  select(SITE_ID, DATE_COL, COVER, SPECIES, SPECIES_NAME_ID, SPECIES_COMMENT) %>%
  separate(col = SPECIES, into = c("genus", "species"), sep = " ") %>%
  mutate(species = replace_na(species, "spp.")) %>%
  rename(site_id = SITE_ID,
         species_code = SPECIES_NAME_ID,
         species_notes = SPECIES_COMMENT)

species_marine$DATE_COL <- dmy(species_marine$DATE_COL)

# Investigate Data ####
# map the data
library(maps)

# I suspect NA's are mostly inland
# site_na <- siteinfo %>%
#   filter(is.na(SANDT_RSTUDY) &
#            !is.na(AA_CENTER_LAT))

map <- ggplot() +
  geom_polygon(aes(long, lat, group = group), data = map_data("usa"), fill = "grey50") +
  coord_quickmap()

# Points should all be coastal
map + geom_point(data = site_marine, aes(x = AA_CENTER_LON, y = AA_CENTER_LAT, col = CLASS_FIELD_HGM)) +
  theme_classic()

map + geom_point(data = site_marine, aes(x = ANALYSIS_LON, y = ANALYSIS_LAT, col = CLASS_FIELD_HGM)) +
  theme_classic()

