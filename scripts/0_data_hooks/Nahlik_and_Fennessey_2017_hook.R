# Author: Jaxine Wolfe

# Hook script for carbon stock dataset referenced in Nahlik and Fennessey (2017)

library(tidyverse)
library(here)

# Download data ####

# scrape data from web
# condition <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")
# condition_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")

soilchem <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem.csv")
soilchem_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem-meta.txt")

siteinfo <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo.csv")
siteinfo_meta <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo-meta.txt")

write_csv(soilchem, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_soilchem.csv")
write_csv(siteinfo, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_siteinfo-meta.csv")

# Data Curation #### 

site_marine <- siteinfo %>%
  filter(SANDT_RSTUDY == "Coastal Watersheds" &
           !is.na(AA_CENTER_LAT))
  
# Trim and recode column headers

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

