# Author: Jaxine Wolfe

# Hook script for blah blah

library(tidyverse)
library(here)

# Download data ####

# scrape data from web
data <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")

metadata <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")

write_csv(data, "./data/primary_studies/Nahlik_Fennessey_2017/original/NWCA_2011_cond_stress.csv")

# Data Curation #### 

data_marine <- data %>%
  filter(SANDT_RSTUDY == "Coastal Watersheds" &
           !is.na(XCOORD))
  
# Trim and recode column headers

# Investigate Data ####
# map the data
library(maps)

# I suspect NA's are mostly inland
data_na <- data %>% 
  filter(is.na(SANDT_RSTUDY) &
           !is.na(XCOORD))

map <- ggplot() +
  geom_polygon(aes(long, lat, group = group), data = map_data("usa"), fill = "grey50") +
  coord_quickmap()

# problem...dataset xy coords are in Alber's
map + geom_point(data = data_na, aes(x = XCOORD, y = YCOORD)) +
  theme_classic()

