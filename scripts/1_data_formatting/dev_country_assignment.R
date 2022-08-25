library(tidyverse)
library(sf)
library(rnaturalearth)
library(rgeos)

cores <- read.csv("./data/CCRCN_synthesis/CCRCN_cores.csv")

dfr <- cores %>% 
  select(core_longitude, core_latitude, core_id) %>%
  filter(!is.na(core_longitude) & !is.na(core_latitude)) %>%
  filter(!is.na(core_id))

sfRegion <- st_as_sf(dfr, coords=c('core_longitude', 'core_latitude'))
sfCountry <- ne_countries(returnclass='sf')
st_crs(sfRegion) <- 4326
country_summary <- st_join(sfCountry, sfRegion) %>%
  select(core_id, name) %>%
  filter(!is.na(core_id)) %>%
  group_by(name) %>%
  summarize(n = n())

core_country_merge <- st_join(sfCountry, sfRegion) %>%
  select(core_id, name) %>%
  filter(!is.na(core_id))

write_csv(country_summary, "./docs/country_summary.csv")
