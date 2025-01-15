# Coastal Carbon Research Coordination Network
# Database Guidance V2
# Synthesis Post-Processing Script

# The code iterates through the CCN database and assigns country and administrative division to each core
# Contact: Jaxine Wolfe, wolfejax@si.edu

## Prepare workspace ####

# load libraries
library(tidyverse)
library(sf)

# ## Read in data with sf
# countries <- st_read(dsn = "data/input_shapefiles/world_countries/", layer = "country")
# states <- st_read(dsn = "data/input_shapefiles/us_states/states_political_boundaries/", layer = "state_pol")
# eez <- st_read(dsn = "data/input_shapefiles/World_EEZ_v11_20191118/", layer = "eez_v11")
# divisions <- st_read(dsn = "data/input_shapefiles/admin_divisions/", layer = "admin")
# 
# # some of these geometries need to be validated
# eez <- st_make_valid(eez)
# countries <- st_make_valid(countries)
# divisions <- st_make_valid(divisions)

# Save all shapefiles as Rdata file (for quicker upload)
# save(countries, eez, divisions, states, file = "data/input_shapefiles/assign_geography.rdata")

## Prepare point and polygon data ####

# isolate core data and drop NAs
# rand_select <- sample(1:8600, size = 100) # for testing, random selection of coordinates from the database
pnts <- ccrcn_synthesis$cores %>% 
  drop_na(longitude, latitude) %>%
  # this could be problematic if coords are updated (it wouldn't detect)
  # filter(!(core_id %in% unique(already_assigned$core_id))) %>%
  select(study_id, site_id, core_id, latitude, longitude) %>%
  mutate(across(c(latitude, longitude), as.numeric))

  
sp_points <- st_as_sf(pnts, coords = c('longitude',"latitude")) # make points spatial
st_crs(sp_points) <- 4326 # Give the points a coordinate reference system (CRS)

# Load  shapefiles from rdata
load("data/input_shapefiles/assign_geography.rdata")

# subset divisions 
admin_divisions <- divisions %>%
  filter(ADMINTYPE %in% c("State", "Province", "Territory")) %>%
  # Select only the information we want out of the file
  rename(admin_division = NAME) %>% 
  select(admin_division)

# These EEZ 'soveriegns' are not in the countries map

# Do nothing
# Western Sahara, Not claimed by Morocco or Mauritania (I think)

# make sure vocab is constant

# Change in EEZ
# Comores in EEZ, Spelled Comoros in countries, Comoros is correct 
# Republic of Mauritius in EEZ, just Mauritius in countries
# East Timor, Timor-Leste in countries
# Republic of the Congo, Just Congo in Countries
# Cape Verde, Cabo verde in countries
# Ivory Coast, 	Côte d'Ivoire in countries
# Palestine, Palestinian Territory in countries
# Taiwan in EEZ, China in countries, change to China because of U.S. govt. policy
# Federal Republic of Somalia in EEZ, just Somalia in countries
# Russia in EEZ, Russian Federation in countries 

countries2 <- countries %>%
  select(COUNTRYAFF) %>%
  mutate(source = "countries",
         country = recode(COUNTRYAFF, "Congo DRC" = "Democratic Republic of the Congo",
                          "Brunei Darussalam" = "Brunei")) %>% 
  select(-COUNTRYAFF)

eez2 <- eez %>% 
  select(SOVEREIGN1) %>% 
  mutate(country = recode(SOVEREIGN1, "Comores" = "Comoros",
                          "Republic of Mauritius" = "Mauritius",
                          "East Timor" = "Timor-Leste",
                          "Republic of the Congo" = "Congo",
                          "Cape Verde" = "Cabo Verde",
                          "Ivory Coast" = "Côte d'Ivoire",
                          "Palestine" = "Palestinian Territory",
                          "Taiwan" = "China",
                          "Federal Republic of Somalia"="Somalia",
                          "Russia" = "Russian Federation")) %>% 
  mutate(source = "EEZ") %>% 
  select(-SOVEREIGN1)

# Which countries are not in others
# left_out_eez <- eez2 %>% 
#   filter(! country %in% countries2$country)
# 
# print(unique(left_out_eez$country))
# 
# left_out_countries <-  countries2 %>% 
#   filter(! country %in% eez2$country)
# 
# print(unique(left_out_countries$country))
# 
## Perform Spatial Intersection ####

# Merge files together
eez_and_countries <- eez2 %>% 
  bind_rows(countries2) %>% 
  select(-source)

# Join country with both countries and EEZ's
sp_points_country <- st_join(sp_points, eez_and_countries)

# Filter out nodata values
sp_points_country <- filter(sp_points_country, complete.cases(country))

# Second join to nearest administrative join
# This is necessary because the admin divisions may not be as expansive as the countries/eez shapefile
sp_points_country_and_admin <- st_join(sp_points_country, admin_divisions,
                                     join = st_nearest_feature)


## Table Merge ####

# clean up assigned geography
assigned_geography <- sp_points_country_and_admin %>% 
  st_drop_geometry() 

# merge with core table
core_geography <- left_join(ccrcn_synthesis$cores, 
                            unique(assigned_geography),
                            by = c("study_id", "site_id", "core_id")) %>% 
  # Jaxine: spot correction to admin division
  mutate(admin_division = recode(admin_division, "Ngöbe-Buglé" = "Bocas del Toro"))

# isolate all cores with no assigned country
# na_country <- core_geography %>% 
#   filter(is.na(country)) %>%
#   select(study_id, site_id, core_id, latitude, longitude)
# 
# # write cores to QA folder
# if(!is_empty(na_country)){
#   write_csv(na_country, "data/QA/no_country_for_soil_cores.csv")
# }

# Update core table in CCN synthesis ####
ccrcn_synthesis$cores <- core_geography # will overwrite initial synthesis core table with updated geography

## Clear variables taking up memory ####

rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
