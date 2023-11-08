# Coastal Carbon Research Coordination Network
# Database Guidance V2
# Synthesis Post-Processing Script

# The code iterates through the CCN database and assigns country and administrative division to each core
# Contact: Jaxine Wolfe, wolfejax@si.edu

## Prepare workspace ####

# load libraries
library(tidyverse)
library(sf)

# Read in data

# Load  shapefiles from rdata
load("data/input_shapefiles/assign_geography.rdata")

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

# subset divisions 
admin_divisions <- divisions %>% filter(ADMINTYPE %in% c("State", "Province", "Territory")) %>%
  # add unique identifier for admin division and country 
  mutate(ADMIN_COUNTRY = paste(NAME, COUNTRY, sep = "_"))


# isolate core data and drop NAs
# rand_select <- sample(1:8600, size = 100) # for testing, random selection of coordinates from the database
pnts <- ccrcn_synthesis$cores %>% 
  # mutate(name = paste(study_id, core_id, sep = "_")) %>% 
  select(study_id, core_id, latitude, longitude) %>%
  drop_na(longitude, latitude) %>%
  mutate(across(c(latitude, longitude), as.numeric)) %>% 
  slice(rand_select) # remove this for full assignment

sp_points <- st_as_sf(pnts, coords = c('longitude',"latitude")) # make points spatial
st_crs(sp_points) <- 4326 # Give the points a coordinate reference system (CRS)


## Perform Spatial Intersection ####

# apply intersection for various polygon layers

# 2 mins
sp_points$state <- apply(st_intersects(states, sp_points, sparse = FALSE), 2, 
                         function(col) {states[which(col), ]$STATE}) 

# 50 mins
sp_points$country <- apply(st_intersects(countries, sp_points, sparse = FALSE), 2, 
                         function(col) {countries[which(col), ]$COUNTRY})

# 90 mins
sp_points$eez <- apply(st_intersects(eez, sp_points, sparse = FALSE), 2, 
                           function(col) {eez[which(col), ]$TERRITORY1})

# 25 mins
sp_points$admin_divisions <- apply(st_intersects(admin_divisions, sp_points, sparse = FALSE), 2, 
                       function(col) {admin_divisions[which(col), ]$ADMIN_COUNTRY})

# this takes a long time (2.8hrs) and needs to be made more efficient - perhaps with parallel computing
library(parallel)

# test <- 


## Table Merge ####

# clean up assigned geography
assigned_geography <- sp_points %>% 
  st_drop_geometry() %>% 
  unnest(c(state, country, eez, admin_divisions), keep_empty = TRUE) %>% 
  separate_wider_delim(admin_divisions, delim = "_", names = c("admin_division", "admin_country")) %>% 
  # selectively combine the assigned geography results
  mutate(admin_division = ifelse(!is.na(state) & is.na(admin_division), state, admin_division),
         country = ifelse(is.na(country), eez, country))

# merge with core table
core_geography <- left_join(ccrcn_synthesis$cores, assigned_geography %>% 
                              select(study_id, core_id, admin_division, country))


# Investigate NA cases in geography assignment
length(which(is.na(core_geography$country))) # country has 23 NA b/c lat/lon is NA
length(which(is.na(core_geography$admin_division))) # number of NA admin division assignments: 1178

# isolate all cores with no assigned country
na_country <- core_geography %>% 
  select(study_id, site_id, core_id, longitude, latitude, country, admin_division) %>% 
  filter(is.na(country)) %>%
  distinct() %>% arrange(country)

# write cores to QA folder
if(!is_empty(na_country)){
  write_csv(na_country, "data/QA/no_country_for_soil_cores.csv")
}

# all the countries without assigned administrative divisions
na_admin_division <- core_geography %>% 
  select(study_id, site_id, core_id, longitude, latitude, country, admin_division) %>% 
  filter(is.na(admin_division)) %>%
  distinct() %>% arrange(country)

# Update core table in CCN synthesis ####

ccrcn_synthesis$cores <- core_geography # will overwrite initial synthesis core table with updated geography

# write_csv(core_geography, "data/CCRCN_synthesis/derivative/CCRCN_cores.csv") # core table is now passed onto the next post-processing function

## Clear variables taking up memory ####

rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
