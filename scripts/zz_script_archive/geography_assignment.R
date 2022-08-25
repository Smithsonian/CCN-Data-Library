# Coastal Carbon Research Coordination Network
# This script ingests and builds US state shapefiles for display
# Contact: David Klinges, klingesd@si.edu
#          Michael Lonneman, lonnemanm@si.edu

## 1. Prep workspace ##############
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(rnaturalearth)

## ... 1A. Import state shapefile and reproject ########
states <- readOGR(dsn = "./data/input_shapefiles/us_states/states_political_boundaries",
                  layer = "state_pol")
states <- spTransform(states, CRS("+proj=longlat +datum=WGS84 +no_defs"))


## ... 1B. Import HUC8 watershed shapefiles ############
watersheds <- readOGR("./data/input_shapefiles/coastal_HUC8s",
                      layer = "HUC8_AllTidalNwiAndNonTidalPlusFarmedBelowMHHWS_ObviousOutliersRemoved")
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs"))

## ... 1C. Import core data and exclude data already assigned states and watersheds ##########

# Import core data
cores_pre_geography <- read.csv("./data/CCRCN_synthesis/CCRCN_cores.csv")

# A few cores do not have coordinates. Remove them for now
cores_pre_geography <- cores_pre_geography %>% 
  filter(!is.na(longitude))

# Import site data. We'll be joining to this once we add geography attributes
sites <- read_csv("data/CCRCN_synthesis/CCRCN_sites.csv")

## 2. Assign state attributes ###########

## ... 2A. Prep data to assign state attributes #############

# Convert core data to spatialPointsDataFrame, with coordinates and core IDs
core_coords <- SpatialPointsDataFrame(
  coords = data.frame(core_longitude = cores_pre_geography$longitude, 
                      core_latitude = cores_pre_geography$latitude),
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),
  data = data.frame(cores_pre_geography$core_id)
)

## ... 2B. Assign states to cores without state assignments ############

# Check to see what cores are in which states
states_and_cores <- gContains(states, core_coords, byid = TRUE)
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one state and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given state (column)

# Convert from matrix to dataframe so we can use pipelines
states_and_cores <- as.data.frame(states_and_cores)

# Identify columns as states
colnames(states_and_cores) <- states$STATE # STATES are the state names

# Identify rows as cores
states_and_cores <- states_and_cores %>%
  mutate(core_id = cores_pre_geography$core_id)

# Replace FALSE with NA, NA's are easier to remove
states_and_cores <- states_and_cores %>%
  mutate_if(is.logical, funs(ifelse(. == FALSE, NA, .)))

# Remove states that don't have any data (ones that have NA in every row)
not_all_na <- function(x) {
  !all(is.na(x))
}

# Only keep ones states with data
states_and_cores <- states_and_cores %>% 
  select_if(not_all_na)

# # Some new data additions might be exclusively international, in which case 
# #   the # of columns in states_and_cores would be 1 (only core IDs). If this
# #   is the case, just set all states to NA and move on.
# if (length(states_and_cores) == 1) {
#   
#   states_and_cores <- states_and_cores %>%
#     mutate(state = NA)
#   
# } else { # If there are non-international data, continue on
#   
#   # Now, if the value of a cell is TRUE, replace the value of the cell with the
#   # core_id instead
states_and_cores <- states_and_cores %>%
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) 

# Remove core_id column so it doesn't interfere with gather
states_and_cores <- states_and_cores %>%
  select(-core_id)

# Now gather the data so that all states are in one column
states_and_cores <- states_and_cores %>%
  gather(key = "state", value = "core_id")

# Remove rows with NA, which correspond to no-match pair between states and cores
states_and_cores <- na.omit(states_and_cores)

# # Coerce state attribute to character
# states_and_cores <- states_and_cores %>%
#   mutate(state = as.character(state))

#   }
# }

## 3. Add watershed attribute ##################
#if(nrow(CCRCN_cores_nowatershed) > 0) {

## 3. Assign Watershed attributes ###############

## ... 3A. Prep data to assign watershed attributes #############

# Convert core data to spatialPointsDataFrame, with coordinates and core IDs
# core_coords <- SpatialPointsDataFrame(
#   coords = data.frame(core_longitude = CCRCN_cores_nowatershed$core_longitude, 
#                       core_latitude = CCRCN_cores_nowatershed$core_latitude),
#   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),
#   data = data.frame(CCRCN_cores_nowatershed$core_id)
#   )

## ... 3B. Assign watersheds to cores without watershed assignments ############

# Check to see what cores are in which watersheds
watersheds_and_cores <- gContains(watersheds, core_coords, byid = TRUE)
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one watershed and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given watershed (column)

# Convert from matrix to dataframe so we can use pipelines
watersheds_and_cores <- as.data.frame(watersheds_and_cores)

# Identify columns as watersheds
colnames(watersheds_and_cores) <- watersheds$Abbrev # 'Abbrev is the unique 
# indicator for each watershed

# Identify rows as cores
watersheds_and_cores <- watersheds_and_cores %>%
  mutate(core_id = cores_pre_geography$core_id)

# Replace FALSE with NA, NA's are easier to remove
watersheds_and_cores <- watersheds_and_cores %>%
  mutate_if(is.logical, funs(ifelse(. == FALSE, NA, .)))

# Only keep ones watersheds with data
watersheds_and_cores <- watersheds_and_cores %>% 
  select_if(not_all_na)


# Some new data additions might be exclusively international, in which case 
#   the # of columns in states_and_cores would be 1 (only core IDs). If this
#   is the case, just set all states to NA and move on.
# if (length(watersheds_and_cores) == 1) {
#   
#   watersheds_and_cores <- watersheds_and_cores %>%
#     mutate(watershed = NA)
#   
# } else { # If there are non-international data, continue on
#   
# # Now, if the value of a cell is TRUE, replace the value of the cell with the
# # core_id instead
watersheds_and_cores <- watersheds_and_cores %>%
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) 

# Remove core_id column so it doesn't interfere with gather
watersheds_and_cores <- watersheds_and_cores %>%
  select(-core_id)

# Now gather the data so that all watersheds are in one column
watersheds_and_cores <- watersheds_and_cores %>%
  gather(key = "watershed", value = "core_id")

# Remove rows with NA, which correspond to no-match pair between watersheds and cores
watersheds_and_cores <- na.omit(watersheds_and_cores)

# We used the abbreviation code for watersheds while processing, now let's 
#   change it to the actual watershed name

# Pull the watershed names and codes from the spatialpolygonsdataframe
watershed_names <- data.frame(HUC_NAME = watersheds$HUC_NAME, 
                              watershed = watersheds$Abbrev)

# Join the names in
watersheds_and_cores <- watersheds_and_cores %>%
  left_join(watershed_names) %>%
  select(-watershed) %>%
  mutate(watershed = as.character(HUC_NAME)) %>%
  select(-HUC_NAME)

#   }
# }

## 4. Join state and watershed column to site data #########

# First need to join to core data to get site and study IDs
cores_post_states <- cores_pre_geography %>%
  left_join(states_and_cores, by = "core_id") %>%
  mutate(state = ifelse(is.na(state), "International", state))

cores_w_geography <- cores_post_states %>%
  left_join(watersheds_and_cores, by = "core_id") %>%
  mutate(watershed = ifelse(is.na(watershed), "International", watershed))

# the lengths of both dataframes should be exactly the same.
# if not, there is something wrong with either object
num_cores <- length(cores_pre_geography$core_id)
num_cores_w_geography <- length(cores_w_geography$core_id)
if(num_cores != num_cores_w_geography) {
  print("WARNING: the number of cores in the Core dataset and state dataset do not match!")
} else {
  
  geography_assignment <- cores_w_geography %>% 
    select(study_id, site_id, core_id, state, watershed)
}

# Assign country tags
dfr <- cores_pre_geography %>% 
  select(longitude, latitude, core_id) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(!is.na(core_id))

sfRegion <- st_as_sf(dfr, coords=c('longitude', 'latitude'))
sfCountry <- ne_countries(returnclass='sf')
st_crs(sfRegion) <- 4326

core_country_merge <- st_join(sfCountry, sfRegion) %>%
  st_set_geometry(NULL) %>%
  select(core_id, name) %>%
  filter(!is.na(core_id)) %>%
  rename(country = name)

geography_assignment <- merge(geography_assignment, core_country_merge, by="core_id", all.x=TRUE, all.y=TRUE)

write_csv(geography_assignment, "./data/CCRCN_synthesis/CCRCN_geography_lookup.csv")

country_summary <- st_join(sfCountry, sfRegion) %>%
  st_set_geometry(NULL) %>%
  select(core_id, name) %>%
  filter(!is.na(core_id)) %>%
  group_by(name) %>%
  summarize(n = n())

write_csv(country_summary, "./docs/country_summary.csv")

