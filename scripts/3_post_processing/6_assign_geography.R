# Coastal Carbon Research Coordination Network
# Database Guidance V2

# Synthesis Post-Processing Script
# The code iterates through the CCN database, which has already been converted to V2 guidance
# and assigns country and administrative division to each core
# Contact: Jaxine Wolfe, wolfejax@si.edu

## 1. Prepare workspace ####
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(rnaturalearth)


## 2. Read in data ####

# Load  shapefiles
# countries
countries <- readOGR(dsn = "./data/input_shapefiles/world/country_gen/",
                  layer = "country_gen")
countries_sp <- spTransform(countries, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# admin divisions
# divisions <- readOGR(dsn = "./data/input_shapefiles/world/admin/",
#                      layer = "admin")
# divisions <- spTransform(divisions, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Core data updated to V2 guidance
cores <- read_csv("data/CCRCN_V2/cores.csv", guess_max=6206)
cores_no_na <- cores %>% drop_na(longitude) # remove cores with NA coords

# site table not in V2 folder yet..
# site data will be joined once the geography attributes are added
# sites <- read_csv("data/CCRCN_synthesis/CCRCN_sites.csv", guess_max=6206)


## 3. Assign Geography to Cores ####

# ... 3a. Assign country attribute to core ID ####

# Convert core data to spatialPointsDataFrame, with coordinates and core IDs
spatial_cores <- SpatialPointsDataFrame(
  coords = data.frame(longitude = cores_no_na$longitude, 
                      latitude = cores_no_na$latitude),
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),
  data = data.frame(cores_no_na$core_id)
)

# Check to see what cores are in which countries
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one state and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given state (column)
country_cores <- gContains(countries_sp, spatial_cores, byid = TRUE)
# admin_cores <- gContains(divisions, spatial_cores, byid = TRUE)

# Convert from matrix to dataframe so we can use pipelines
country_cores <- as.data.frame(country_cores)
# admin_cores <- as.data.frame(admin_cores)

# Identify columns as countries
colnames(country_cores) <- countries_sp$Country
# colnames(admin_cores) <- divisions$NAME

# clean up the dataframe to isolate cores and countries
country_cores_trim <- country_cores %>%
  # Identify rows as cores
  mutate(core_id = cores$core_id) %>% 
  # Replace FALSE with NA
  mutate_if(is.logical, funs(ifelse(. == FALSE, NA, .))) %>%
  # Remove countries that don't have any data (ones that have NA in every row)
  select_if(function(x) {!all(is.na(x))}) %>%
  # replace the value of the cell with the core_id if the value of a cell == TRUE
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) %>%
  select(-core_id)

# Now gather the data so that all countries are in one column
country_cores_gather <- country_cores_trim %>%
  gather(key = "country", value = "core_id") %>%
  drop_na(core_id)

# Merge the country assignments to the core table
core_geography <- left_join(cores, country_cores_gather)

## ... 3b. Assign administrative divisions attribute to core ID ####
# still looking for a good shapefile

## 4. Visual QA: Plot coordinates ####
# not a great graph yet, maybe use leaflet
# there are still NAs where there should be associated countries

countries <- ne_countries(scale=110)

# plot the cores with associated countries
# too many countries to tell if there are incorrect assignments
# use leaflet with popup instead
ggplot() +  
  geom_polygon(data=countries, aes(x=long, y=lat, group=group),  
               color="white", lwd = .25) +
  # plot core locations
  geom_point(data = core_geography %>% filter(!is.na(country)), 
             mapping = aes(x = longitude, y = latitude, col = country))

# plot the cores that do not have an associated country
ggplot() +  
  geom_polygon(data=countries, aes(x=long, y=lat, group=group),  
               color="white", lwd = .25) +
  # plot core locations
  geom_point(data = core_geography %>% filter(is.na(country)), 
             mapping = aes(x = longitude, y = latitude),
             color = "salmon")


# 5. Write updated core table to CCRCN V2 folder ####
write_csv(core_geography, "data/CCRCN_V2/core_geography.csv")