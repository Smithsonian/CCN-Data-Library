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

# Could be useful for a generalized function to be written that will
# locate each cores table within any desired spatial polygon object 
# ex. locate cores within...country polygons, state polygons, EEZs, etc

## 2. Read in data ####

# Load  shapefiles from rdata
load("data/input_shapefiles/assign_geography.rdata")

# EEZs
# 200 nautical mile EEZ shapefile 
# located all the nearshore cores (although its much larger to load into R)
# eez <- readOGR(dsn = "./data/input_shapefiles/World_EEZ_v11_20191118/",
#                   layer = "eez_v11")
# the 24 nautical mile eez shapefile did not capture all the cores outside of the country shapefile

# world countries
# countries <- readOGR(dsn = "./data/input_shapefiles/world_countries/",
#                      layer = "country")

# US States
# states <- readOGR(dsn = "./data/input_shapefiles/us_states/states_political_boundaries",
#                      layer = "state_pol")

# admin divisions
# divisions <- readOGR(dsn = "./data/input_shapefiles/admin_divisions/",
#                      layer = "admin")

# try saving all shapefiles as Rdata file (for quicker upload)
# save(countries, eez, divisions, states, file="data/input_shapefiles/assign_geography.rdata")

# convert to spatial data
countries_sp <- spTransform(countries, CRS("+proj=longlat +datum=WGS84 +no_defs"))
eez_sp <- spTransform(eez, CRS("+proj=longlat +datum=WGS84 +no_defs"))
states_sp <- spTransform(states, CRS("+proj=longlat +datum=WGS84 +no_defs"))
divisions_sp <- spTransform(divisions, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# remove large shapefiles from R's memory
rm(countries, eez, divisions, states)

# Core data updated to V2 guidance
cores <- ccrcn_synthesis$cores %>% # read straight from synthesis
  mutate(across(c("latitude", "longitude"), as.numeric)) %>% 
  rename(original_core_id = core_id) %>%
  # create a REAL unique core id
  mutate(core_id = str_c(study_id, original_core_id, sep = "_"))
cores_no_na <- cores %>% drop_na(longitude) # remove cores with NA coords

# THIS SCRIPT ASSUMES ALL CORE IDS are unique
# length(unique(cores$core_id)) == nrow(cores)
# duplicates <- cores %>% add_count(study_id, site_id, core_id) %>%
#   filter(n>1)

# site table not in V2 folder yet..
# site data will be joined once the geography attributes are added
# sites <- read_csv("data/CCRCN_synthesis/CCRCN_sites.csv", guess_max=6206)

# sp_cores  <- sp::SpatialPoints(cores_no_na[c('longitude','latitude')], proj4string = CRS(projection))
# res <- rgeos::gContains(countries_sp, sp_cores, byid=T, prepared=T, returnDense=F)
# cores_no_na$country <- apply(res, 1, which)
# which(res == TRUE, arr.ind = )

## 3. Assign Geography to Cores ####

# Convert core data to spatialPointsDataFrame, with coordinates and core IDs
spatial_cores <- SpatialPointsDataFrame(
  coords = data.frame(longitude = cores_no_na$longitude, 
                      latitude = cores_no_na$latitude),
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),
  data = data.frame(cores_no_na$core_id)
)

# ... 3a. Assign country attribute to core ID ####

# Check to see what cores are in which countries
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one state and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given state (column)
country_cores <- gContains(countries_sp, spatial_cores, byid = TRUE)
# admin_cores <- gContains(divisions, spatial_cores, byid = TRUE)

# Convert from matrix to dataframe so we can use pipelines
country_cores <- as.data.frame(country_cores)

# Identify columns as countries
colnames(country_cores) <- countries_sp$COUNTRY

# the non-generalized country shp has 3 NA elements
country_cores <- country_cores[, which(!is.na(names(country_cores)))]

# clean up the dataframe to isolate cores and countries
country_cores_trim <- country_cores %>%
  # Identify rows as cores
  mutate(core_id = cores_no_na$core_id) %>%
  # Replace FALSE with NA
  mutate_if(is.logical, list(~na_if(., FALSE))) %>% 
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
  mutate(country = gsub("[.]1|[.]2|[.]3|[.]4|[.]5", "", country)) %>%
  drop_na(core_id)

# Merge the country assignments to the core table
# core_geography <- left_join(cores, country_cores_gather)

## ... 3b. Assign us states to core ID ####

# Check to see what cores are in which countries
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one state and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given state (column)
state_cores <- gContains(states_sp, spatial_cores, byid = TRUE)

# Convert from matrix to dataframe so we can use pipelines
state_cores <- as.data.frame(state_cores)

# Identify columns as states
colnames(state_cores) <- states_sp$STATE

# clean up the dataframe to isolate cores and states
state_cores_trim <- state_cores %>%
  # Identify rows as cores
  mutate(core_id = cores_no_na$core_id) %>% 
  # Replace FALSE with NA
  mutate_if(is.logical, funs(ifelse(. == FALSE, NA, .))) %>%
  # Remove states that don't have any data (ones that have NA in every row)
  select_if(function(x) {!all(is.na(x))}) %>%
  # replace the value of the cell with the core_id if the value of a cell == TRUE
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) %>%
  select(-core_id)

# Now gather the data so that all states are in one column
state_cores_gather <- state_cores_trim %>%
  gather(key = "us_state", value = "core_id") %>%
  drop_na(core_id)

## ... 3c. Assign administrative divisions attribute to core ID ####

# determine the frequency of admin types in each country of the divisions shapefile
country_admins <- data.frame(admin_type = as.character(divisions_sp@data[["ADMINTYPE"]]),
                         admin_country = as.character(divisions_sp@data[["COUNTRY"]])) %>%
  distinct() %>% arrange(admin_country) %>% add_count(admin_type)
  # add_count(admin_type) %>% 
  # distinct() %>% 
  # arrange(desc(n))

# isolate states, provinces, territories from admin shp
# more admin types can be added to have more inclusion
# these three capture about 40% of the admin divisions in the shapfile
admin_types <- c("State", "Province", "Territory")
# alternatives: District, Region

# subset the admin divisions by selected types (otherwise it will take a very long time)
# admin_states <- st_as_sf(divisions)
admin_subset <- divisions_sp[divisions_sp@data$ADMINTYPE %in% admin_types, ]
# NAME = name of admin division
# COUNTRY = country of the corresponding admin division

admin_cores <- gContains(admin_subset, spatial_cores, byid = TRUE)

# Convert from matrix to dataframe so we can use pipelines
admin_cores <- as.data.frame(admin_cores)

# Identify columns as admins
colnames(admin_cores) <- str_c(admin_subset$NAME, admin_subset$COUNTRY, sep = "_")
# store the corresponding countries => might not be necessary
# admin_countries <- admin_subset$COUNTRY

# eliminate NA elements (this also makes the colnames unique)
admin_cores <- admin_cores[, which(!is.na(names(admin_cores)))]

# clean up the dataframe to isolate cores and admins
admin_cores_trim <- admin_cores %>%
  # Identify rows as cores
  mutate(core_id = cores_no_na$core_id) %>%
  # Replace FALSE with NA
  mutate_if(is.logical, funs(ifelse(. == FALSE, NA, .))) %>%
  # Remove admins that don't have any data (ones that have NA in every row)
  select_if(function(x) {!all(is.na(x))}) %>%
  # replace the value of the cell with the core_id if the value of a cell == TRUE
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) %>%
  select(-core_id)

# Now gather the data so that all admins are in one column
admin_cores_gather <- admin_cores_trim %>%
  gather(key = "admin_division", value = "core_id") %>%
  mutate(admin_division = gsub("[.]1|[.]2|[.]3|[.]4|[.]5", "", admin_division)) %>%
  drop_na(core_id) %>%
  separate(admin_division, into = c("admin_division", "admin_country"), sep = "_")

## ... 3d. Assign EEZs to core ID ####

# Check to see what cores are in which countries
# This returns a matrix of logical outputs (TRUE or FALSE). Each column corresponds
#   to one state and each row corresponds to one state. A TRUE value in a
#   cell indicates that the given core (row) is in the given state (column)
eez_cores <- gContains(eez_sp, spatial_cores, byid = TRUE)

# Convert from matrix to dataframe so we can use pipelines
eez_cores <- as.data.frame(eez_cores)

# Identify columns as states
colnames(eez_cores) <- eez_sp$TERRITORY1

# this names the columns uniquely (workaround)
eez_cores <- eez_cores[, which(!is.na(names(eez_cores)))]

# clean up the dataframe to isolate cores and countries
eez_cores_trim <- eez_cores %>%
  # Identify rows as cores
  mutate(core_id = cores_no_na$core_id) %>%
  # Replace FALSE with NA
  mutate_if(is.logical, list(~na_if(., FALSE))) %>% 
  # Remove countries that don't have any data (ones that have NA in every row)
  select_if(function(x) {!all(is.na(x))}) %>%
  # replace the value of the cell with the core_id if the value of a cell == TRUE
  mutate_all(~ as.character(.x)) %>%
  mutate_at(vars(-core_id), funs(
    ifelse(. == TRUE, core_id, NA)
  )) %>%
  select(-core_id)

# Now gather the data so that all countries are in one column
eez_cores_gather <- eez_cores_trim %>%
  gather(key = "country_eez", value = "core_id") %>%
  mutate(country_eez = gsub("[.]1|[.]2|[.]3|[.]4|[.]5", "", country_eez)) %>%
  drop_na(core_id)

## 4. Table Merge ####

# Merge the geography assignments to the core table
core_geography <- left_join(cores, country_cores_gather) %>%
  left_join(state_cores_gather) %>%
  left_join(admin_cores_gather) %>%
  left_join(eez_cores_gather) %>%
  # the state shapefile is more inclusive of coastal waters than the admin divisions
  # merge the us state and international admin divisions fields into one column
  mutate(admin_division = ifelse(!is.na(us_state) & is.na(admin_division), us_state, admin_division),
         country = ifelse(is.na(country), country_eez, country)) %>%
  select(-c(us_state, admin_country, country_eez, core_id)) %>%
  rename(core_id = original_core_id)

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

## 5. Visual QA: Plot coordinates ####

## Static 

# world <- ne_countries(scale=110)

# plot the cores with associated countries
# too many countries to tell if there are incorrect assignments
# use leaflet with popup instead
# ggplot() +  
#   geom_polygon(data=world, aes(x=long, y=lat, group=group),  
#                color="white", lwd = .25) +
#   # plot core locations
#   geom_point(data = core_geography, mapping = aes(x = longitude, y = latitude, col = country))

## Interactive
# library(leaflet)
# 
# # plot cores with no associated admin division
# core_geography %>% filter(is.na(admin_division)) %>% 
#   leaflet() %>% 
#   addProviderTiles(providers$CartoDB) %>%
#   addCircleMarkers(lng = ~longitude, lat = ~latitude, 
#                    radius = 2, label = ~country)


# THIS MAP IS UNECESSARY (the EEZ shapefile fills in the previous NAs)
# # map all the cores and indicate their geography assignment
# # and whether a core is geo-located or in international waters
# core_geography_leaflet <- core_geography %>%
#   mutate(geography_flag = ifelse(!is.na(country), "Geography Located", country))
# 
# # define palette 
# pal <- colorFactor(palette = "PRGn", 
#                    # reverse = T,
#                    core_geography_leaflet$geography_flag)
# 
# m2 <- leaflet(core_geography_leaflet) %>% 
#   addProviderTiles(providers$CartoDB) %>%
#   # addTiles() %>%
#   addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
#                    radius = 5, color = ~pal(geography_flag), label = ~country) %>%
#   # addPopups(lng, lat, content, layerId = core_id) %>%
#   addLegend(pal = pal, values = ~geography_flag)
# m2
# 
# withr::with_dir("data/QA", saveWidget(m2, file="assigned_geography.html"))

# 6. Update core table in CCN synthesis ####

ccrcn_synthesis$cores <- core_geography # will overwrite initial synthesis core table with updated geography

# write_csv(core_geography, "data/CCRCN_synthesis/derivative/CCRCN_cores.csv") # core table is now passed onto the next post-processing function

## 7. Clear variables taking up memory ####

rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results"))])

