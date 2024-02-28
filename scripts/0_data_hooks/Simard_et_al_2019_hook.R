## CCN Data Library ########

## Soil core data curation script for Simard et al 2019 biomass data
## contact: wolfejax@si.edu, bettsh@si.edu


# Dataset: https://doi.org/10.3334/ORNLDAAC/1665
# Associated article: https://doi.org/10.1038/s41561-018-0279-1

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# load in data 
raw_treedata <- read_csv("data/primary_studies/Simard_et_al_2019/original/North_South_America_tree_measurements.csv",
                     na = "-9999")

## Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Simard_et_al_2019"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

# function to extract mode
getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

treedata <- raw_treedata %>% 
  rename(habitat = biome,
         plot_id = plot_name,
         latitude = lat1, longitude = lon1,
         plant_id = ID) %>% 
  mutate(study_id = id,
         site_id = str_c(region, subregion, sep = "_"),
         year = year(as.Date(date)),
         month = month(as.Date(date)),
         day = day(as.Date(date)),
         plot_shape = case_when(plot_shape %in% c("circular", "r") ~ "circle",
                                plot_shape == "s" ~ "square",
                                T ~ plot_shape),
         plot_radius = case_when(plot_shape == "circle" ~ sqrt(plot_area/pi),
                                 T ~ NA),
         field_or_manipulation_code = "field",
         
         # correct for longitude entry error and remove sites listed off the coast until author provides corrected positions
         longitude = case_when(longitude > 0 & site_id %in% c("south_america_Columbia", "south_america_tropical_Pacific_Colombia_Ecuador", "central_america_Honduras") ~ -longitude,
                               site_id == "south_america_tropical_Pacific_Colombia_Ecuador" & longitude < -79 ~ NA,
                               T ~ longitude),
         latitude = case_when(site_id == "south_america_tropical_Pacific_Colombia_Ecuador" & longitude < -79 ~ NA,
                              T ~ latitude),
         
         plot_center_latitude = latitude,
         plot_center_longitude = longitude,
         plant_plot_detail_present = "yes",
         soil_core_present = "no",
         harvest_or_allometry = "allometry",
         alive_or_dead = case_when(live == 0 ~ "dead",
                                   live == 1 ~ "alive",
                                   T ~ NA),
         above_or_belowground = "aboveground",
         allometric_eq_present = "no",
         plant_measurements_present = "yes",
         diameter = dbh,
         diameter_method = "breast height",
         diameter_unit = "cm",
         species_counter = paste(genus, species, sep = " "),
         n_plants = 1,
         height_unit = "meters",
         height_method = "clinometer") %>% 
  distinct()

tree_summary <- treedata %>% 
  group_by(site_id) %>% 
  summarise(dominant_species = getmode(species_counter)) 

## ... Plot Summary ####
plot_summary <- full_join(treedata, tree_summary) %>% 
  select(study_id, site_id, plot_id, plot_area, plot_shape, plot_center_latitude, plot_center_longitude,
         plot_radius, year, month, day, field_or_manipulation_code, habitat, dominant_species, plant_plot_detail_present, soil_core_present) %>% 
  distinct()

## ... Plant-Plot Detail ####
plant_plot_detail <- treedata %>% 
  select(study_id, site_id, plot_id, plot_area, 
         longitude, latitude, year, month, day, harvest_or_allometry,
         genus, species, alive_or_dead, above_or_belowground, 
         allometric_eq_present, plant_measurements_present)
         
## ... Plant ####
plant <- treedata %>% 
  select(study_id, site_id, plot_id, plant_id, year, month, day, genus, species, alive_or_dead, above_or_belowground,
         diameter, diameter_method, diameter_unit)

study_citations <- data.frame(study_id = id, 
                             bibliography_id = "Simard_et_al_2019_data",
                             publication_type = "primary dataset",
                             bibtype = "misc", 
                             title = "Global Mangrove Distribution, Aboveground Biomass, and Canopy Height",
                             author = "Simard, M., T. Fatoyinbo, C. Smetanka, V.H. Rivera-monroy, E. Castaneda, N. Thomas, and T. Van der stocken",
                             doi = "10.3334/ORNLDAAC/1665",
                             url = "https://doi.org/10.3334/ORNLDAAC/1665",
                             year = "2019",
                             journal = NA,
                             volume = NA,
                             pages = NA) %>% 
  add_row(study_id = id, 
          bibliography_id = "Simard_et_al_2019_article",
          publication_type = "associated source",
          bibtype = "article", 
          title = "Mangrove canopy height globally related to precipitation, temperature and cyclone frequency",
          author = "Simard, M., T. Fatoyinbo, C. Smetanka, V.H. Rivera-monroy, E. Castaneda, N. Thomas, and T. Van der stocken",
          doi = "10.1038/s41561-018-0279-1",
          url = "https://doi.org/10.1038/s41561-018-0279-1",
          year = "2019",
          journal = "Nature Geoscience",
          volume = 12,
          pages = "40-45")

## Mapping
leaflet(plant_plot_detail) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~plot_id)


## Write Curated Data ####

# write data to final folder
write_csv(plot_summary, "./data/primary_studies/Simard_et_al_2019/derivative/Simard_et_al_2019_plot_summary.csv")
write_csv(plant, "./data/primary_studies/Simard_et_al_2019/derivative/Simard_et_al_2019_plant.csv")
write_csv(plant_plot_detail, "./data/primary_studies/Simard_et_al_2019/derivative/Simard_et_al_2019_plant_plot_detail.csv")
write_csv(study_citations, "./data/primary_studies/Simard_et_al_2019/derivative/Simard_et_al_2019_study_citations.csv")
