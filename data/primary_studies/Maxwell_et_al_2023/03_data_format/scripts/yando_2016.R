## import data from Yando et al 2016, USGS dataset
## from Florida, Louisiana, Texas, 2012-2013
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 25.11.22


library(tidyverse)
library(janitor)
input_file01 <- "reports/03_data_format/data/core_level/Yando_2016_USGS/8_new_soil_data_collection_site_level_phyiscochemical_properties_10_14_2014.csv"

input_data01 <- read.csv(input_file01) %>% 
  slice(10:118) %>% 
  row_to_names(row_number = 1)


input_file02 <- "reports/03_data_format/data/core_level/Yando_2016_USGS/11_new_elevation_data_collection_site_level_elevation_10_14_2014.csv"

locations <- read.csv(input_file02) %>% 
  slice(10:46) %>% 
  row_to_names(row_number = 1)


### bind the two datasets

input_data02 <- left_join(input_data01, locations, by = c("Site", "State", "Habitat", "Typeid"))


##### add informational  
source_name <- "Yando et al 2016"
author_initials <- "ESY"


input_data03 <- input_data02 %>% 
  dplyr::rename(State_abbr = State) %>% 
  mutate(Core = Site,
         Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Country = "United States") %>% 
  mutate(State = case_when(State_abbr == "FL" ~ "Florida",
                           State_abbr == "LA" ~ "Louisiana",
                           State_abbr == "TX" ~ "Texas")) %>% 
  mutate(Habitat_type = case_when(Habitat == "MF" ~ "Mud flat",
                                  Habitat == "SM" ~ "Saltmarsh")) 

#### reformat data ####

input_data04 <- input_data03 %>% 
  rename(Latitude = Lat,
         Longitude = Long,
         SOM_perc = SOM, 
         BD_reported_g_cm3 = BD,
         Year_collected = Year) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "LOI",
         Conv_factor = NA)


## edit depth

input_data05 <- input_data04 %>% 
  mutate(Depth = fct_recode(Depth, "5-15" = "15-May")) %>% 
  separate(Depth, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

test <-  input_data05 %>% 
  mutate(GPS_combined = paste(Latitude, Longitude)) %>% 
  group_by(Source, Site_name)  %>% 
  dplyr::summarise(distinct_location = n_distinct(GPS_combined))


#### export ####

export_data01 <- input_data05 %>% 
  filter(Habitat_type == "Saltmarsh")


export_data02 <- export_data01 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, State, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, SOM_perc, BD_reported_g_cm3, Conv_factor)


export_data03 <- export_data02 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, State, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1111/1365-2745.12571")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data03

write.csv(export_df, export_file)

