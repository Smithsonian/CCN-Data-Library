## import data from Smeaton et al 2021, UK CEH dataset
## from Kyle of Tongue saltmarsh, Scotland, 2018
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 04.07.22
#edit 15.05.23 added soil type from Simplified_Troels_Smith column

library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date
library(janitor)

#to check location points
library("ggmap")
library(maptools)
library(maps)


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Smeaton_2021_UKCEH/Hammer_Cores_KoT.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Smeaton et al 2021"
author_initials <- "CS"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Salt marsh",
         Site = "Kyle of Tongue",
         Nation = "Scotland",
         Country = "UK") %>% 
  rename(Plot = Core)

#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Lat_Decimal_Deg,
         Longitude = Long_Decimal_Deg,
         OC_perc = Mean_OC_perc, 
         BD_reported_g_cm3 = Dry_Bulk_Density_g_cm_3,
         Soil_type = Simplified_Troels_Smith) %>% 
  mutate(Date = lubridate::dmy(Collection_Date)) %>% 
  mutate(Year_collected = lubridate::year(Date), #separate Year, Month, Day
         Month = lubridate::month(Date), 
         Day = lubridate::day(Date)) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Country, Nation, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type,Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Nation, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.5285/b57ef444-54d4-47f9-8cbf-3cfef1182b55")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


