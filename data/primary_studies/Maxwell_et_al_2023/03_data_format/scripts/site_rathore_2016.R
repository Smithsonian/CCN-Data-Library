## import data from Rathore et al 2016, International Journal for Phytoremediation
## from India
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 01.08.22
#edit 20.12.22

library(tidyverse)
library(measurements) #to convert to decimal degrees


input_file01 <- "reports/03_data_format/data/site_level/Rathore_2016_Table1/Rathore_2016_Table1.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Rathore et al 2016"
author_initials <- "APR"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Sites),
         Habitat_type = "Salt marsh",
         Country = "India") %>% 
  dplyr::rename(Site = Sites) %>% 
  mutate(Plot = Site)


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Lat = str_trim(Lat),
         Long = str_trim(Long)) %>% 
  mutate(lat = gsub("°", " ",
                    gsub("\\.", " ",
                         gsub("N", "", Lat))),
         long = gsub("°", " ",
                     gsub("\\.", " ",
                          gsub("E", "",  Long))), 
         lat_dec_deg = measurements::conv_unit(lat, from = "deg_min_sec", to = "dec_deg"), #N , Keep positive
         long_dec_deg = measurements::conv_unit(long, from = "deg_min_sec", to = "dec_deg"), #E , Keep positive
         Latitude = as.numeric(lat_dec_deg),
         Longitude = as.numeric(long_dec_deg),
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Year_collected = "2013-2014",
         Method = "Walkley-Black wet oxidation")


## edit depth and separate mean from 

input_data04 <- input_data03 %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = 20) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100) %>% # cm to m
  separate(OC_perc, c("OC_perc_mean", "OC_perc_s"), sep = '±') %>%    #separate upper and lower depth
  mutate(OC_perc_s  = gsub("[^0-9.-]", "", OC_perc_s)) #removing letters at end of string



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_s)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1080/15226514.2016.1146228")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



