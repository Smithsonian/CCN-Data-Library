## import data from Guerra et al 2022, SEANOE dataset
## from Northwestern Adriatic Sea
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.07.22

library(tidyverse)
library(stringr) # extract first n values for date
library(janitor) # to clean names

input_file01 <- "reports/03_data_format/data/core_level/Guerra_2022_SEANOE/72917_Guerra.csv"

input_data01 <- read.csv(input_file01)


### format data

input_data01 <- input_data01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _
 



##### add informational  
source_name <- "Guerra et al 2022"
author_initials <- "RG"


input_data02 <- input_data01 %>% 
  slice(1:78) %>% 
  rename(Core = X_Sampling_ID,
         Habitat_type = Habitat) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Site = "Northwestern Adriatic Sea",
         Country = "Italy") 


#### reformat data ####


input_data03 <- input_data02 %>% 
  # converted directly in csv file
  # mutate(lat_detail = Latitude_N,
  #        long_detail = Longitude_E,
  #        lat = gsub("°", " ",
  #                   gsub("\\.", " ", lat_detail)),
  #        long = gsub("°", " ",
  #                    gsub("\\.", " ", long_detail)),
  #        lat_dec_deg = measurements::conv_unit(lat, from = "deg_min_sec", to = "dec_deg"), #N , Keep positive
  #        long_dec_deg = measurements::conv_unit(long, from = "deg_min_sec", to = "dec_deg"), #E , Keep positive
  #        Latitude = as.numeric(lat_dec_deg),
  #        Longitude = as.numeric(long_dec_deg)) %>% 
  mutate(BD_reported_g_cm3 = NA,
         Year_collected = "2008") %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(layer_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.17882/73534")# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

#check SOM vs OC

plot(export_data02$SOM_perc, export_data02$OC_perc)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



