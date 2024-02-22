## import data from Nogueira et al 2021, Limnology and Oceanography
#https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lno.11992
#https://doi.pangaea.de/10.1594/PANGAEA.925346
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.07.22
# edit 20.12.22

library(tidyverse)
library(measurements) #convert deg min sec to decimal degrees 
#to bind 
library(plyr)

#to check location points
library("ggmap")
library(maptools)
library(maps)

input_file01 <- "reports/03_data_format/data/core_level/Nogueira_2020_PANGEA/THI_geochem_edit.tab"
input_file02 <- "reports/03_data_format/data/core_level/Nogueira_2020_PANGEA/THIll_geochem_edit.tab"


input_data_inner01 <- read.table(input_file01, sep = '\t', header = T)
input_data_main01 <- read.table(input_file02, sep = '\t', header = T)


input_data_inner01 <-  input_data_inner01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _

input_data_main01 <-  input_data_main01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) #replacing . in columns by _

#### add locations for each before merging ####


input_data_inner02 <- input_data_inner01 %>% 
  mutate(lat_detail ="27°59.139", #North
         long_detail = "12°17.109", #West
         lat_D = 27,
         long_D = 12,
         lat_M = 59.139,
         long_M = 17.109,
         Latitude = lat_D + lat_M/60, #N , Keep positive
         Longitude = (long_D + long_M/60)*-1, #W , convert to neg
         accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Site = "Lacustrine_lagoon_THI")

input_data_main02 <- input_data_main01 %>% 
  mutate(lat_detail = "28°01.626",
         long_detail = "12°16.558",
         lat_D = 28,
         long_D = 12,
         lat_M = 1.626,
         long_M = 16.558,
         Latitude = lat_D + lat_M/60, #N , Keep positive
         Longitude = (long_D + long_M/60)*-1, #W , convert to neg
         accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Site = "Lacustrine_lagoon_THIII")

input_data01 = plyr::rbind.fill(input_data_inner02, input_data_main02)


##### add informational  
source_name <- "Noguiera et al 2022"
author_initials <- "JN"


input_data02 <- input_data01 %>% 
  dplyr::rename(Core = Site) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Lagoon near marsh",
         Country = "Morocco", 
         Site = "Lacustrine lagoon") 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = "2016") %>% # estimate from paper
  mutate(OC_perc = TOC__,
         U_depth_m = Depth_m_,
         L_depth_m = Depth_m_ + 0.02) %>% #2cm core layers
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA", 
         BD_reported_g_cm3 = "measured but not in dataset",
         DOI = "https://doi.org/10.1594/PANGAEA.925024")

#### check location points ####


mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = input_data03, aes(x = Longitude, y = Latitude, 
                                      color = Site), alpha = 0.5)
mp



#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Core,  Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) 


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



