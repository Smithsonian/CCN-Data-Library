## import data from Pollman et al 2021, CATENA
# published in PANGEA
#https://doi.org/10.1594/PANGAEA.932812
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 18.08.22
#edit 20.12.22

library(tidyverse)
library(stringr) # for str_split_i


input_file01 <- "reports/03_data_format/data/core_level/Pollman_2021_PANGEA/Pollman_2021.csv"

input_data01 <- read.csv(input_file01) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _


##### add informational  
source_name <- "Pollman et al 2021"
author_initials <- "TP"


input_data02 <- input_data01 %>% 
  mutate(Site = "Spiekerook Ostplate",
         Core = stringr::str_extract(Sample_ID, "[^_]+")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Core),
         Habitat_type = "Salt marsh",
         Country = "Germany") 


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = "2016") %>% # all other sites Nov 2016
  dplyr::rename(OC_perc = TOC__,
         U_depth_m = Depth_top_m_,
         L_depth_m = Depth_bot_m_) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA", 
         BD_reported_g_cm3 = NA,
         DOI = "https://doi.org/10.1594/PANGAEA.932812")


#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
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


