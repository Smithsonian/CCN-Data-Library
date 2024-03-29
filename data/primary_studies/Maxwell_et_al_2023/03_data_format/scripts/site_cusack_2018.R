## import data from Cusack et al 2018, UK CEH dataset
## from Western coast of Arabian gulf
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.08.22

library(tidyverse)


input_file01 <- "reports/03_data_format/data/site_level/Cusack_2018_Table1/Cusack_2018_Table1.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Cusack et al 2018"
author_initials <- "MC"


input_data02 <- input_data01 %>% 
  rename(Site = Region, 
         Habitat_type = Habitat,
         Plot = Core.ID) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot),
         Country = "Saudi Arabia") 


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Latitude_N, #N
         Longitude = Longitude_E,
         OC_perc_mean = Corg_perc_mean,
         OC_perc_se = Corg_perc_se) %>% #east
  mutate(Year_collected = "",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA") %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = Depth_cm,
         U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_se,
                BD_g_cm3_mean, BD_g_cm3_se)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1088/1748-9326/aac899")


export_data03 <- export_data02 %>% 
  filter(Habitat_type == "Saltmarsh")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data03

write.csv(export_df, export_file)




