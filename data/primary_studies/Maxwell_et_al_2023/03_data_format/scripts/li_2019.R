## import data from Li et al 2019, Figure 2 in PlosONE
# https://doi.org/10.1371/journal.pone.0210768
## from Yellow River Delta, China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 29.07.22
# edit 20.12.22

library(tidyverse)


input_file01 <- "reports/03_data_format/data/core_level/Li_2019_Table1/li_2019_locations_data.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
  slice(1:39)


##### add informational  
source_name <- "Li et al 2019"
author_initials <- "YL"


input_data02 <- input_data01 %>% 
  group_by(Location) %>% 
  mutate(Rep = row_number()) %>% 
  ungroup() %>% 
  mutate(Site = substring(Location,1, nchar(Location)-1), #site name without number at the end
         Plot = paste(Location, Rep, sep = "-")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site),
         Habitat_type = "Salt marsh",
         Country = "China")

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Latitude = gsub("N", "", Latitude),
         Longitude = gsub("E", "", Longitude)) %>% 
  mutate(Latitude = gsub(" ", "", Latitude),
         Longitude = gsub(" ", "", Longitude)) %>% 
  mutate(OC_perc = SOC_g_kg/10) %>% #to go to percent
  mutate(Year_collected = "2018",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_cm  = 0,
         L_depth_cm = 15) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1371/journal.pone.0210768")# cm to m

#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



