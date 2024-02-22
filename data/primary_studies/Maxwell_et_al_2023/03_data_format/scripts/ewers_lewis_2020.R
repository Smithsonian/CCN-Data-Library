## import data from Ewers Lewis et al 2020 Dataverse; 
## from Ecosystems 2018
## same data used in Young et al 2018, Biology Letters & Dryad but cleaner and clearer
## export for marsh soil C (and previously mangrove soil C)
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.05.22
#edit 20.12.22

library(tidyverse)
library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date
library(janitor) # to clean names
#library(lubridate) #to clean dates - not good for TOC, etc

input_file <- "reports/03_data_format/data/core_level/Ewers_Lewis_2020/Ewers_Lewis_2020_dataverse_for_reformat.csv"

input_data0 <- read.csv(input_file)



##### format data  #####

input_data1 <- input_data0 %>% 
  slice(1:858) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%  #replacing . in columns by _
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(1:21) %>% 
  mutate_all(na_if,"")

##### add informational  #####

source_name <- "Ewers Lewis et al 2020"
author_initials <- "CEL"


input_data2 <- input_data1 %>% 
  rename(Habitat_type = Habitat) %>% 
  mutate(Habitat_type = fct_recode(Habitat_type, "Mangrove" = "MG", 
                                   "Tidal marsh" = "SM", "Seagrass" = "SG")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Core = paste(Location, Site, "Rep", Replicate),
         Site_name = paste(Source_abbr,Core)) %>% 
  rename(Site_number = Site) %>% 
  mutate(Site = coalesce(Park, NearbyPark, Location))


## add information from paper



#### site data #####

input_data3 <- input_data2 %>% 
  rename(Latitude = North,
         Longitude = East) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Year_collected = 2014,
         Country = "Australia")

##### horizon data  #####

#from metadata
# Percent_C: Percentage of sample that is organic carbon (by dry weight); calculated by dividing (X mg C/g sediment) by (1,000 mg/g), 
#then multiplying by 100 to convert to percent.

input_data4 <- input_data3 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%  #separate upper and lower depth
  mutate(U_depth_m = as.numeric(as.character(U_depth_cm))/100 , #cm to m
         L_depth_m = as.numeric(as.character(L_depth_cm))/100) %>% # cm to m 
  rename(OC_perc = Percent_C, 
         BD_reported_g_cm3 = DBD_g_cm3) %>%   #1 g cm-3 = 1 Mg m-3
mutate(Method = "MIR predicted",
       DOI = "https://doi.org/10.7910/DVN/6PFBO0")

#### export data ####

export_data <- input_data4 %>% 
  select(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
         accuracy_flag, accuracy_code, Country, Year_collected, U_depth_m, L_depth_m, Method,
         OC_perc, BD_reported_g_cm3, TC_mg_g, OC_mg_g, IC_mg_g, TN_mg_g, DOI)

## subset for marsh
export_data_marsh <- export_data %>% 
  filter(Habitat_type == "Tidal marsh")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data_marsh


write.csv(export_df, export_file)
