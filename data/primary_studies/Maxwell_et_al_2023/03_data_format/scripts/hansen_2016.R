## import data from Hansen et al 2016, Supplementary
## from Elbe Estuary, Germany
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 14.09.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Hansen_2016_SI/Hansen_et_al_2016_TableS1.csv"

input_data01 <- read.csv(input_file01) %>%  
  fill(Sample_ID, .direction = "down") 


input_file02 <- "reports/03_data_format/data/core_level/Hansen_2016_SI/Hansen_et_al_2016_locations.csv"

locations <- read.csv(input_file02) %>% 
  rename(Latitude = Y, 
         Longitude = X,
         Sample_ID = Name) %>% 
  select(-description)


input_data02 <- left_join(input_data01, locations, by = "Sample_ID")



##### add informational  
source_name <- "Hansen et al 2016"
author_initials <- "KH"


input_data03 <- input_data02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Sample_ID),
         Habitat_type = "Salt marsh",
         Site = "Elbe Estuary",
         Country = "Germany") %>% 
  rename(Core = Sample_ID)



#### reformat data ####

input_data04 <- input_data03 %>% 
  rename(OC_perc = TOC_perc) %>% 
  mutate(Year_collected = "2010", 
         Year_collected_end = "2012",
    accuracy_flag = "estimated from GEE",
         accuracy_code = "2") %>% 
  mutate(Method = "EA",
         U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1007/s11368-016-1500-8")# cm to m



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


