## import data from Camacho et al 2014, Journal of Integrated Coastal Zone Management 
## from la Guadiana, Southeast Portugal
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 03.08.22

library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Camacho_2014_Table3/Camacho_tabula_edit.csv"

input_data01 <- read.csv(input_file01)

## import locations
input_file02 <- "reports/03_data_format/data/core_level/Camacho_2014_Table3/locations_appendix3.csv"

locations01 <- read.csv(input_file02)

locations02 <- locations01 %>% 
  dplyr::select(Sample, Lat_DD, Long_DD)


### merge datasets
input_data02 <- left_join(input_data01, locations02, by = "Sample")




###mean of summer and winter
input_data03 <- input_data02 %>% 
  group_by(Sample, Lat_DD, Long_DD) %>% 
  summarise_at(vars(TOC:C.N), mean, na.rm = T)




##### add informational  
source_name <- "Camacho et al 2014"
author_initials <- "SC"


input_data04 <- input_data03 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Sample),
         Habitat_type = "Salt marsh",
         Site = "La Guadiana",
         Country = "Portugal") %>% 
  dplyr::rename(Plot = Sample)


#### reformat data ####

input_data05 <- input_data04 %>% 
  dplyr::rename(Latitude = Lat_DD,
         Longitude = Long_DD,
         OC_perc = TOC) %>% 
  mutate(Year_collected = "2010",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA",
         U_depth_m = 0,
         L_depth_cm = 2,
         L_depth_m = L_depth_cm/100,
         DOI = "https://doi.org/10.5894/rgci452")


#### export ####

export_data01 <- input_data05 %>% 
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


