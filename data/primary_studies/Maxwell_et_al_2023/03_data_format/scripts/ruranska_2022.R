## import data from Ruranska et al 2022, UK CEH dataset
## from England and Wales surface soils 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 11.08.22
# edit 20.12.22
# edit 15.05.23 added soil type

library(tidyverse)


input_file01 <- "reports/03_data_format/data/core_level/Ruranska_2022_UKCEH/Eng_Wales_SM_Surficial.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Ruranska et al 2022"
author_initials <- "PR"


input_data02 <- input_data01 %>%
  slice(1:212) %>% 
  rename(Site = Saltmarsh_ID,
         Soil_type = Soil_Texture) %>% 
  group_by(Site) %>% 
  mutate(n_core = row_number()) %>% 
  ungroup() %>% 
  mutate(Core = paste(Site, n_core)) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Salt marsh",
         Country = "UK") 



#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Lat_dec_deg,
         Longitude = Long_dec_deg,
         Year_collected = Year,
         OC_perc = OC_Perc, 
         SOM_perc = LOI_Perc,
         BD_reported_g_cm3 = Dry_Bulk_Density_g_cm_3) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.5285/e5554b83-910f-4030-8f4e-81967dc7047c")# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Soil_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


