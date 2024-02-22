## import data from Smeaton et al unpublished Essex dataset
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 16.09.22



library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Smeaton_unpublished_Essex/Wide_Essex.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Smeaton unpublished Essex"
author_initials <- "CS"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core_ID),
         Habitat_type = "Salt marsh",
         Nation = "England",
         Country = "UK") %>% 
  rename(Site = Saltmarsh,
         Soil_type = Substrate,
         Core = Core_ID)


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Latitude_Dec_Degree,
         Longitude = Longitude_Dec_Degree,
         OC_perc = OC_Perc, 
         BD_reported_g_cm3 = Dry_bulk_density_g_cm_3,
         Year_collected = Collection_Year) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  mutate(Core_depth_cm = gsub("-", "_", Core_depth_cm)) %>% #some core depths were separated by - instead of _
  separate(Core_depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Marsh_type, Soil_type, Country, Nation, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type,Marsh_type, Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Nation, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.5285/fa3f4087-528e-4c5d-90d8-6bb4675d6317")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


