## import data from Gailis et al 2021, Coastal Engineering
## from Boundary Bay, BC, Canada
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 17.08.22


library(tidyverse)


input_file01 <- "reports/03_data_format/data/site_level/Gailis_2021_Table1/Gailis_2021_Table1.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _


input_file02 <- "reports/03_data_format/data/site_level/Gailis_2021_Table1/Gailis_2021_TableS1.csv"

locations0 <- read.csv(input_file02)

locations1 <- locations0 %>%  
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%   #replacing . in columns by _
  dplyr::select(Core_ID, Latitude, Longitude, Date_CollectedSummer_, Depth_of_Refusal)


## joining datasets

input_data02 <- full_join(input_data01, locations1, by = "Core_ID")


##### add informational  
source_name <- "Gailis et al 2021"
author_initials <- "MG"


input_data03 <- input_data02 %>% 
  rename(Year_collected = Date_CollectedSummer_,
         Plot = Core_ID) %>% 
  mutate(Site = "Boundary Bay, BC", 
         Habitat_type = "Salt marsh",
         Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot),
         Country = "Canada")


#### reformat data ####

input_data04 <- input_data03 %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "LOI",
         Conv_factor = "0.44x -1.33") %>% 
  mutate(Depth_to_bedrock_m = as.numeric(Depth_of_Refusal)/100, #cm to m
         U_depth_cm = 0,
         L_depth_cm = 10,
         U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



input_data05 <- input_data04 %>% 
  separate(Average_DBD_g_cm3__SD, c("BD_g_cm3_mean", "BD_g_cm3_SD"), sep = '±') %>%  
  separate(Average_C_SD, c("OC_perc_mean", "OC_perc_SD"), sep = '±') 
  


#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Depth_to_bedrock_m, Method, Conv_factor,
                OC_perc_mean, OC_perc_SD,
                BD_g_cm3_mean, BD_g_cm3_SD)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1080/21664250.2021.1894815")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



