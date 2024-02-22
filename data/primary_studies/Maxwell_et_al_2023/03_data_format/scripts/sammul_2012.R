## import data from Sammul et al 2012, Applied Vegetation Science
## from Estonian coastland grasslands
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 11.02.22
# edit 20.12.22
# edit 15.05.23 added soil type from notes


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Sammul_2012_email/Sammul_Estonia.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Sammul et al 2012"
author_initials <- "MS"


input_data02 <- input_data01 %>% 
  rename(Treatment = kasutus,
         Soil_type = notes) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Plot = paste(Site, Treatment),
         Site_name = paste(Source_abbr, Plot),
         Habitat_type = "Salt marsh",
         Country = "Estonia") 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = 2005) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "Tyurin spectrophotometry",
         Conv_factor  = NA)


## edit depth

input_data04 <- input_data03 %>% 
  separate(Soil_depth, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100, 
         DOI = "https://doi.org/10.1111/j.1654-109X.2011.01167.x") 

#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, Conv_factor, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

