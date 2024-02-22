## import data from Vitti et al 2020, Journal of Plant Ecology
## sent via email
## from northern Adriatic (Italy)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 15.08.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Vitti_2020_email/Vitti_et_al_2020.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Vitti et al 2020"
author_initials <- "SV"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, ID_CODE),
         Site = "Marano and Grado Lagoon",
         Country = "Italy") %>% 
  rename(Habitat_type = Habitat,
    Plot = ID_CODE)


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Y,
         Longitude = X,
         OC_perc = Soil.organic.Carbon,
         N_perc = Soil.Nitrogen) %>% 
  mutate(Year_collected = "2018",
    accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")



## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = 12) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, N_perc)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1093/jpe/rtaa052")


export_data03 <- export_data02 %>% 
  filter(Habitat_type == "Salt marsh")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data03

write.csv(export_df, export_file)

