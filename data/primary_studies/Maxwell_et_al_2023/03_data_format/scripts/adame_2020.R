## import data from Adame et al 2020, Ecosystems
## from Insulator Creek, Australia 2011
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 19.10.22

# Jaxine Wolfe check 21.02.24

library(tidyverse)

datapath <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/"
input_file01 <- paste0(datapath, "core_level/Adame_2020_email/Adame_2020_IC.csv")

# input_file01 <- "reports/03_data_format/data/core_level/Adame_2020_email/Adame_2020_IC.csv"

input_data01 <- read.csv(input_file01)

input_file02 <- paste0(datapath, "core_level/Adame_2020_email/locations.csv")

locations <- read.csv(input_file02) %>% 
  rename(Plot = Name) %>% 
  dplyr::select(X,Y, Plot)


##merge files

input_data02 <- left_join(input_data01, locations, by = "Plot") %>% 
  rename(Latitude = Y, Longitude = X)


##### add informational  
source_name <- "Adame et al 2020"
author_initials <- "MFA"


input_data03 <- input_data02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Insulator Creek",
         Site_abbr = "IC",
         Site_name = paste(Source_abbr, Site_abbr, Plot),
         Habitat_type = "Salt marsh",
         Country = "Australia") 

#### reformat data ####

input_data04 <- input_data03 %>% 
  mutate(Year_collected = 2016,
         accuracy_flag = "direct from dataset and estimated",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data05 <- input_data04 %>% 
  separate(Depth, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1007/s10021-019-00414-5")# cm to m



#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)
