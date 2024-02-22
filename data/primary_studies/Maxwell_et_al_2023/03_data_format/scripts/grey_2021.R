## import data from Grey et al 2021, Regional Studies in Marine Science
#https://doi.org/10.1016/j.rsma.2021.101834
## from Bull Island saltmarsh, Ireland
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 25.07.22
# edit 15.05.23 to include Soil_type column


#note: this dataset also includes mudflats and sand samples.
#locations and data retrieved only for salt marshes

library(tidyverse)

input_file01 <- "reports/03_data_format/data/core_level/Grey_2021_SI/Grey_2021_SI_tabula.csv"

input_data01 <- read.csv(input_file01)

#import locations from file derived from Google Earth
input_file02 <- "reports/03_data_format/data/core_level/Grey_2021_SI/Grey_et_al_2021.csv"

locations1 <- read.csv(input_file02)

locations2 <- locations1 %>% 
  rename(Sample = Name) %>% 
  filter(Sample != "28") # data not located in supplementary table


input_data02  <- full_join(input_data01, locations2, by = "Sample")

input_data02 <- input_data02 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _



##### add informational  
source_name <- "Grey et al 2021"
author_initials <- "AG"


input_data03 <- input_data02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Bull Island",
         Site_name = paste(Source_abbr, Site, Sample),
         Country = "Ireland") %>% 
  rename(Plot = Sample,
         Habitat_type = Zone,
         Soil_type = Sample_appearance)



#### reformat data ####

input_data04 <- input_data03 %>% 
  rename(Latitude = Y,
         Longitude = X,
         OC_perc = X_TOC,
         N_perc = X_TN,
         SOM_perc = X_OM) %>% 
  mutate(Year_collected = "2016",
         BD_reported_g_cm3 = NA) %>% 
  mutate(accuracy_flag = "estimated from GE",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data05 <- input_data04 %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = 10) %>% #from methods section
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1016/j.rsma.2021.101834")# cm to m




#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, N_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



