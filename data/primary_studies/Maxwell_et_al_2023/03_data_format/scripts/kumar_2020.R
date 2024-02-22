## import data from Kumar et al 2020, Continental Shelf Research
# https://doi.org/10.1016/j.csr.2020.104076 
## from Guadiana Estuary saltmarsh, SW Iberian Peninsula)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 29.07.22

# Jaxine Wolfe revised 21.02.24

library(tidyverse)
library(measurements) #to convert to decimal degrees

input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Kumar_2020_Table1/Kumar_2020_Table1.csv"

input_data01 <- read.csv(input_file01, na = "")


##### add informational  
source_name <- "Kumar et al 2020"
author_initials <- "MK"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Site = "Guadiana Estuary",
         Country = "Portugal") %>% 
  dplyr::rename(Plot = Core)



#### reformat data ####

input_data03 <- input_data02 %>% 
  dplyr::rename(lat_detail = Latitude,
         long_detail = Longitude) %>% 
  mutate(lat_detail = gsub(" N", "", lat_detail),
         long_detail = gsub(" W", "", long_detail)) %>% 
  mutate(lat_dec_deg = measurements::conv_unit(lat_detail, from = "deg_min_sec", to = "dec_deg"), #N , Keep positive
         long_dec_deg = measurements::conv_unit(long_detail, from = "deg_min_sec", to = "dec_deg"), #W , convert to neg)
         Latitude = as.numeric(lat_dec_deg),
         Longitude = as.numeric(long_dec_deg)*-1) %>% 
  dplyr::rename(OC_perc = C.... ) %>% 
  mutate(Year_collected = "2015", 
    accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1016/j.csr.2020.104076")# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Species, Carbonate_removed,
                U_depth_m, L_depth_m, Method, OC_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)


# export_data03 <- export_data02 %>% 
#   filter(Habitat_type == "Saltmarsh" )

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)



