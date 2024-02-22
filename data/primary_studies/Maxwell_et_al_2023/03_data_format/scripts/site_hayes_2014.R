## import site-level data from mixed riverine and non-riverine setting Hayes et al 2014, GCB
## from Moreton Bay, Australia
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 16.01.23


library(tidyverse)
input_file01 <- "reports/03_data_format/data/site_level/Hayes_2014_SI/Hayes_2014_SI.csv"

input_data01 <- read.csv(input_file01) %>% 
  slice(1:5) %>% 
  dplyr::select(1:8)


##### add informational  
source_name <- "Hayes et al 2014"
author_initials <- "MAH"


input_data02 <- input_data01 %>% 
  dplyr::rename(Habitat_type = Species,
                Plot = Setting,
                BD_reported_g_cm3_mean = BD_g_cm3_mean,
                BD_reported_g_cm3_se = BD_g_cm3_se) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Moreton Bay",
         Site_name = paste(Source_abbr, Site),
         Country = "Australia") 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Latitude = -27.744882,
         Longitude = 153.386378,
         Year_collected = "2012",
         Year_collected_end = "2013") %>% 
  mutate(accuracy_flag = "estimated from GEE",
         accuracy_code = "3") %>% 
  mutate(Method = "MIR absorbance spectra")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(L_depth_cm = gsub("\\'", "", L_depth_cm)) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Year_collected_end, 
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, n, OC_perc_mean, OC_perc_se,
                BD_reported_g_cm3_mean, BD_reported_g_cm3_se)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country,Year_collected, Year_collected_end, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1111/gcb.13722")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

