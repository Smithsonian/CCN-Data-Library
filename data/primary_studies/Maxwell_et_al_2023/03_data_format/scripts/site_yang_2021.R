## writing data from Yang et al 2021, LDD 
## Jiangsu Yancheng Wetland Natural Reserve, China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 16.09.22
# edit 20.12.22

library(tidyverse)
##from supplementary table S1
#https://onlinelibrary.wiley.com/doi/full/10.1002/ldr.3859 

OC_perc_mean <- 7.37/10 # to turn from per mille to percent
OC_perc_sd <- 1.38/10

BD_reported_g_cm3_mean <- 1.25
BD_reported_g_cm3_sd  <- 0.11

input_data01 <- as.data.frame(cbind(OC_perc_mean, OC_perc_sd,
                                    BD_reported_g_cm3_mean, BD_reported_g_cm3_sd))

##### add informational  
source_name <- "Yang et al 2021"
author_initials <- "RMY"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Jiangsu Yancheng Wetland",
         Site_name = paste(Source_abbr, Site),
         Habitat_type = "Saltmarsh",
         Country = "China",
         Plot = Site) 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Longitude = 120.694884,
         Latitude = 33.435201,
         accuracy_flag = "estimated from GEE",
         accuracy_code = "2",
         Method = "Walkley–Black wet combustion",
         Year_collected = "2017",
         U_depth_cm = 0,
         L_depth_cm = 30)%>%   
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_sd,
                BD_reported_g_cm3_mean, BD_reported_g_cm3_sd) %>% 
  mutate(DOI = "https://doi.org/10.1002/ldr.3859")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)
