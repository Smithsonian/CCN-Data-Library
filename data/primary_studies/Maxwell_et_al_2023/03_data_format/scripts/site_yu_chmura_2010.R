## import data from Yu and Chmura et al 2010, Environmental Conservation
## from St Lawrence Estuary tidal marsh
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 17.08.22
# edit 20.12.22


library(tidyverse)


input_file01 <- "reports/03_data_format/data/site_level/Yu_Chmura_2010_Table1/Yu_Chmura_2010_Table1.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Yu and Chmura 2010"
author_initials <- "OTY"


input_data02 <- input_data01 %>% 
  rename(Treatment = X) %>% 
  mutate(Site = "Ile Verte", 
         Habitat_type = "Tidal marsh",
         Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Treatment),
         Country = "Canada",
         Plot = Site) 


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = "2007",
         accuracy_flag = "estimated from GEE",
         accuracy_code = "2") %>% 
  mutate(Method = "EA") %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = 10,
         U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_SD,
                BD_g_cm3_mean, BD_g_cm3_SD)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1017/S0376892910000184")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

