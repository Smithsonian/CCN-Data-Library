## import data from Adame et al 2013, Plos ONE
## from SianKaan, Mexico 2011
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 17.10.22

# Jaxine Wolfe check 21.02.24

library(tidyverse)

datapath <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/"
input_file01 <- paste0(datapath, "core_level/Adame_2013_email/Adame_2013_SianKaan.csv")

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Adame et al 2013"
author_initials <- "MFA"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot),
         Habitat_type = "Salt marsh",
         Country = "Mexico") 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "combined LOI EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   # separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100, #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100, # cm to m
         month = 9, 
         day = 12,
         DOI = "https://doi.org/10.1371/journal.pone.0056569")

#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected, month, day, 
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

# path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(datapath, "exported/", source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)




