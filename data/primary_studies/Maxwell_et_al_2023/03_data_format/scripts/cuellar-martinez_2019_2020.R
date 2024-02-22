## import data from Ana Carolina Ruiz Fernandes 
## from Mexico
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 01.12.2022


library(tidyverse)
library(janitor)
input_file01 <- "reports/03_data_format/data/core_level/Ruiz_Fernandez_email/20221129 SALTMARSH EL MECHAL MEXICO.csv"

el_mechal01 <- read.csv(input_file01) %>% 
  clean_names()

input_file02 <- "reports/03_data_format/data/core_level/Ruiz_Fernandez_email/20221129 SALTMARSH SAN QUINTIN MEXICO.csv"

san_quintin01 <- read.csv(input_file02) %>% 
  dplyr::select(1:43) %>% 
  clean_names()
 
## combine the datasets
input_data01 <- rbind(el_mechal01, san_quintin01)

##### add informational  
author_initials <- "TCM"


input_data02 <- input_data01 %>% 
  mutate(Source = case_when(study_area == "Ensenada" ~ "Cuellar-Martinez et al 2019",
                            study_area == "Laguna de Terminos" ~ "Cuellar-Martinez et al 2020")) %>% 
  mutate(Source_abbr = author_initials,
         Site_name = paste(Source_abbr, core_code),
         Country = "Mexico") %>% 
  rename(Core = core_code,
         Habitat_type = environment,
         Site = study_area)


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Latitude = latitude_n,
         Longitude = longitude_w*-1) %>%  # negative because west
  rename(OC_perc = corg,
         N_perc = ntot,
         SOM_perc = loi550) %>% 
  mutate(Year_collected = "2018",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  rename(U_depth_cm = layer_top,
         L_depth_cm = layer_bot) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100) %>% # cm to m
  mutate(DOI = case_when(Source == "Cuellar-Martinez et al 2019" ~ "https://doi.org/10.1016/j.scitotenv.2019.03.388",
                         Source == "Cuellar-Martinez et al 2020" ~ "https://doi.org/10.1016/j.gloplacha.2020.103215"))



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, N_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export
source_name <- "Cuellar-Martinez et al 2019 2020"

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


