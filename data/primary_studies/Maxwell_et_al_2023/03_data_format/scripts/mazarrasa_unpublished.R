## import data from Mazarrasa unpublished
#Mondego estuary, Oyambre estuary, Santoña marshes, Santander Bay, the Western Scheldt
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 08.11.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Mazarrasa_unpublished_email/Mazarrasa_unpublished.csv"


input_data01 <- read.csv(input_file01) 


##### add informational 
source_name <- "Mazarrasa et al in prep"
author_initials <- "IM"


input_data02 <- input_data01 %>% 
  mutate(Source = gsub("\\.,", "",Source)) %>% 
  mutate(Site_name = paste(author_initials, Core)) %>% 
  mutate(accuracy_code = 1) %>% 
  mutate(Latitude = gsub("°", "", Latitude),
         Longitude = gsub("°", " ",Longitude)) %>% 
  mutate(across(where(is.character), str_trim)) %>%  # trim white spaces before and after character strings
  mutate(Method = "EA", 
         DOI = "https://doi.org/10.1016/j.scitotenv.2023.163957")



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source,  Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

