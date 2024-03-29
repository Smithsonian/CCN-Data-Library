## import data from Cott et al 2013, Estuaries and Coasts
## from Ireland
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.01.23


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Cott_2013_email/GCott_data.csv"


input_data01 <- read.csv(input_file01) 

##### add informational 
source_name <- "Cott et al 2013"
author_initials <- "GC"


input_data02 <- input_data01 %>% 
  dplyr::select(-Source) %>% 
  dplyr::rename(Core_simple = Core,
                Location = Location.on.site) %>% 
  mutate(Core = paste(Site, Core_simple)) %>% 
  mutate(Source = source_name, 
    Site_name = paste(author_initials, Core, Location)) %>% 
  mutate(accuracy_code = 1,
         DOI = "https://doi.org/10.1007/s12237-012-9579-7")



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source,  Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, SOM_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)


# export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '')
export_df <- export_data02

write.csv(export_df, export_file)





