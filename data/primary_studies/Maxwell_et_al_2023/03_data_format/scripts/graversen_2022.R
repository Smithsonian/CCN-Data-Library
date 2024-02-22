## import data from Graversen et al 2022, emailed
## from Denmark 2018
# grazed vs ungrazed 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 09.09.22
# edit 20.12.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Graversen_2022_email/Global_tidalmarshC_data_DK.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Graversen et al 2022"
author_initials <- "EG"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Management),
         Country = "Denmark")




#### reformat data ####


#note: 1 core per site and management
# for some, the layers were pooled, and others, three cores were separately measured
# to have 1 value per core, taking the mean of the individual cores at the layers were they were separately measured
# most layers were pooled

input_data03 <- input_data02 %>% 
  group_by(Site, Management, U_depth_m..compressed., L_depth_m..compressed.) %>% 
  summarise_at(vars(OC_perc:SOM_perc), funs(mean)) %>% 
  ungroup()


input_data_info <- input_data02 %>% 
  group_by(Source, Site, Site_name, Management, Marshzone, Habitat_type, Latitude, Longitude,
           accuracy_flag.see.note.in.metadata, Country, Year_collected, 
           U_depth_m..compressed., L_depth_m..compressed., Method) %>% 
  distinct(U_depth_m..compressed.)


input_data04 <- left_join(input_data03, input_data_info, 
                          by = c("Site", "Management", "U_depth_m..compressed.",
                                 "L_depth_m..compressed."))  


## applying the lat and long to the whole core

input_data05 <- input_data04 %>%
  group_by(Site, Management) %>% 
  fill(Latitude, Longitude, .direction = "down") 


input_data06 <- input_data05 %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA") %>% 
  rename(Treatment = Management,
         U_depth_cm = U_depth_m..compressed.,
         L_depth_cm = L_depth_m..compressed.) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100) %>% # cm to m 
  mutate(Core = paste(Site, Treatment),
         DOI = "https://doi.org/10.1002/lno.12011")

#### export ####

export_data01 <- input_data06 %>% 
  dplyr::select(Source, Site_name, Site, Core, Treatment, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, SOM_perc, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Treatment, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



