## import data from Yuan et al 2017, Fig Share
#https://doi.org/10.1016/j.jmarsys.2017.06.001
## from Andong salt marsh, China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.07.22

library(tidyverse)

input_file01 <- "reports/03_data_format/data/core_level/Yuan_2017_FigShare/Yuan_2017_reduced-data.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))  #replacing . in columns by _
  


##### add informational  
source_name <- "Yuan et al 2017"
author_initials <- "HWY"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Andong",
         Site_name = paste(Source_abbr, Site, Location),
         Habitat_type = "Salt marsh",
         Country = "China") %>% 
  rename(Plot = Location)


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Latitude = gsub("°", "",Latitude_N), # keep positive
         Longitude = gsub("°", "",Longitude_E), #keep positive
         OC_perc = TOC__, 
         BD_reported_g_cm3 = NA,
         TN_perc = TN__) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, TN_perc, BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.jmarsys.2017.06.001")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



