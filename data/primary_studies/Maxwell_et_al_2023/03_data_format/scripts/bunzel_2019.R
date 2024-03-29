## import data from Bunzel et al 2019, Newsletters on Stratigraphy
#https://doi.org/10.1127/nos/2020/0540
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.07.22
# edit 20.12.22

# Jaxine Wolfe checked 21.02.24
# Can't find original data

library(tidyverse)

#to check location points
library("ggmap")
library(maptools)
library(maps)


input_file01 <- "reports/03_data_format/data/core_level/Bunzel_2019_PANGEA/Bunzel-etal_Table_A1_edit.tab"

input_data01 <- read.table(input_file01, sep = '\t', header = T)

##### add informational  
source_name <- "Bunzel et al 2019"
author_initials <- "DB"


input_data02 <- input_data01 %>% 
  dplyr::rename(Site = Event.2,
         Core = Event) %>% 
  mutate(Site = fct_recode(Site, "Bay of Tumlau" = "Bay of Tümlau")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Salt marsh",
         Country = "Germany") 




#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = case_when(Core == "TB13-1" ~ "2013", #from paper, Bay of Tumlau August 2013
                                    TRUE ~ "2016")) %>% # all other sites Nov 2016
  rename(OC_perc = TOC....,
         U_depth_m = Depth.top..m.,
         L_depth_m = Depth.bot..m.) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA", 
         BD_reported_g_cm3 = NA,
         DOI = "https://doi.org/10.1594/PANGAEA.905218")


#### check location points ####


mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = input_data03, aes(x = Longitude, y = Latitude, 
                                               color = Site), alpha = 0.5)
mp



#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) 


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


