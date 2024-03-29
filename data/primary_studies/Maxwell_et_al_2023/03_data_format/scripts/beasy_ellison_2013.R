## import data from Beasy & Ellison 2013, 
## from Rubicon river estuary, Tasmania,Australia
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 18.01.23
# 09.05.23 edit with locations sent by Kim Beasy

# Jaxine Wolfe checked 21.02.24

library(tidyverse)

datapath <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/"

input_file01 <- paste0(datapath, "core_level/Beasy_Ellison_2013/Beasy_Ellison_2013.csv")
# input_file01 <- "reports/03_data_format/data/core_level/Beasy_Ellison_2013/Beasy_Ellison_2013.csv"

input_data01 <- read.csv(input_file01) %>% 
  dplyr::select(-c(Latitude, Longitude))

input_file02 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Beasy_Ellison_2013/Beasy_locations.csv"

locations <- read.csv(input_file02)

##### add informational  
source_name <- "Beasy and Ellison 2013"
author_initials <- "KMB"


input_data02 <- input_data01 %>% 
  dplyr::rename(Core = Site) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, "Tasmania", Core),
         Habitat_type = "Salt marsh",
         Site = "Rubicon Estuary",
         Nation = "Tasmania",
         Country = "Australia")

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = "2011",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") 


## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

## add location
input_data05 <- full_join(input_data04, locations, by = "Core") %>% 
  mutate(DOI = "https://doi.org/10.3390/biology7020027")
  
#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Nation, Year_collected,
                Latitude, Longitude, 
                accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, SOM_perc_Heiri, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, 
           Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Nation, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


#visualize 
plot(export_df$SOM_perc, export_df$OC_perc)
abline(a = c(0,1))
plot(export_df$SOM_perc_Heiri, export_df$OC_perc)
abline(a = c(0,1))
