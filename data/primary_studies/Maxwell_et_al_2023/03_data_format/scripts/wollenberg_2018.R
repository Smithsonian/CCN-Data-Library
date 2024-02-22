## import data from Wollenberg et al 2018, PLOS ONE supplementary info 
## from Bay of Fundy, Canada
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 09.09.22



library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Wollenberg_2018_FigShare/Wollenberg_2018_TableS2.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Wollenberg et al 2018"
author_initials <- "JTW"


input_data02 <- input_data01 %>% 
  rename(Core = X) %>% 
  slice(2:29) %>%
  dplyr::select(1:11) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Salt marsh",
         Site = "Bay of Fundy, New Brunswick",
         Country = "Canada")


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(OC_perc = OC, 
         SOM_perc = avgerage.LOI,
         BD_reported_g_cm3 = Bulk.density) %>% 
  mutate(Year_collected = "2016", 
    accuracy_flag = "averaged from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = "LOI",
         Conv_factor = "0.40*x + (0.0025*x)2") #TYPO in paper



## edit depth

input_data04 <- input_data03 %>% 
  rename(U_depth_cm = Depth..cm., 
         L_depth_cm = X.1) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, Conv_factor, OC_perc, SOM_perc, BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1371/journal.pone.0193930")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



