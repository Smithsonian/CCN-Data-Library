## import data from Ferronato et al 2019
## from Baiona Lagoon, Italy, 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 24.03.23



library(tidyverse)


input_file01 <- "reports/03_data_format/data/site_level/Ferronato_2019_email/Ferronato_2019_toedit.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Ferronato et al 2019"
author_initials <- "CF"


input_data02 <- input_data01 %>% 
  dplyr::rename(Plot = site) %>% 
  mutate(Habitat_type = case_when(ecosystem == "TES"~ "Terrestrial tidal marsh",
                                   ecosystem == "ITS"~ "Intertidal tidal marsh",
                                   ecosystem == "SAS"~ "Subaqueous tidal marsh")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Baiona lagoon",
         Site_name = paste(Source_abbr, Plot),
         Country = "Italy")


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = "2015",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "CF-IRMS") %>% 
  mutate(TOC_g_kg_se = gsub("na", NA, TOC_g_kg_se)) %>% 
  mutate(OC_perc_mean = as.numeric(TOC_g_kg_mean)/10, 
         OC_perc_se = as.numeric(TOC_g_kg_se)/10)
  
  
#### format depths #### 

input_data04 <- input_data03 %>%  
  mutate(U_depth_cm = case_when(ecosystem == "TES" & horizon == "A" ~ "0",
                                ecosystem == "ITS" & horizon == "A" ~ "0",
                                ecosystem == "SAS" & horizon == "A" ~ "0",
                                ecosystem == "TES" & horizon == "AC" ~ "11",
                                ecosystem == "ITS" & horizon == "AC" ~ "6",
                                ecosystem == "SAS" & horizon == "AC" ~ "5",
                                ecosystem == "TES" & horizon == "C" ~ "28",
                                ecosystem == "ITS" & horizon == "C" ~ "24",
                                ecosystem == "SAS" & horizon == "C" ~ "80"),
         L_depth_cm = case_when(ecosystem == "TES" & horizon == "A" ~ "11",
                                ecosystem == "ITS" & horizon == "A" ~ "6",
                                ecosystem == "SAS" & horizon == "A" ~ "5",
                                ecosystem == "TES" & horizon == "AC" ~ "28",
                                ecosystem == "ITS" & horizon == "AC" ~ "24",
                                ecosystem == "SAS" & horizon == "AC" ~ "80",
                                ecosystem == "TES" & horizon == "C" ~ "100",
                                ecosystem == "ITS" & horizon == "C" ~ "100",
                                ecosystem == "SAS" & horizon == "C" ~ "118")) %>%
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_se)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.geoderma.2018.12.019")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

