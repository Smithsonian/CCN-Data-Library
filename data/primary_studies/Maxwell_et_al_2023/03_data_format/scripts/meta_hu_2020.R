## import data from Hu et al 2020, SI from Environmental Research
# https://doi.org/10.1016/j.envres.2020.109576
## from wetlands across China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 19.08.22


library(tidyverse)


input_file01 <- "reports/03_data_format/data/meta_analysis/Hu_2020_SI/Hu_2020_SI.csv"

input_data01 <- read.csv(input_file01)

str(input_data01)

##### add informational  
source_name <- "Hu et al 2020"
author_initials <- "MH"

## renaming author names

input_data01$References <- as.factor(input_data01$References) 
levels(input_data01$References) <- gsub("., ", " ", levels((input_data01$References)))
levels(input_data01$References) <- gsub("aNA", "and", levels((input_data01$References)))




input_data02 <- input_data01 %>% 
  dplyr::rename(Site = Location,
    Original_source = References) %>% 
  mutate(ID = c(1:length(Site)),
         Source = source_name,
         Source_abbr = author_initials,
         Country = "China",
         Plot = paste(Country, ID), 
         Site_name = paste(Source_abbr, Plot),
         Habitat_type = "Coastal wetland")

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(OC_perc_mean = SOC_g_kg/10) %>% # g per kg to percent 
  mutate(Year_collected = Year,
         accuracy_flag = "direct from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = NA) #would need to check individual studies 


## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_cm = 0,
         L_depth_cm = 20) %>% #from methods
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Original_source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean)


export_data02 <- export_data01 %>% 
  relocate(Source, Original_source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.envres.2020.109576")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



