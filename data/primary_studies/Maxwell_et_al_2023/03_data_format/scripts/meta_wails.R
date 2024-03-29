## import data from Wails et al 2021, supplementary info
# https://link.springer.com/article/10.1007/s10530-021-02540-5
## meta-analysis, effect of invasion by Spartina alterniflora and Phragmites australis 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 26.07.22
# edit 20.12.22

library(tidyverse)


input_file01 <- "reports/03_data_format/data/meta_analysis/Wails_2021_SI/Wails_2021_toextract.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Wails et al 2021"
author_initials <- "CNW"

input_data02 <- input_data01 %>% 
  rename(Habitat_type = Habitat) %>% 
  mutate(Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site))


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = Year,
         accuracy_flag = "direct from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = "LOI") %>% 
  separate(SoilDepth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m



input_data04 <- input_data03 %>% 
  dplyr::select(Source, Original_source, Site_name, Site, Habitat_type, Country, Year_collected,
                Month,
                Latitude, Longitude, accuracy_flag, accuracy_code, 
                Method, U_depth_m, L_depth_m,
                Invaded_measure, Invaded_SD, Invaded_Sample.size, 
                Uninvaded_measure, Uninvaded_SD, Uninvaded_Sample.size)


### pivot table to extract relevant data 
input_data05 <- input_data04 %>% 
  pivot_longer(cols = c("Invaded_measure":"Uninvaded_Sample.size")) %>% 
  mutate(Treatment = case_when(name == "Invaded_measure"|
                                 name == "Invaded_SD" |
                                 name == "Invaded_Sample.size" ~ "Invaded",
                               name == "Uninvaded_measure"|
                                 name == "Uninvaded_SD" |
                                 name == "Uninvaded_Sample.size" ~ "Uninvaded")) %>% 
  mutate(name = fct_recode(name, "n" = "Invaded_Sample.size", 
                           "SOM_perc_mean" = "Invaded_measure", "SOM_perc_SD" = "Invaded_SD",
                           "n" = "Uninvaded_Sample.size", 
                           "SOM_perc_mean" = "Uninvaded_measure", "SOM_perc_SD" = "Uninvaded_SD"))


input_data06 <- input_data05 %>% 
  pivot_wider(names_from = c("name"),
              values_from = "value")

## mean of series of cores
input_data07 <- input_data06 %>% 
  group_by(Source, Original_source, Site_name, Site, Habitat_type, Country, Year_collected,
           Latitude, Longitude, accuracy_flag, accuracy_code,Method, U_depth_m, L_depth_m, Treatment) %>% 
  summarise_at(vars(SOM_perc_mean, SOM_perc_SD), funs(mean)) %>% 
  ungroup() %>% 
  mutate(Plot = paste(Site, Treatment))



#### export ####

export_data01 <- input_data07 %>% 
  dplyr::select(Source, Original_source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Treatment,
                Method, U_depth_m, L_depth_m,SOM_perc_mean, SOM_perc_SD)


export_data02 <- export_data01 %>% 
  relocate(Source, Original_source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
           Latitude, Longitude, accuracy_flag, accuracy_code, Treatment,
           Method, .before = U_depth_m) %>% 
  mutate(DOI = "https://doi.org/10.1007/s10530-021-02540-5")


###removing treatment values

# export_data03 <- export_data02 %>% 
#   filter(Treatment == "Invaded")



## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)







