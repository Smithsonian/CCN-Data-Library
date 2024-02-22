## import data from Ford et al 2016, UK CEH dataset
## from Morecambe Bay and Essex
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.07.22

# Jaxine Wolfe checked 21.02.24
# make core IDs unique

library(tidyverse)

input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Ford_2016_UKCEH/CBESS_Soil_organic_matter.csv"

input_data01 <- read.csv(input_file01)


#### PIVOT LONGER #####

input_data02 <- input_data01 %>% 
  select(-Row.Number) %>% 
  pivot_longer(cols = OM_0.10:OM_20.30) %>% 
  rename(layer_cm = name,
         SOM_perc = value)


##### add informational  
source_name <- "Ford et al 2016"
author_initials <- "HF"


input_data03 <- input_data02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Quadrat.No),
         Country = "UK") %>% 
  rename(Habitat_type = Habitat,
         Core = Quadrat.No) %>% 
  mutate(Season = fct_recode(Season, "Winter" = "W", "Summer" = "S"),
         Location = fct_recode(Location, "Essex" = "E", "Morecambe" = "M"),
         Habitat_type = fct_recode(Habitat_type, "Saltmarsh" = "SM"))

#### reformat data ####

input_data04 <- input_data03 %>% 
  mutate(lat = case_when(Site == "AH" ~ "51 47 00",
                         Site == "FW"  ~ "51 49 00",
                         Site == "TM"  ~ "51 41 00",
                         Site == "CS"  ~ "54 10 00",
                         Site == "WS"  ~ "54 8 00",
                         Site == "WP"  ~ "54 9 00"),
         long = case_when(Site == "AH" ~ "00 52 00", #east
                          Site == "FW"  ~ "00 58 00", #east
                          Site == "TM"  ~ "00 56 00", #east
                          Site == "CS"  ~ "03 00 00", #west, convert to negative
                          Site == "WS"  ~ "02 48 00", #west
                          Site == "WP"  ~ "02 58 00")) %>% #west
  # mutate(lat = as.factor(lat),
  #        long = as.factor(long)) %>% 
  mutate(lat_dec_deg = measurements::conv_unit(lat, from = "deg_min_sec", to = "dec_deg"), #N , Keep positive
         long_dec_deg = measurements::conv_unit(long, from = "deg_min_sec", to = "dec_deg"), #need to convert morecambe bay sites
         Latitude = as.numeric(lat_dec_deg),
         Longitude = case_when(Location == "Essex" ~ as.numeric(long_dec_deg), #east, keep positive
           Location == "Morecambe" ~ as.numeric(long_dec_deg)*-1)) %>%   #west, convert to neg
  mutate(BD_reported_g_cm3 = "measured but not in dataset",
         Year_collected = "2013") %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = "LOI")


## edit depth

input_data05 <- input_data04 %>% 
  mutate(U_depth_cm = case_when(layer_cm == "OM_0.10" ~ 0,
                                layer_cm == "OM_10.20" ~ 10,
                                layer_cm == "OM_20.30" ~ 20),
         L_depth_cm = case_when(layer_cm == "OM_0.10" ~ 10,
                                layer_cm == "OM_10.20" ~ 20,
                                layer_cm == "OM_20.30" ~ 30)) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         Core = paste(Site, Location, Season, Core, sep = "_"),
         DOI = "https://doi.org/10.5285/90457ba1-f291-4158-82dc-425d7cbb1ac5")# cm to m



#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected, Season,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, Season, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)



