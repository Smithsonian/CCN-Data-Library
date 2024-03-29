## import data from Bulmer et al 2020, 
## from Tairua estuary, NZ
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 08.12.22

# Jaxine Wolfe checked 21.02.24

library(tidyverse)
library(janitor) # for clean_names

input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Bulmer_2020_email/Bulmer et al saltmarsh C data.csv"

input_data01 <- read.csv(input_file01) %>% 
  slice(1:63)


##### add informational  
source_name <- "Bulmer et al 2020"
author_initials <- "RHB"


input_data02 <- input_data01 %>% 
  mutate_if(is.character, function(x) tolower(as.character(x))) %>% 
  clean_names(case = "none") %>% 
  rename(Site = SITE_TRANSECT,
         Core = Sediment_core_reference) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Country = "New Zealand") 


#### reformat data ####

# if you just want the % organic carbon value I would take the %TC (e.g. 2%) 
# then multiply by the %organics value in the cell e.g. 70%, which would take 
# it to 1.4%.  The total C and the organic/inorganic splits were determined using 
# an HCL acid digestion for the splitsband then running through the elemental analyser.


input_data03 <- input_data02 %>% 
  mutate(ratio_TC_to_OC = X_Organics/100,
         OC_perc = X_Total_C * ratio_TC_to_OC) %>% 
  rename(N_perc = X_Total_N,
         BD_reported_g_cm3 = Bulk_density_g_cm3) %>% 
  mutate(Date = lubridate::dmy(Date_sampled)) %>% 
  mutate(Year_collected = lubridate::year(Date), #separate Year, Month, Day
         month = lubridate::month(Date), 
         day = lubridate::day(Date)) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")

## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.3389/fmars.2020.00380")# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected, month, day,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, N_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>%   
  arrange(Site, Habitat_type)

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)
