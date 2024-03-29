## import data from Perera et al 2022, Science of the Total Environment
## from Sri Lanka
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 09.09.22
#edit 20.12.22

library(tidyverse)

input_file01 <- "reports/03_data_format/data/site_level/Perera_2022_SI/Perera_2022_TableS2.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Perera et al 2022"
author_initials <- "NP"


input_data02 <- input_data01 %>% 
  dplyr::rename(Site = Site_name) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site),
         Habitat_type = "Salt marsh",
         Country = "Sri Lanka",
         Plot = Site) 



#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Year_collected = NA,
         Method = "LOI",
         Conv_factor = "0.47 from Craft 1991")

## edit depth and separate mean from se

input_data04 <- input_data03 %>% 
  mutate(Depth_cm = gsub("–", "-", Depth_cm)) %>% 
  separate(Depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%    #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100) %>% # cm to m
  separate(OC_perc, c("OC_perc_mean", "OC_perc_se"), sep = ' ± ') %>%    
  separate(DBD_g_cm3, c("BD_reported_g_cm3_mean", "BD_reported_g_cm3_se"), sep = ' ± ') %>%    
  separate(OC_dens_g_cm3, c("OC_dens_g_cm3_mean", "OC_dens_g_cm3_se"), sep = ' ± ')    
  



#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, Conv_factor,
                OC_perc_mean, OC_perc_se,BD_reported_g_cm3_mean,BD_reported_g_cm3_se )


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.scitotenv.2022.153313")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



