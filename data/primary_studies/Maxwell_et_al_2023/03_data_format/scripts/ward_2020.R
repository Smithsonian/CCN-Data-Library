## import data from Ward 2020, Science of the Total Environment
## from Norwar
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 24.10.22
# edit 20.12.22

library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Ward_2020_email/Ward2020_data.csv"


input_data01 <- read.csv(input_file01) 


##### add informational 
source_name <- "Ward 2020"
author_initials <- "RDW"


input_data02 <- input_data01 %>% 
  mutate(Site_name = paste(author_initials, Site)) %>% 
  mutate(accuracy_flag = fct_recode(accuracy_flag, "direct from dataset" = "From GPS coords of site")) %>% 
  mutate(accuracy_code = 1) %>% 
  mutate(Core = Site)



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source,  Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, Conv_factor)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.scitotenv.2020.141343")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

