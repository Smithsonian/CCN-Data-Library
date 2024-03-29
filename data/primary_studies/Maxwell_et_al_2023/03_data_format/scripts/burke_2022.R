## import data from Burke et al 2022, Frontiers in Marine Biology
## from Dublin, Ireland
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 09.02.23

# Jaxine Wolfe checked 21.02.24

library(tidyverse)
input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Burke_2022_email/Burke_2022.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Burke et al 2022"
author_initials <- "SAB"


input_data02 <- input_data01 %>% 
  dplyr::rename(core_notunique = Core) %>% 
  mutate(Core = paste(Site, core_notunique),
         Site_name = paste(author_initials, Core),
         accuracy_code = "1") %>% 
  mutate(Method = "EA",
         DOI = "https://doi.org/10.3389/fmars.2022.976457")


#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, Conv_factor, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


