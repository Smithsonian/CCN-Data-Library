## import data from Raw et al 2020, African Journal of Aquatic Science
## from Knysna Estuary, South Africa
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 24.10.22


library(tidyverse)
input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Raw_2020_email/Knysna_data.csv"


input_data01 <- read.csv(input_file01) %>% 
  slice(1:63) ##removing NAs at the bottom


##### add informational 
source_name <- "Raw et al 2020"
author_initials <- "JLR"


input_data02 <- input_data01 %>% 
  mutate(Source =  gsub("\\(", "",
                        gsub("\\)", "", Source)),
         Latitude = gsub("°", "", Latitude),
         Longitude = gsub("°", "", Longitude)) %>% 
  mutate(Site_name = paste(author_initials, "Knysna", Subsite, Core)) %>% 
  mutate(accuracy_code = 1,
         Core = paste("Knysna", Subsite, Core, sep = "_"),
         DOI = "https://doi.org/10.2989/16085914.2019.1662763")
  # add_count(Site, Subsite, Core, U_depth_m, L_depth_m) %>% filter(n > 1)



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source,  Site_name, Site, Subsite, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Species,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, Conv_factor, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Subsite, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)

