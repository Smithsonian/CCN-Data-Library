## import data from Human et al 2022, Science of the Total Environment
## from Swartkops Estuary, South Africa
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 24.10.22

# Jaxine Wolfe revised 21.02.24
# made core IDs unique

library(tidyverse)
input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Human_2022_email/Swartkops_data.csv"

input_data01 <- read.csv(input_file01)


##### add informational 
source_name <- "Human et al 2022"
author_initials <- "LRDH"


input_data02 <- input_data01 %>% 
  mutate(Source =  gsub("\\(", "",
                        gsub("\\)", "", Source))) %>% 
  mutate(Site_name = paste(author_initials, "Swartkops", Subsite, Season, Core)) %>% 
  mutate(accuracy_code = 1,
         Core = case_when(Species == "Spartina maritima" ~ paste("Swartkops", Season, Subsite, Core, "Spartina", sep = "_"),
                          Species == "Salicornia tegetaria" ~ paste("Swartkops", Season, Subsite, Core, "Salicornia", sep = "_")),
         DOI = "https://doi.org/10.1016/j.scitotenv.2022.156955")



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(Source,  Site_name, Site, Subsite, Season, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Species,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, Conv_factor, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Subsite, Season, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)

