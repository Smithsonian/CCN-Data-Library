## import data from Santos 2019, Scientific Reports (Portugal)
# Martins et al 2019, Ecosystems (Portugal)
# de los Santos et al 2022 Estuarine, Coastal and Shelf Science (Portugal)
# de los Santos et al 2022, Ecosystems (Spain)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 08.11.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Santos_2019_2021_2022ab_email/Santos_alldata.csv"


input_data01 <- read.csv(input_file01) 


##### add informational 
source_name <- "Santos2019 delosSantos2022ab Martins2022"
author_initials <- "RS"


input_data02 <- input_data01 %>% 
  mutate(Source = gsub("\\.", "",Source)) %>% 
  mutate(author_initials = case_when(Source == "Santos et al 2019" ~ "RS",
                                     Source == "Martins et al 2022" ~ "MM",
                                     Source == "de los Santos et al 2022 (a)" ~ "CBdlS",
                                     Source == "de los Santos et al 2022 (b)" ~ "CBdlS")) %>% 
  mutate(DOI = case_when(Source == "Santos et al 2019" ~ "https://doi.org/10.1038/s41598-018-37031-6",
                         Source == "Martins et al 2022" ~ "https://doi.org/10.1007/s10021-021-00660-6",
                         Source == "de los Santos et al 2022 (a)" ~ "https://doi.org/10.1016/j.ecss.2022.107896",
                         Source == "de los Santos et al 2022 (b)" ~ "https://doi.org/10.1007/s10021-022-00801-5"  )) %>% 
  mutate(Site_name = paste(author_initials, Core)) %>% 
  mutate(accuracy_code = 1)

##add organic carbon values from SOM

input_data03 <- input_data02 %>% 
  #adding SOM converted carbon values to the OC_perc column
  mutate(OC_perc = case_when(Method == "LOI" & Source == "de los Santos et al 2022 (a)" & is.na(OC_perc) == TRUE
                                   ~ -0.066 + (0.3102*SOM_perc),
                                  Method == "LOI" & Source == "Martins et al 2022" & is.na(OC_perc) == TRUE
                                    ~ -0.066 + (0.3102*SOM_perc),
                                  Method == "LOI" & Source == "de los Santos et al 2022 (b)" & is.na(OC_perc) == TRUE
                                    ~ 0.461*SOM_perc -0.266,
                             TRUE ~ OC_perc)) %>% #keeping raw OC values if they are provided
  mutate(OC_perc = replace(OC_perc, which(OC_perc<0), NA)) # replacing negative OC_perc values with NA



#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source,  Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, Conv_factor, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



