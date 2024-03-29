## import data from Voltz et al 2021
## from  Canche and Authie estuaries, France
## export for marsh soil C
## site-level (mean and sd)
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.12.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/site_level/Voltz_2021_email/Global_tidalmarshC_data_template_SGontharet.csv"

input_data01 <- read.csv(input_file01)

## add informational data, reformat columns
source_name <- "Voltz et al 2021"
author_initials <- "BV"


input_data02 <- input_data01 %>% 
  mutate(Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         accuracy_code = "1") %>% 
  mutate(Habitat_type = case_when(Habitat_type_intermed == "Mud flat" ~ "Mud flat",
                             Habitat_type_intermed == "Sand flat" | 
                               Habitat_type_intermed == "Sand bank" ~ "Sand flat",
                             Habitat_type_intermed == "High salt" |
                               Habitat_type_intermed == "Low salt" ~ "Salt marsh"))

## filter for salt marsh only 

input_data03 <- input_data02 %>% 
  filter(Habitat_type == "Salt marsh")



#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean, OC_perc_sd, SOM_perc_mean,SOM_perc_sd)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1016/j.csr.2021.104554")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


