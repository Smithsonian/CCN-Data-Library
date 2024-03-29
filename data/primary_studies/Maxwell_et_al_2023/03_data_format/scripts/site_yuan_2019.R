## import data from Yuan et al 2019, Journal of Ecology
## from China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 09.09.22
# edit 20.12.22


library(tidyverse)

input_file01 <- "reports/03_data_format/data/site_level/Yuan_2019_DRYAD/Yuan_2019_DRYAD.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Yuan et al 2019"
author_initials <- "JY"


input_data02 <- input_data01 %>% 
  slice(-c(1,17:22)) %>% 
  dplyr::rename(Plot = Site) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot),
         Habitat_type = "Salt marsh",
         Country = "China",
         Site = "Yancheng National Wetland Reserve") 

input_data02$SOC..g.C.kg.1. <- as.numeric(input_data02$SOC..g.C.kg.1.)
input_data02$X <- as.numeric(input_data02$X)


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Longitude = case_when(Plot == "SA-1" ~ "120.6136681950972", 
                               Plot == "SA-12" ~ "120.6136681950972", 
                               Plot == "TF" ~ "120.6142708364346"),
    Latitude = case_when(Plot == "SA-1" ~ "33.60435983648976", 
                         Plot == "SA-12" ~ "33.60435983648976", 
                         Plot == "TF" ~ "33.60457407365762")) %>% 
  mutate(OC_perc_mean = SOC..g.C.kg.1./10, #convert from g per kg to %
           OC_perc_se = X/10) %>%  #convert from g per kg to %
  mutate(accuracy_flag = "estimated from GEE",
         accuracy_code = "2") %>% 
  mutate(Year_collected = "2011",
         Method = "wet oxidation redox titration method")

## edit depth
input_data04 <- input_data03 %>% 
  separate(Layer..cm., c("U_depth_cm", "L_depth_cm"), sep = '-') %>%    #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)

#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, 
                OC_perc_mean, OC_perc_se)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.5061/dryad.6f60v3q")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



