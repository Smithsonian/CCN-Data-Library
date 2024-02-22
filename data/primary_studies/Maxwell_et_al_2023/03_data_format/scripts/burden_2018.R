## import data from Burden et al 2018, UK CEH dataset
## from saltmarshes of different ages on the Essex coast, UK 2011
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.07.22

# Jaxine Wolfe checked 21.02.24
# we already have this data

library(tidyverse)

input_file01 <- "reports/03_data_format/data/core_level/Burden_2018_UKCEH/Saltmarsh _Chronosequence_data_2011.csv"

input_data01 <- read.csv(input_file01)


### filtering only natural sites
input_data02 <- input_data01 %>% 
  rename(Site = SiteName) %>% 
  filter(Site == "Barrow Hill Natural" |
           Site == "Brandy Hole Natural" |
           Site == "Ferry Lane (B) Natural" |
           Site == "North Fambridge Natural" |
           Site == "Northey Natural" |
           Site == "Orplands Natural" |
           Site == "Tollesbury Natural" |
           Site == "Wallasea Island Natural" )

##### add informational  
source_name <- "Burden et al 2018"
author_initials <- "AB"


input_data03 <- input_data02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, SiteCode),
         Habitat_type = "Salt marsh",
         Country = "UK") %>% 
  rename(Plot = SiteCode)


#### reformat data ####

# lat and long in supplementary as easting and northing
#converted using https://webapps.bgs.ac.uk/data/webservices/convertForm.cfm#bngToLatLng 

input_data04 <- input_data03 %>% 
  mutate(Longitude = case_when(Site == "Barrow Hill Natural" ~ "0.921222",
                                Site == "Brandy Hole Natural" ~ "0.645383",
                                Site == "Ferry Lane (B) Natural" ~ "0.960211",
                                Site == "North Fambridge Natural" ~ "0.663894",
                                Site == "Northey Natural" ~ "0.709383",
                                Site == "Orplands Natural" ~ "0.859936",
                                Site == "Tollesbury Natural" ~ "0.836512",
                                Site == "Wallasea Island Natural" ~ "0.811350"),
         Latitude = case_when(Site == "Barrow Hill Natural" ~ "51.797117",
                              Site == "Brandy Hole Natural" ~ "51.629647",
                              Site == "Ferry Lane (B) Natural" ~ "51.852865",
                              Site == "North Fambridge Natural" ~ "51.639013",
                              Site == "Northey Natural" ~ "51.719498",
                              Site == "Orplands Natural" ~ "51.718446",
                              Site == "Tollesbury Natural" ~ "51.769154",
                              Site == "Wallasea Island Natural" ~ "51.621201")) %>% 
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) %>% 
  rename(OC_perc = Carbon,
         SOM_perc = OrganicMatter,
         N_perc = Nitrogen,
         BD_reported_g_cm3 = BulkDensity) %>% 
  mutate(Year_collected = "2011") %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")



## edit depth

input_data05 <- input_data04 %>%
  mutate(U_depth_cm = 0, #information in metadata
         L_depth_cm = 30) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100, 
         DOI = "https://doi.org/10.5285/0b1faab4-3539-457f-9169-b0b1fbd59bc2")# cm to m


#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, N_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



