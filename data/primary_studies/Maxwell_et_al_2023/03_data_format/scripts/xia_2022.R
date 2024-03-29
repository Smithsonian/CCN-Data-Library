## import data from Xia et al 2022, supplementary info
# https://onlinelibrary.wiley.com/doi/10.1111/gcb.16325
## from Coastal wetlands of China
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 27.07.22
# edit 20.12.22, 04.04.23 for permille to percent
# edit 16.11.23 for permille to percent for nitrogen as well

library(tidyverse)

## import SOC data
input_file01 <- "reports/03_data_format/data/core_level/Xia_2022_FigShare/Xia_2022_SI.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
  slice(1:634) %>% 
  dplyr::select(-c(X:X.11))


## import locations
input_file02 <- "reports/03_data_format/data/core_level/Xia_2022_FigShare/Xia_2022_SI_locations.csv"

study_locations <- read.csv(input_file02)


##join locations to soil organic content
input_data02 <- full_join(input_data01, study_locations, by = c("Province", "Wetland", "Site"))


## import soil properties (bulk density)

input_file03 <- "reports/03_data_format/data/core_level/Xia_2022_FigShare/Xia_2022_SI_properties.csv"
study_properties <- read.csv(input_file03)

study_properties <- study_properties %>% 
  slice(1:472) %>% 
  dplyr::select(-X)



### FULL JOIN
input_data03 <- full_join(input_data02, study_properties, 
                          by = c("Province", "Wetland", "Site", "Soil.depth")) 


input_data03 <- input_data03 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _


##### add informational  
source_name <- "Xia et al 2022"
author_initials <- "SX"


input_data04 <- input_data03 %>% 
  mutate(Original_source  = case_when(Reference == "This study" ~ "Xia et al 2022",
                                      TRUE ~ Reference)) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Core = paste(Wetland, Site),
         Site_name = paste(Source_abbr, Core),
         Country = "China") %>% 
  dplyr::rename(n_plot = Site,
      State = Province,
         Site = Wetland)


## add a column with the habitat type
table(input_data04$Vegetation)

input_data04$Vegetation <- as.factor(input_data04$Vegetation)
list(levels(input_data04$Vegetation))

input_data05 <- input_data04 %>% 
  mutate(Habitat_type = case_when(Vegetation == "A. corniculatum" | Vegetation == "A. marina" |
                                    Vegetation == "Avicennia marina" |
                                  Vegetation == "B. gymnorrhiza" |
                                    Vegetation == "B. sexangula"|
                                    Vegetation == "B. sexangula, Hibiscus tiliaceus" |
                                    Vegetation ==  "C. tagal"|
                                    Vegetation == "Ceriops tagal" |
                                    Vegetation == "K. obovata"  |
                                    Vegetation == "K. obovata, R. stylosa" |
                                    Vegetation == "Kandelia obovate" |
                                    Vegetation == "R. stylosa"|
                                    Vegetation == "R. stylosa, C. tagal" |
                                    Vegetation == "S. apetala" ~ "Mangrove",
                                  
                                  Vegetation == "Aeluropus littoralis, P. australis" |
                                    Vegetation == "P. australis" |
                                    Vegetation == "Phragmites australis" |
                                    Vegetation == "Phragmites australis, Suaeda salsa"|
                                    Vegetation ==  "S. alterniflora"|
                                    Vegetation == "S. alterniflora, S. salus" |
                                    Vegetation == "S. salus"|
                                    Vegetation == "Spartina alterniflora"|
                                    Vegetation == "Suaeda glauca" |
                                    Vegetation == "Suaeda salsa" ~ "Saltmarsh", 
                                  
                                  Vegetation == "Tidal flat"  ~ "Tidal flat"))

data_test <- input_data05 %>% 
  filter(is.na(Habitat_type))

## for 13 rows, location and vegetation class not included in original dataset.
#these will be removed when filtering for a certain habitat type


#### reformat data ####

input_data06 <- input_data05 %>% 
  mutate(OC_perc = as.numeric(SOC_content__g_kg_1_)/10, # from per mill to per cent
         N_perc = as.numeric(TN__g_kg_1_)/10, # from per mill to per cent
         BD_reported_g_cm3 = as.numeric(Bulk_density_g_cm_3_)) %>% 
  mutate(Year_collected = NA,
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") 



## edit depth

input_data07 <- input_data06 %>% 
  separate(Soil_depth, c("U_depth_cm", "L_depth_cm"), sep = '—') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

input_data08 <- input_data07 %>% 
  filter(is.na(OC_perc) == FALSE | is.na(SOC_density_Mg_ha_1_) == FALSE)

test <-  input_data08 %>% 
  mutate(GPS_combined = paste(Latitude, Longitude)) %>% 
  group_by(Source, Site_name, Core)  %>% 
  dplyr::summarise(distinct_location = n_distinct(GPS_combined))

#### export ####

export_data01 <- input_data08 %>% 
  dplyr::select(Source, Original_source, Site_name, Core, Habitat_type, Country, State, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3)


## filter for saltmarsh and remove values with nd in OC_perc

export_data02 <- export_data01 %>% 
  filter(Habitat_type == "Saltmarsh") %>% 
  filter(OC_perc != "nd") %>% 
  #remove values without Lat and Long values
  filter(Latitude != "—") %>% 
  mutate(N_perc = gsub("nd", NA, N_perc),
         BD_reported_g_cm3 = gsub("nd", NA, BD_reported_g_cm3),) %>% 
  mutate(Original_source = gsub("\\.", "",
                    gsub("\\,", "", Original_source))) %>% 
  mutate(Original_source = fct_recode(Original_source, "Yang et al 2016" = "Yang 2016"))

#### add info for studies ####

export_data03 <- export_data02 %>% 
  mutate(Year_collected = case_when(Original_source == "Xia et al 2022" ~ "2015", # core-level
                                    Original_source == "Gao et al 2016" ~ "2012", #SITE-LEVEL
                                    Original_source == "Liu et al 2017" ~ "2012")) %>%  # core-level
                                    #Original_source == "Xia et al 2022" ~ NA_real_ 
  mutate(Year_collected_end = case_when(Original_source == "Xia et al 2022" ~ "2019",
                                        Original_source == "Gao et al 2016" ~ "2013")) %>% 
  mutate(Method = case_when(Original_source == "Xia et al 2022" ~ "EA",
                            Original_source == "Gao et al 2016" ~ "EA",
                            Original_source == "Liu et al 2017" ~ "EA")) %>% 
                            #Original_source == "Xia et al 2022" ~ 
  mutate(DOI = case_when(Original_source == "Xia et al 2022" ~ "https://doi.org/10.6084/m9.figshare.20180450.v1",
                         Original_source == "Gao et al 2016" ~ "https://doi.org/10.1016/j.ecoleng.2016.06.088",
                         Original_source == "Liu et al 2017" ~ "https://doi.org/10.1016/j.ecoleng.2017.05.041",
                         Original_source == "Yang et al 2016" ~ "Yang, T., Ren, H., Zhang, Z., Chen, Y., & Jiang, D. (2016). Distribution and influence factors of soil organic carbon of different land-use types in the Jiangsu coastal areas. Journal of Subtropical Resources and Environment, 11, 46–52. (in Chinese)"))


export_data04 <- export_data03 %>% 
  relocate(Source, Original_source, Site_name, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, State, Year_collected, .before = U_depth_m) %>% 
  arrange(Original_source,Site_name, Habitat_type)


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data04

write.csv(export_df, export_file)


         