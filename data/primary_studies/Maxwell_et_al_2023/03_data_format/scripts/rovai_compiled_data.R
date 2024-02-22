## import data compiled by Andre Rovai (personal communication)
## export for marsh soil C 
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 07.07.22
# edit 28.02.23

library(tidyverse)
input_file01 <- "reports/03_data_format/data/meta_analysis/Rovai_compiled/Andre_marsh_tidalfresh.csv"

input_data01 <- read.csv(input_file01)


source_name <- "Rovai compiled"
author_initials <- "ASR"


#### subset to remove CCRCN data (already have this data) ####

input_data02 <- input_data01 %>% 
  filter(Collated_by != "CCRCN") 



#### reformat to match other datasets
input_data02$study_id.x <- input_data02$study_id.x %>% 
  fct_relabel(~ gsub("_", " ", .x)) %>% 
  fct_relabel(~ gsub("etal", "et al", .x))%>% 
  fct_relabel(~ gsub("\\(", "", .x)) %>% 
  fct_relabel(~ gsub("\\)", "", .x)) %>% 
  fct_relabel(~ gsub("\\.", "", .x)) %>% 
  fct_relabel(~ gsub("\\,", "", .x))

input_data03 <- input_data02 %>% 
  mutate(U_depth_m = depth_min/100, #cm to m
         L_depth_m = depth_max/100) %>%  #cm to m
  dplyr::rename(Source = Collated_by,
    Original_source = study_id.x,
    Country = country,
         Latitude = core_latitude,
         Longitude = core_longitude,
         Site = site_id.x,
         Core = core_id,
         Habitat_type = vg_typ) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Site_name = Core)
  

test <- input_data03 %>% 
  mutate(Core_site = coalesce(Site, Core)) %>% 
  relocate( Core,Core_site, .after = Site)
#rename source "This study" to Rovai

input_data03$Source <- input_data03$Source %>% 
  fct_relabel(~ gsub("This study", "Rovai compiled", .x)) 


#making carbon data  match main dataset
input_data04 <- input_data03 %>% 
  mutate(OC_perc_toedit = fraction_carbon*100,
         SOM_perc_toedit = fraction_organic_matter*100) %>% 
  dplyr::rename(BD_reported_g_cm3_toedit = dry_bulk_density) %>% 
  mutate(Original_source = as.factor(Original_source))


#### correcting wrong source names and removing sources where no data was extracted

input_data05 <- input_data04 %>%
  filter(Original_source != "Adame et al 2005" & # actually Adame et al 2015, which is already extracted
           Original_source != "Callaway et al 1996") %>%  #no data for studies (figures at Cs not C)
  filter(is.na(fraction_carbon) == FALSE | is.na(fraction_organic_matter) == FALSE) %>% ## delete
  droplevels() 
  
input_data05$Original_source <- input_data05$Original_source %>%  
  fct_relabel(~ gsub("Fuchs et al 2018 1", "Fuchs et al 2018", .x)) %>% 
  fct_relabel(~ gsub("Sousa etat 2010", "Sousa et al 2010", .x)) 



#### adding year_collected, method, and DOI - investigated in each study

sources <- input_data05 %>% 
  group_by(Source, Original_source, Core) %>% 
  count()

table(input_data05$Original_source)

locations <- input_data05 %>% 
  group_by(Original_source, Core) %>% 
  distinct(Latitude,Longitude) %>% 
  count()

input_data06 <- input_data05 %>% 
  mutate(Year_collected = case_when(Original_source == "Anisfeld et al 1999" ~ "1996", #core-level
                                    Original_source == "Bryant and Chabreck 1998" ~ "1994", #core-level
                                    Original_source == "Cahoon et al 1996" ~ "1993", #core-level
                                    Original_source == "Chmura and Hung 2004" #core-level
                                    & Core == "CMHG_04_Core_13" ~ "1994",
                                    Original_source == "Chmura and Hung 2004"
                                    & Core == "CMHG_04_Core_14" ~ "1996",
                                    Original_source == "Chmura and Hung 2004"
                                    & Core == "CMHG_04_Core_15" ~ "1996",
                                    Original_source == "Chmura and Hung 2004"
                                    & Core != "CMHG_04_Core_13" & Core != "CMHG_04_Core_14"
                                    & Core != "CMHG_04_Core_15" ~ "2000",
                                    Original_source == "Connor et al 2001" ~ "1998", #core-level
                                    Original_source == "Craft et al 1993" ~ "1988", #core-level
                                    Original_source == "Day et al 2011" ~ "1992", # SITE-LEVEL #best guess: "More details of these measurements are given in Day et al. (1994).
                                    Original_source == "Fuchs et al 2018" ~ "2016", #SITE-LEVEL
                                    #Original_source == "Hatton et al 1983" ~ , # SITE-LEVEL # collection date NOT included
                                    Original_source == "Loomis and Craft 2010" ~ "2005" , #SITE-LEVEL
                                   # Original_source == "Macreadie et al 2017" ~  # SITE-LEVEL # compiled from studies - various dates
                                   Original_source == "Markewich et al 1998" ~ "1996", #core-level BUT TWO CORES IN SAME GEOGRAPHIC LOCATION
                                   #Original_source == "Morris and Jensen 1998" ~ "", # SITE-LEVEL, no sampling date
                                   #Original_source == "Orson et al 1998" ~ "", #core-level # collection date NOT included
                                   Original_source == "Patrick and DeLaune 1990" ~ "1983", #core-level
                                   Original_source == "Roman et al 1997" ~ "1991", #core-level
                                   Original_source == "Rybczyk and Cahoon 2002" ~ "1992", # core-level
                                   Original_source == "Siewert et al 2016" ~ "2013", # core-level but averaged to 0-100cm
                                   Original_source == "Sousa et al 2010" & #SITE-LEVEL
                                     Core == "SUS_10_Gala" ~ "1997", 
                                   Original_source == "Sousa et al 2010" & 
                                     Core == "SUS_10_Jusante" ~ "1997",
                                   Original_source == "Sousa et al 2010" & 
                                     Core == "SUS_10_Pancas" ~ "1998",
                                   Original_source == "Sousa et al 2010" & 
                                     Core == "SUS_10_Corroios" ~ "1998",
                                   Original_source == "Sun et al 2019" ~ "2017", # SITE-LEVEL
                                   Original_source == "Ye et al 2015" ~ "2007", # SITE-LEVEL
                                   Original_source == "Zubrzycki et al 2013" ~ "2011")) %>% # SITE-LEVEL
  
  #when studies over several years: 
  mutate(Year_collected_end = case_when(Original_source == "Roman et al 1997" ~ "1992",
                                        Original_source == "Sousa et al 2010" & 
                                          Core == "SUS_10_Gala" ~ "1998", #SITE-LEVEL
                                        Original_source == "Sousa et al 2010" & 
                                          Core == "SUS_10_Jusante" ~ "1998",
                                        Original_source == "Sousa et al 2010" & 
                                          Core == "SUS_10_Pancas" ~ "1999",
                                        Original_source == "Sousa et al 2010" & 
                                          Core == "SUS_10_Corroios" ~ "1999")) %>% 
  
  mutate(Method = case_when(is.na(fraction_carbon) == FALSE ~ "EA", # andre: included org C values in this column
                            #note: unsure about Anisfeld --> measured both LOI data and CHN data, 
                            #but unclear if conversion factor was used for figures where data was extracted
                            is.na(fraction_carbon) == TRUE ~ "LOI")) %>% 
  
  mutate(DOI = case_when(Original_source == "Anisfeld et al 1999" ~ "https://doi.org/10.2307/1352980",
                         Original_source == "Bryant and Chabreck 1998" ~ "https://doi.org/10.2307/1352840",
                         Original_source == "Cahoon et al 1996" ~ "https://doi.org/10.1006/ecss.1996.0055",
                         Original_source == "Chmura and Hung 2004" ~ "https://doi.org/10.1007/BF02803561",
                         Original_source == "Connor et al 2001" ~ "https://doi.org/10.1029/2000GB001346",
                         Original_source == "Craft et al 1993" ~ "https://doi.org/10.1006/ecss.1993.1062",
                         Original_source == "Day et al 2011" ~ "https://doi.org/10.1016/j.ecoleng.2010.11.021",
                        # Original_source == "Fuchs et al. 2018c" ~ "https://doi.org/10.1007/s41063-018-0056-9", # DATA NOT EXTRACTED
                         Original_source == "Fuchs et al 2018" ~ "Soil carbon and nitrogen stocks in Arctic river deltas: 
                        New data for three Northwest Alaskan deltas,  5TH EUROPEAN CONFERENCE ON PERMAFROST",
                         Original_source == "Hatton et al 1983" ~ "https://doi.org/10.4319/lo.1983.28.3.0494",
                         Original_source == "Loomis and Craft 2010" ~ "https://doi.org/10.2136/sssaj2009.0171",
                         Original_source == "Macreadie et al 2017" ~ "https://doi.org/10.1038/srep44071",
                        Original_source == "Markewich et al 1998" ~ "https://doi.org/10.3133/ofr98429",
                        Original_source == "Morris and Jensen 1998" ~ "https://doi.org/10.1046/j.1365-2745.1998.00251.x",
                        Original_source == "Orson et al 1998" ~ "https://doi.org/10.1006/ecss.1998.0363",
                        Original_source == "Patrick and DeLaune 1990" ~ "https://doi.org/10.4319/lo.1990.35.6.1389",
                        Original_source == "Roman et al 1997" ~ "https://doi.org/10.1006/ecss.1997.0236",
                        Original_source == "Rybczyk and Cahoon 2002" ~ "https://doi.org/10.1007/BF02691346",
                        Original_source == "Siewert et al 2016" ~ "http://dx.doi.org/10.1016/j.catena.2016.07.048 ; https://doi.org/10.1594/PANGAEA.862959",
                        Original_source == "Sousa et al 2010" ~ "https://doi.org/10.1016/j.marpolbul.2010.02.018",
                        Original_source == "Sun et al 2019" ~ "https://doi.org/10.1016/j.scitotenv.2019.04.122", 
                        Original_source == "Ye et al 2015" ~ "https://doi.org/10.1007/s12237-014-9927-x",
                        Original_source == "Zubrzycki et al 2013" ~ "https://doi.org/10.5194/bg-10-3507-2013" ))


##add state and country info
input_data07 <- input_data06 %>%   
  mutate(State = case_when(Original_source == "Anisfeld et al 1999" ~ "Connecticut",
                           Original_source == "Bryant and Chabreck 1998" ~ "Louisiana",
                           Original_source == "Cahoon et al 1996" ~ "California", 
                           Original_source == "Chmura and Hung 2004" ~ "Nova Scotia, New Brunswick",
                           Original_source == "Connor et al 2001" ~ "Nova Scotia, New Brunswick",
                           Original_source == "Craft et al 1993" ~ "North Carolina",
                           Original_source == "Day et al 2011" ~ "Mississippi",
                           Original_source == "Fuchs et al 2018" ~ "Alaska",
                           Original_source == "Hatton et al 1983" ~"Louisiana" ,
                           Original_source == "Loomis and Craft 2010" ~ "Georgia",
                           Original_source == "Macreadie et al 2017" &
                             Location == "NSW" ~  "New South Wales",
                           Original_source == "Macreadie et al 2017" &
                             Location == "QLD" ~  "Queensland",
                           Original_source == "Macreadie et al 2017" &
                             Location == "SAUS" ~  "South Australia",
                           Original_source == "Macreadie et al 2017" &
                             Location == "VCT" ~  "Victoria",
                           Original_source == "Macreadie et al 2017" &
                             Location == "WAUS" ~  "Western Australia", 
                           Original_source == "Markewich et al 1998" ~ "Louisiana",
                          # Original_source == "Morris and Jensen 1998" ~ "", # denmark
                           Original_source == "Orson et al 1998" ~ "Connecticut",
                           Original_source == "Patrick and DeLaune 1990" ~ "California",
                           Original_source == "Roman et al 1997" ~ "Massachusetts",
                           Original_source == "Rybczyk and Cahoon 2002" ~ "Louisiana")) %>%
                           # Original_source == "Siewert et al 2016" ~ "", # russia
                           #Original_source == "Sousa et al 2010" ~ "", # in portugal
                           #Original_source == "Sun et al 2019" ~ "", # china
                           # Original_source == "Ye et al 2015" ~ "", # china
                          # Original_source == "Zubrzycki et al 2013" ~ "" # russia
  
  mutate(Country = case_when(Original_source == "Anisfeld et al 1999" |
                               Original_source == "Bryant and Chabreck 1998" |
                               Original_source == "Cahoon et al 1996" |
                               Original_source == "Craft et al 1993"  |
                               Original_source == "Day et al 2011" | 
                               Original_source == "Fuchs et al 2018" |
                               Original_source == "Hatton et al 1983" |
                               Original_source == "Loomis and Craft 2010" |
                               Original_source == "Markewich et al 1998" |
                               Original_source == "Orson et al 1998" |
                               Original_source == "Patrick and DeLaune 1990" |
                               Original_source == "Roman et al 1997" |
                               Original_source == "Rybczyk and Cahoon 2002" ~ "United States",
                           
                             Original_source == "Chmura and Hung 2004" | 
                               Original_source == "Connor et al 2001" ~ "Canada",
                             Original_source == "Macreadie et al 2017" ~ "Australia",
                             Original_source == "Sousa et al 2010" ~ "Portugal",
                             Original_source == "Ye et al 2015" | 
                               Original_source == "Sun et al 2019" ~ "China",
                             Original_source == "Siewert et al 2016" |
                               Original_source == "Zubrzycki et al 2013" ~ "Russia",
                             Original_source == "Morris and Jensen 1998" ~ "Denmark"))
#Original_source == "" ~ ,

#### change core to site-level data 

## Day et al 2011 
# Core = DAY11_BC is mean of 4 cores
# Core = DAY11_OOB is mean of 5 cores

#Fuchs et al 2018
# n = unsure, 8? (26 total within 3 locations) 
# s.d. in table 1 


# Hatton et al 1983
#no n for mean of cores, but s.e. or s.d. in table 
# Conv_factor = 1.724

#Macreadie et al 2017 
# means
# core numbers and s.d. are in supplementary:
#https://static-content.springer.com/esm/art%3A10.1038%2Fsrep44071/MediaObjects/41598_2017_BFsrep44071_MOESM41_ESM.pdf


# Morris and Jensen 1998 
# mean for grazed and ungrazed
# n = 3
# sd in table 2 


# CAUTION: Markewich et al 1998 two cores with same geographic coordinates
# CAUTION: Roman et al 1997 two cores with same geographic coordinates

# Sousa et al 2010
# n = 6 (sampled bimonthly for a year), no s.d. values


# Sun et al 2019
# n = 9, s.d values in table (0.0023)

# Zubrzycki et al 2013
# n = not specified
# average per soil depth (BD sd in table, C sd in figure)


#### export

export_data01 <- input_data07 %>% 
  dplyr::select(Source, Original_source, Site_name, Core, Habitat_type, Country, State, 
                Year_collected, Year_collected_end,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, OC_perc_toedit, SOM_perc_toedit, BD_reported_g_cm3_toedit, DOI) %>% 
  dplyr::rename(Compiled_by = Source, 
                Source = Original_source) 
  
  
#write.csv(export_data01, "Data from Andre.csv")


#### renaming OC_perc column to OC_perc_mean when it is site-level

export_data02 <- export_data01 %>% 
  mutate(OC_perc_mean = case_when(Source == "Day et al 2011"
                                  | Source == "Fuchs et al 2018" | Source == "Hatton et al 1983"
                                  | Source == "Loomis and Craft 2010" | Source == "Macreadie et al 2017"
                                  | Source == "Morris and Jensen 1998"| Source == "Sousa et al 2010"
                                  | Source == "Sun et al 2019"| Source == "Ye et al 2015" 
                                  | Source == "Zubrzycki et al 2013" ~ OC_perc_toedit),
         SOM_perc_mean = case_when(Source == "Day et al 2011"
                                  | Source == "Fuchs et al 2018" | Source == "Hatton et al 1983"
                                  | Source == "Loomis and Craft 2010" | Source == "Macreadie et al 2017"
                                  | Source == "Morris and Jensen 1998"| Source == "Sousa et al 2010"
                                  | Source == "Sun et al 2019"| Source == "Ye et al 2015" 
                                  | Source == "Zubrzycki et al 2013" ~ SOM_perc_toedit),
         BD_reported_g_cm3_mean = case_when(Source == "Day et al 2011"
                                  | Source == "Fuchs et al 2018" | Source == "Hatton et al 1983"
                                  | Source == "Loomis and Craft 2010" | Source == "Macreadie et al 2017"
                                  | Source == "Morris and Jensen 1998"| Source == "Sousa et al 2010"
                                  | Source == "Sun et al 2019"| Source == "Ye et al 2015" 
                                  | Source == "Zubrzycki et al 2013" ~ BD_reported_g_cm3_toedit)) %>% 
  mutate(OC_perc = case_when(is.na(OC_perc_mean) == TRUE ~ OC_perc_toedit),
         SOM_perc = case_when(is.na(OC_perc_mean) == TRUE ~ SOM_perc_toedit),
         BD_reported_g_cm3 = case_when(is.na(OC_perc_mean) == TRUE ~ BD_reported_g_cm3_toedit)) # only place value here if OC_perc_mean is NA (i.e. this is core-level not site-level)



export_data03 <- export_data02 %>% 
  dplyr::select(Compiled_by, Source, Site_name, Core, Habitat_type, Country, State, 
                Year_collected, Year_collected_end,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, OC_perc, SOM_perc, BD_reported_g_cm3,
                OC_perc_mean, SOM_perc_mean, BD_reported_g_cm3_mean, DOI)


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data03

write.csv(export_df, export_file)





