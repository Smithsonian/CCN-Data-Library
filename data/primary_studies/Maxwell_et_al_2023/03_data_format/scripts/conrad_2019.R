## import data from Conrad et al 2019, Frontiers in Marine Science
## export for tidal marsh soil C (and previously mangrove soil C)
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 04.07.22
#edited 20.02.23 to replace location to site



library(tidyverse)
library(measurements) #to convert to decimal degrees

input_file <- "reports/03_data_format/data/core_level/Conrad_2019/Conrad_2019_SI_for_reformat.csv"

input_data0 <- read.csv(input_file)

##### format data  #####

input_data1 <- input_data0 %>% 
  slice(1:111) %>% # keep only relevant data
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) #replacing . in columns by _


##### add informational  #####

source_name <- "Conrad et al 2019"
author_initials <- "SC"


input_data2 <- input_data1 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Location, Core)) 

## add information from paper



#### site data #####


input_data3 <- input_data2 %>% 
  filter(Habitat_type == "Saltmarsh") %>% 
  mutate(Latitude = case_when(Location == "Coffs" & Core == "Site 1" ~ "-30.29923485884984", #South
                                Location == "Coffs" & Core == "Site 2" ~ "-30.3031568840442",
                                Location == "Coffs" & Core == "Site 3" ~ "-30.29699976610663",
                                Location == "Corindi" & Core == "Site 1" ~ "-29.98214918238689", 
                                Location == "Corindi" & Core == "Site 2" ~ "-29.97604598646839", 
                                Location == "Corindi" & Core == "Site 3" ~ "-29.97422604374854", 
                                Location == "Corindi" & Core == "Site 4" ~ "-29.97683335444045", 
                                Location == "Corindi" & Core == "Site 5" ~ "-29.98868091031976", 
                                Location == "Wooli" & Core == "Site 1" ~ "-29.88759127623967",
                                Location == "Wooli" & Core == "Site 3" ~ "-29.85863752742738",
                                Location == "Wooli" & Core == "Site 5" ~ "-29.84363855963511"), 
         Longitude = case_when(Location == "Coffs" & Core == "Site 1" ~ "153.1248383998784", #East
                                 Location == "Coffs" & Core == "Site 2" ~ "153.1299701482165",
                                 Location == "Coffs" & Core == "Site 3" ~ "153.1381272123313",
                                 Location == "Corindi" & Core == "Site 1" ~ "153.2255485359246", 
                                 Location == "Corindi" & Core == "Site 2" ~ "153.2224732934468", 
                                 Location == "Corindi" & Core == "Site 3" ~ "153.2163639619591", 
                                 Location == "Corindi" & Core == "Site 4" ~ "153.2086949633706", 
                                 Location == "Corindi" & Core == "Site 5" ~ "153.2097512366367", 
                                 Location == "Wooli" & Core == "Site 1" ~ "153.2636823400601",
                                 Location == "Wooli" & Core == "Site 3" ~ "153.2621673778419",
                                 Location == "Wooli" & Core == "Site 5" ~ "153.2429509905861"), 
         accuracy_flag = "estimated from GE",
         accuracy_code = "2",
         Year_collected = case_when(Location == "Coffs" ~ 2016, 
                                    Location == "Corindi"~ 2017,
                                    Location == "Wooli" ~ 2018),
         Country = "Australia") %>% 
  dplyr::rename(Site = Location)

# ### PREVIOUS METHOD FROM FIGURE 1 MAP 
# 
# input_data3 <- input_data2 %>% 
# 
#   mutate(lat_detail = case_when(Location == "Coffs" ~ "30°18'0°", #South
#                          Location == "Corindi"~ "29°59'03°", 
#                          Location == "Wooli" ~ "29°51'17°"), 
#          long_detail = case_when(Location == "Coffs" ~ "153°7'7°", #Eeast
#                           Location == "Corindi"~ "153°13'38°", 
#                           Location == "Wooli" ~ "153°13'41°"), 
#          lat = gsub("°", " ",
#                     gsub("'", " ", lat_detail)),
#          long = gsub("°", " ",
#                      gsub("'", " ", long_detail)),
#          lat_dec_deg = measurements::conv_unit(lat, from = "deg_min_sec", to = "dec_deg"), #S , need to convert dec_deg to negative
#          long_dec_deg = measurements::conv_unit(long, from = "deg_min_sec", to = "dec_deg"), #E , keep positive
#          accuracy_flag = "estimated from GE",
#          accuracy_code = "2",
#          Latitude = as.numeric(lat_dec_deg)*-1,
#          Longitude = as.numeric(long_dec_deg),
#          Year_collected = case_when(Location == "Coffs" ~ 2016, 
#                                     Location == "Corindi"~ 2017,
#                                     Location == "Wooli" ~ 2018),
#          Country = "Australia")



##### horizon data  #####

#"Once collected, sediment cores were sectioned into 2 cm intervals"
#depth_interval_cm_ is middle of 2cm interval


# "Organic C content was calculated by multiplying the organic material, 
# determined from the loss on ignition (LOI) method, by 0.58"

input_data4 <- input_data3 %>% 
  mutate(U_depth_cm = depth_interval_cm_ - 1, #upper depth: interval -1 
         L_depth_cm = depth_interval_cm_ + 1, #lower depth: interval +1
         U_depth_m = U_depth_cm/100 , #cm to m
         L_depth_m = L_depth_cm/100,# cm to m
         Depth_to_bedrock_m = Depth_to_bedrock_cm_/100, #cm to m
         OC_perc = C_content_g_C_per_g_sediment_*100, #convert to percent OC %
         SOM_perc = OC_perc/0.58, #reconverting back to SOM using 0.58 factor
         BD_reported_g_cm3 = DBD_g_per_cm_3_) %>%   #1 g cm-3 = 1 Mg m-3
  mutate(Method = "LOI",
         Conv_factor = 0.58,
         DOI = "https://doi.org/10.3389/fmars.2018.00518")  
    
input_data4$SOM_perc <- round(input_data4$SOM_perc, 2)


##### prepare for export  #####

## reformat
export_data <- input_data4 %>% 
  select(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
         accuracy_flag, accuracy_code, Country, Year_collected, Depth_to_bedrock_m, U_depth_m, L_depth_m, 
         Method, Conv_factor, OC_perc, SOM_perc, BD_reported_g_cm3, DOI)

## subset for marshes
export_data_marsh <- export_data %>% 
  filter(Habitat_type == "Saltmarsh")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data_marsh

write.csv(export_df, export_file)
