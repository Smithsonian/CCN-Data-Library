## import data from Kohfeld et al 2022, Biogeosciences
#https://doi.org/10.5194/bg-19-5751-2022
#dataset located at https://doi.org/10.1594/PANGAEA.947824
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.01.23
# edit 15.05.23 to corrected dbd and corrected depth value, and exporting soil type

library(tidyverse)
library(janitor) # to clean names



#to check location points
library("ggmap")
library(maptools)
library(maps)



input_file01 <- "reports/03_data_format/data/core_level/Kohfeld_2022_PANGEA/03_Carbon_data.tab"

input_data01 <- read.table(input_file01, sep = '\t', header = T) %>% 
  clean_names()

input_file02 <- "reports/03_data_format/data/core_level/Kohfeld_2022_PANGEA/core_locations.csv" 

library(stringr)
locations  <- read.csv(input_file02) 
locations <- locations %>% 
  mutate(Core  = str_trim(locations$Core))


##### add informational  
source_name <- "Kohfeld et al 2022"
author_initials <- "KEH"


input_data02 <- input_data01 %>% 
  dplyr::rename(location_abbrv = location,
                Core = event_core_id) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = "Salt marsh",
         Country = "Canada") 

### add location

input_data03 <- full_join(input_data02, locations, by = "Core") 


#### reformat data ####

input_data04 <- input_data03 %>% 
  rename(OC_perc = tc_elemental_analyser, # "Inorganic C was negligible in all 93 of the subsamples analyzed (max: 0.015 %) and assumed to be zero for all C calculation purposes."
         SOM_perc = loi,
         BD_reported_g_cm3 = dbd_g_cm_3_corrected_for_compaction_usin, 
         U_depth_m = depth_cor_m_upper_depth,
         Soil_type = sediment_soil_type) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA") # this is the method for the OC_perc column

#NOTE: this is the first study where both measured OC and estimated OC from LOI are included
# will keep the measured OC as EA, and convert 

  
plot(input_data04$SOM_perc, input_data04$OC_perc) # SOM and OC look good


#### check location points ####


mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(45,55)+
  xlim(-130,-120)+
  geom_point(data = input_data04, aes(x = Longitude, y = Latitude
                                      ), alpha = 0.5)
mp

## add a lower depth 

input_data05 <- input_data04 %>% 
  group_by(Core) %>% #calculations for each core separately
  mutate(diff = U_depth_m - lead(U_depth_m)) %>% # difference is U_depth - the next value
  mutate(diff = case_when(is.na(diff) == TRUE ~ lag(diff), #for the bottom of the core (when diff is NA), take the previous diff value (i.e. lag(diff))
                          TRUE~diff)) %>% #keep diff value if not NA
  mutate(L_depth_m = U_depth_m - diff) %>% # -diff because diff are negative values
  relocate (diff,L_depth_m, .after = U_depth_m) %>% 
  ungroup()

table(input_data05$diff) 


#### export ####

export_data01 <- input_data05 %>% 
  mutate(DOI = "https://doi.pangaea.de/10.1594/PANGAEA.947824") %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Soil_type, Country, State, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Soil_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, State, Year_collected, .before = U_depth_m) 


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)

