## import data from Gallagher et al 2021, supplementary in Wetland Soils
## from Tasmania, AUS
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 22.07.22

library(tidyverse)


input_file01 <- "reports/03_data_format/data/core_level/Gallagher_2021_SI/Gallagher_2021_TableS2.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _

plot(input_data01$TOC_perc, input_data01$Dry_bulk_density_g_ml)



##### add informational  
source_name <- "Gallagher et al 2021"
author_initials <- "JBG"


input_data02 <- input_data01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site_No),
         Habitat_type = "Salt marsh",
         Country = "Australia") %>% 
  rename(Plot = Site_No)


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(OC_perc = TOC_perc, 
         BD_reported_g_cm3 = Dry_bulk_density_g_ml) %>% #1g/ml = 1 g/cm3 
  mutate(Year_collected = "2018") %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Treatment = "Post-fires, input of black carbon") %>% 
  mutate(Method = "EA")


## edit depth
#An open-faced ‘Russian Peat Corer’ was used to take 50 cm cores of uncompressed 
#sediment for macro char profiles in vegetated salt marsh soils and stock measurements 
# within seagrass stands. The remaining salt marsh stock samples were exhumed carefully 
# with a PVC pipe and shovel from the base of the soil, where quaternary sands mark the 
# depth of the salt marsh accumulation. Samples were immediately put on ice before transport. 
# The sample cores were mixed well within sealed plastic bags to physically represent the stock 
# variables average; after which a measured volume (≥ 5 cm3) was taken for dry bulk density 
# with a cut-off 20 cm3 syringe, in the manner of a piston corer. After drying at 60 °C 
# the remaining mixed samples were shaken and sieved through a 200 μm mesh to remove both 
# roots and large shells pieces before further processing for organic matter and
# elemental carbon analysis (Chew and Gallagher 2018).

# --> it seems that "Soil_depth_cm" is the lower depth, and the sample comes from 0 to Soil_depth_cm

input_data04 <- input_data03 %>% 
  mutate(U_depth_cm = 0) %>% 
  rename(L_depth_cm = Soil_depth_cm) %>% 
  mutate(L_depth_cm  = case_when(L_depth_cm == ">30" ~ "30", # unsure about the sedbury creek site
                                 TRUE ~ L_depth_cm)) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1007/s13157-021-01460-3")# cm to m


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Treatment, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Plot, Treatment, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)





