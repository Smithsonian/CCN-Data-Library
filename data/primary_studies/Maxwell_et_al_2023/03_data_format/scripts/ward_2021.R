## import data from Ward et al 2021, DRYAD dataset
## from California Blue Carbon, data associated to Biogeosciences paper
#https://doi.org/10.5194/bg-2021-27 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 20.07.22
# edit 20.12.22
# edit 29.08.2023 - added unique core letter manually only for salt marshes

library(tidyverse)


input_file01 <- "reports/03_data_format/data/core_level/Ward_2021_DRYAD/PublishedCoreData_edited.csv"

input_data01 <- read.csv(input_file01) %>% 
  dplyr::select(-X)



input_data01 <- input_data01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))  #replacing . in columns by _


##### add informational  
source_name <- "Ward et al 2021"
author_initials <- "MAW"


# ### create a column with replicate type
# ## NOT POSSIBLE -no unique identifier per core 
#
# input_data02 <- input_data01 %>% 
#   group_by(Site, Habitat_Type, Coordinates) %>% 
#   mutate(Replicate = seq(1:n()))



input_data02 <- input_data01 %>%
  mutate(Habitat_abbr = case_when(Habitat_Type == "Bare sed" ~ "BS",
                                  Habitat_Type == "Pan" ~ "Pan",
                                  Habitat_Type == "Salt Marsh" ~ "SM",
                                  Habitat_Type == "Seagrass" ~ "SG")) %>% 
  # filtering for marsh here, as have only manually added a Core_Id for SM in the original data
  filter(Habitat_Type == "Salt Marsh") %>% 
  rename(Core_per_site = Core_ID) %>% 
  mutate(Core = paste(Site, Habitat_abbr, Core_per_site)) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Country = "USA")

#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Habitat_type = Habitat_Type,
         OC_perc = OC__, 
         SOM_perc = TOM__,
         BD_reported_g_cm3 = Bulk_Density_g_cm3_) %>% 
  mutate(Year_collected = NA) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = "LOI",
         Conv_factor = "0.22*x^1.1") #see paper



## edit depth

input_data04 <- input_data03 %>% 
  rename(U_depth_cm = Top_Interval_cm_) %>% 
  mutate(L_depth_cm = U_depth_cm + 2) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

input_data04$U_depth_m <- round(input_data04$U_depth_m, 2)
input_data04$L_depth_m <- round(input_data04$L_depth_m, 2)



#### removing incorrect core locations ####

input_data05 <- input_data04 %>% 
  filter(Core != "Tomales Bay SM E", Core != "Tomales Bay SM F", 
         Core != "Elkhorn Slough SM F", Core != "Elkhorn Slough SM G",
         Core != "Elkhorn Slough SM H", Core != "Elkhorn Slough SM J",
         Core != "Elkhorn Slough SM K", Core != "Elkhorn Slough SM L")

#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site,Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, Conv_factor,
                OC_perc, SOM_perc, BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.5194/bg-18-4717-2021")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



