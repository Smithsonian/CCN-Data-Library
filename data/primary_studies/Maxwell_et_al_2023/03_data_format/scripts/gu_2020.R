## import data from Gu et al 2020, JGR Biogeosciences
#10.17632/2dg3spxsbh.2
#https://data.mendeley.com/datasets/2dg3spxsbh/2
## from Kyle of Tongue saltmarsh, Scotland, 2018
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.07.22
# edit 20.12.22


library(tidyverse)

# #to check location points
# library("ggmap")
# library(maptools)
# library(maps)


input_file01 <- "reports/03_data_format/data/core_level/Gu_2020_Mendeley_Data/Gu_2020_lapocatiere.csv"

input_data01 <- read.csv(input_file01)


##### add informational  
source_name <- "Gu et al 2020"
author_initials <- "JG"

input_data01 <- input_data01 %>% 
  slice(c(1:147)) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _



input_data02 <- input_data01 %>% 
  mutate(Site = "La Pocatiere") %>% 
  mutate(Plot = case_when(Core_ID == "LP1-1" | Core_ID == "LP1-2" | Core_ID == "LP1-3" ~ "LP1",
                          Core_ID == "LP2-1" | Core_ID == "LP2-2" | Core_ID == "LP2-3" ~ "LP2",
                          Core_ID == "LP3-1" | Core_ID == "LP3-2" | Core_ID == "LP3-3" ~ "LP3",
                          Core_ID == "SP1-1" | Core_ID == "SP1-2" | Core_ID == "SP1-3" ~ "SP1")) %>% 
  mutate(Replicate = case_when(Core_ID == "LP1-1" | Core_ID == "LP2-1" | 
                                 Core_ID == "LP3-1" | Core_ID == "SP1-1" ~ "Oct",
                               Core_ID == "LP1-2" | Core_ID == "LP2-2" | 
                                 Core_ID == "LP3-2" | Core_ID == "SP1-2" ~ "May",
                               Core_ID == "LP1-3" | Core_ID == "LP2-3" | 
                                 Core_ID == "LP3-3" | Core_ID == "SP1-3" ~ "June",
                               TRUE ~ Core_ID)) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core_ID),
         Habitat_type = "Salt marsh",
         Country = "Canada") 

#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = case_when(Core_ID == "LP1-1" | Core_ID == "LP2-1" | 
                                      Core_ID == "LP3-1"~ "2018", #from paper, https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2019JG005473
                                    Core_ID == "SP1-1" | Core_ID == "SP1-2" | 
                                      Core_ID == "SP1-3" ~ "2014",
                                    TRUE ~ "2019")) %>% 
  separate(Depth_range___cm_, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%  #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100,
         L_depth_m = as.numeric(L_depth_cm)/100) %>% 
  rename(OC_perc = X_OC_Craft_et_al_1991_,
         Core = Core_ID) %>% 
  mutate(accuracy_flag = "estimated from GE",
         accuracy_code = "2") %>% 
  mutate(Method = "LOI",
         Conv_factor = "0.4*x + 0.0025*x2",
         BD_reported_g_cm3 = Bulk_density_g_cm3_)


##add location data

input_data04 <- input_data03 %>% 
  mutate(Longitude = case_when(Plot == "LP1" ~ -70.05553421633046,
                               Plot == "LP2" ~ -70.05553421633046,
                               Plot == "LP3" ~ -70.05544224826002,
                               Plot == "SP1" ~ -70.05544224826002),
         Latitude = case_when(Plot == "LP1" ~ 47.37584461876665,
                               Plot == "LP2" ~ 47.37584461876665,
                               Plot == "LP3" ~ 47.37590455802447,
                               Plot == "SP1" ~ 47.37590455802447)) %>% 
  mutate(DOI = "https://doi.org/10.17632/2dg3spxsbh.2")

#### check location points ####



mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = input_data04, aes(x = Longitude, y = Latitude, 
                                      color = Site), alpha = 0.5)
mp

#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Plot, Replicate, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, Conv_factor, OC_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Plot, Replicate, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) 


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



