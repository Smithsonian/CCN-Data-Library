## import data from Kauffman et al 2020, SWAMP dataset
## from Kauffman et al 2017, Bio Letters
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 04.07.22

library(tidyverse)
library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date

#to check location points
library("ggmap")
library(maptools)
library(maps)


input_file01 <- "reports/03_data_format/data/core_level/Kauffman_2020_CIFOR/Geographic Coordinate for SWAMP dataset-Mangrove soil carbon-Brazil-2017.csv"

input_file02 <- "reports/03_data_format/data/core_level/Kauffman_2020_CIFOR/SWAMP Data-Soil carbon-Marisma Low-2017-Brazil.csv"
input_file03 <- "reports/03_data_format/data/core_level/Kauffman_2020_CIFOR/SWAMP Data-Soil carbon-Marisma Medium-2017-Brazil.csv"
input_file04 <- "reports/03_data_format/data/core_level/Kauffman_2020_CIFOR/SWAMP Data-Soil carbon-Marisma High-2017-Brazil.csv"


input_data_location01 <- read.csv(input_file01)

input_data_low <- read.csv(input_file02)
input_data_medium <- read.csv(input_file03)
input_data_high <- read.csv(input_file04)

##### merge three soil datasets ####
input_data_low <- input_data_low[c(1:30), c(1:20)]
input_data_medium <- input_data_medium[c(1:30), c(1:20)]
input_data_high <- input_data_high[c(1:30), c(1:20)]

input_data_soil01 <- rbind(input_data_low, input_data_medium, input_data_high)


#### format location dataset ####

input_data_location02 <- input_data_location01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%  #replacing . in columns by _
  rename(Plot = Site,
         Sub_plot = Sub_Plot) %>%  # to match soil dataset
  mutate(Plot = fct_recode(Plot, "Marisma Low" = "Marisma low", 
                           "Marisma Medium" = "Marisma medium",
                           "Marisma High" = "Marisma high"))

input_data_location03 <- input_data_location02 %>% 
  filter(Plot == "Marisma Low" | Plot == "Marisma Medium" | Plot == "Marisma High")


input_data_location04 <- input_data_location03 %>%
   mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1")


#### check location points ####

mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = input_data_location04, aes(x = Longitude, y = Latitude, 
                                               color = Plot), alpha = 0.5)
mp


#### format soil dataset ####
input_data_soil02 <- input_data_soil01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%   #replacing . in columns by _
  mutate_if(is.character, as.factor)


##### add informational  
source_name <- "Kauffman et al 2020"
author_initials <- "JBK"


input_data_soil03 <- input_data_soil02 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot, Sub_plot),
         Habitat_type = "Marshes",
         Year_collected = "2017",
         Country = "Brazil")



input_data_soil04 <- input_data_soil03 %>% 
  separate(Depth_interval_cm_, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_cm = fct_recode(U_depth_cm, "100" = "300"), # upper_Depth is 100cm. see backcalculations from C stocks value
         # equation found in https://www.cifor.org/publications/pdf_files/WPapers/WP86CIFOR.pdf
         L_depth_cm = case_when(U_depth_cm == "100" ~ "300", #lower depth is 300cm as indicated
                                TRUE ~ as.character(L_depth_cm))) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,# cm to m
         Depth_to_bedrock = NA) %>% 
  rename(OC_perc = Carbon_content__, #percent OC)
          BD_reported_g_cm3 = Bulk_density_g_cm3_, 
          N_perc = Nitrogen_content__) %>%  #percent N
  mutate(Method = "EA")
  
#### merge location and soil datasets ####

input_data_location05 <- input_data_location04 %>% 
  dplyr::select(Plot, Sub_plot, Latitude, Longitude, accuracy_flag, accuracy_code)

input_data_soil05 <- input_data_soil04 %>% 
  dplyr::select(Source, Site_name, Plot, Sub_plot, Habitat_type, Country, Year_collected, 
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, N_perc)


export_data_merged01 <- full_join(input_data_soil05, input_data_location05,  
                                  by = c("Plot", "Sub_plot"))  %>% 
  mutate(DOI = "https://doi.org/10.17528/CIFOR/DATA.00244")

export_data_merged02 <- export_data_merged01 %>% 
  rename(Site = Plot, 
         Plot = Sub_plot) %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data_merged02

write.csv(export_df, export_file)


