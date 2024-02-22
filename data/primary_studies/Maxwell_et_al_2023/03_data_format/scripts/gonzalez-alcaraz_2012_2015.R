## import data from two papers from Spain
## Gonzalez-Alcaraz et al 2012, Geoderma El carmoli salt marsh
## Gonzalez-Alcaraz et al 2015, CATENA Antigua Amarga salt marsh
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 18.08.22


library(tidyverse)

#### PAPER 1: Agua Amarga salt marsh from CATENA 2015  ####


input_file01 <- "reports/03_data_format/data/core_level/Gonzalez-Alcaraz_2012_2015_email/Agua_amarga_data.csv"

input_data01 <- read.csv(input_file01) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _


input_file02 <- "reports/03_data_format/data/core_level/Gonzalez-Alcaraz_2012_2015_email/Agua_amarga_locations.csv"

AA_locations <- read.csv(input_file02)

agua_amarga01 <- left_join(input_data01, AA_locations, by = "Sampling_point")


##### add informational  
source_name <- "Gonzalez-Alcaraz et al 2015"
author_initials <- "MNGA"


agua_amarga02 <- agua_amarga01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "Agua Amarga",
         Site_name = paste(Source_abbr, Site, Sampling_point),
         Habitat_type = "Salt marsh",
         Country = "Spain") %>% 
  dplyr::rename(Vegetation = Treatment,
                Core = Sampling_point)

#### reformat data ###

agua_amarga03 <- agua_amarga02 %>% 
  mutate(OC_perc = TOC_g_kg_/10) %>%  #per mille to per cent 
  dplyr::rename(BD_reported_g_cm3 = Bulk_density_g_cm3_) %>% 
  mutate(Year_collected = "2012",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "oxidation with potassium dichromate")


## edit depth

agua_amarga04 <- agua_amarga03 %>%
  mutate(U_depth_cm = 0,
         L_depth_cm = 20) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1016/j.catena.2014.11.012")# cm to m


#### export 

export_AA01 <- agua_amarga04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_reported_g_cm3, DOI)


export_AA02 <- export_AA01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_AA02

write.csv(export_df, export_file)




#### PAPER 2: El Carmoli salt marsh from Geoderman 2012  ####


input_file03 <- "reports/03_data_format/data/core_level/Gonzalez-Alcaraz_2012_2015_email/Carmoli_data.csv"

input_data02 <- read.csv(input_file03) %>% 
  slice(1:18) %>%  # remove na at end
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _

input_data02$Sampling_point <- str_trim(input_data02$Sampling_point)


input_file04 <- "reports/03_data_format/data/core_level/Gonzalez-Alcaraz_2012_2015_email/Carmoli_locations.csv"

C_locations <- read.csv(input_file04)

carmoli00 <- left_join(input_data02, C_locations, by = "Sampling_point")

carmoli00$Bulk_density_g_cm3_ <- as.numeric(carmoli00$Bulk_density_g_cm3_)

#### taking a mean from each of the three seasons

carmoli01 <- carmoli00 %>% 
  group_by(Sampling_point, Replicate, Latitude, Longitude) %>% 
  dplyr::summarise(across(.cols = c(TOC_g_kg_, Bulk_density_g_cm3_), .fns = c(mean, sd),
                          na.rm = T)) %>% 
  ungroup()


##### add informational  
source_name <- "Gonzalez-Alcaraz et al 2012"
author_initials <- "MNGA"


carmoli02 <- carmoli01 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site = "El Carmoli",
         Site_name = paste(Source_abbr, Site, Sampling_point, "Rep", Replicate),
         Habitat_type = "Salt marsh",
         Country = "Spain") %>% 
  mutate(Plot = paste(Sampling_point, "Rep", Replicate))


#### reformat data ###

carmoli03 <- carmoli02 %>% 
  dplyr::rename(TOC_g_kg_mean = TOC_g_kg__1,
         TOC_g_kg_sd = TOC_g_kg__2,
         BD_reported_g_cm3_mean = Bulk_density_g_cm3__1, 
         BD_reported_g_cm3_sd = Bulk_density_g_cm3__2) %>% 
  mutate(OC_perc_mean = TOC_g_kg_mean/10,
         OC_perc_sd = TOC_g_kg_sd/10) %>%  #per mille to per cent 
  mutate(Year_collected = "2005",
         Year_collected_end = "2006",
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

carmoli04 <- carmoli03 %>%
  mutate(U_depth_cm = 0,
         L_depth_cm = 10) %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         DOI = "https://doi.org/10.1016/j.geoderma.2012.03.019")# cm to m


#### export 

export_C01 <- carmoli04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Country, Year_collected, Year_collected_end,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc_mean,OC_perc_sd, BD_reported_g_cm3_mean,BD_reported_g_cm3_sd, DOI)


export_C02 <- export_C01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected,Year_collected_end, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)  

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_C02

write.csv(export_df, export_file)

