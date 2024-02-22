## import data from Miller et al 2022, Marine Scotland dataset
## from Scottish soils (narrow vs wide sampling)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 11.08.22

# Jaxine Wolfe revised 21.02.24
# retain mudflat habitat and delta_c13

library(tidyverse)


input_file01 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Miller_2022_Marine_Scotland/Physical_geochemical_properties_narrow_cores.csv"

input_data_narrow <- read.csv(input_file01)

narrow_cores <- input_data_narrow %>% 
  mutate(Core_type = "Narrow")



input_file02 <- "data/primary_studies/Maxwell_et_al_2023/03_data_format/data/core_level/Miller_2022_Marine_Scotland/Physical_geochemical_properties_wide_cores.csv"

input_data_wide <- read.csv(input_file02)

wide_cores <- input_data_wide %>% 
  mutate(Core_type = "Wide") %>% 
  rename(Sample_depth_cm = Sample_depth_interval_cm, #to match the narrow core dataset
         Mid_Point_depth_cm = Mid_point_depth_cm,
         Dry_bulk_density_g_cm_3 = Dry_Bulk_Density_g_cm_3)


## merge datasets
input_data01 <- rbind(narrow_cores, wide_cores)

# input_data02 <- input_data01 %>% 
#   filter(Marsh_zone != "Mudflat" )


##### add informational  
source_name <- "Miller et al 2022"
author_initials <- "LCM"



input_data02 <- input_data01 %>%
  rename(State = Local_authority,
         Site = Marsh_ID,
         Core = Core_ID) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core),
         Habitat_type = ifelse(Marsh_zone == "Mudflat", "Mudflat", "Salt marsh"),
         Country = "UK") 


#### reformat data ####

input_data03 <- input_data02 %>% 
  rename(Latitude = Lat_dec_deg,
         Longitude = Long_dec_deg,
         Year_collected = Sampling_year,
         delta_c13 = delta_13Corg_per_mil,
         BD_reported_g_cm3 = Dry_bulk_density_g_cm_3) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA",
         DOI = "https://doi.org/10.7489/12422-1")


## edit depth

input_data04 <- input_data03 %>% 
  filter(Sample_depth_cm != "Surface") %>% 
  separate(Sample_depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_|-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100) %>%  # cm to m
  # there were a few duplicate rows
  distinct()
  # add_count(Site, Core, U_depth_m, L_depth_m) %>% filter(n > 1)


#### export ####

export_data01 <- input_data04 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, State, Year_collected, Core_type,
                Latitude, Longitude, accuracy_flag, accuracy_code, Marsh_type, Marsh_zone, delta_c13,
                U_depth_m, L_depth_m, Method, OC_perc, N_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, State, Year_collected,
           .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)

## export

path_out = 'data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write_csv(export_df, export_file)




