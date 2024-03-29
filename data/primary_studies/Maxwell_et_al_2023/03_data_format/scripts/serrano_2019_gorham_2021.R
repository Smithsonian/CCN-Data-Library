## import data from Oscar Serrano
## Gorham 2020, Science of the Total Environment
## and unpublished data
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 25.10.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/core_level/Serrano_2019_Gorham_2021_email/Serrano_data_sent.csv"


input_data01 <- read.csv(input_file01) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _



##### add informational 
input_data02 <- input_data01 %>% 
  mutate(Source = fct_recode(Publication, 
                             "Gorham et al 2021" = "Gorham et al Ecosystems",
                             "Serrano et al 2019" = "Serrano et al 2019 NatComms",
                             "Serrano unpublished" = "unpublished")) %>% 
  mutate(DOI = case_when(Source == "Gorham et al 2021"~ "https://doi.org/10.1007/s10021-020-00520-9",
                         Source == "Serrano et al 2019"~ "https://doi.org/10.1038/s41467-019-12176-8",
                         Source == "Serrano unpublished"~ "unpublished")) %>% 
  mutate(accuracy_code = 1) %>% 
  mutate(author_initials = case_when(Source == "Gorham et al 2021"~ "CG",
                                     Source == "Serrano et al 2019"~ "OS",
                                     Source == "Serrano unpublished"~ "OS")) %>% 
  mutate(Site_name = paste(author_initials, ID_CORE)) %>% 
  dplyr::rename(Core = ID_CORE,
         Habitat_type = Habitat,
         Latitude = lat,
         Longitude = long,
         Year_collected = year_of_sampling)


### editing data a bit more
input_data03 <- input_data02 %>% 
  rename(U_depth_cm = Upper_depth_decompressed_cm_,
         L_depth_cm = Lower_depth_decompressed_cm_,
         OC_perc = X_Corg_bulk, 
         BD_reported_g_cm3 = X_DBD_g_cm_3_decompresed) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Country = "Australia") %>% 
  mutate(Method = "EA")



## edit depth

input_data04 <- input_data03 %>% 
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


##keeping Serrano et al 2019, although previously exported, because now we have raw data
#instead of just calculated stocks
# the published CSIRO dataset has been moved the folder 07_Cam_postdoc/Data/Done


##### adding loi info from Gorham et al paper #####

input_file02 <- "reports/03_data_format/data/core_level/Serrano_2019_Gorham_2021_email/sw_marsh_LOI_fromPaul.csv"
gorham_loi01 <- read.csv(input_file02) %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%    #replacing . in columns by _
  dplyr::select(Core_ID, Sample_ID, X_LOI, X_OC) %>% 
  rename(Core = Core_ID) # to match input_data01

gorham_loi02 <- gorham_loi01 %>% 
  mutate(L_depth_cm = gsub(".*\\_", "", Sample_ID)) %>%  # remove numbers before an underscore 
  mutate(L_depth_cm = as.numeric(L_depth_cm))

str(gorham_loi02)

test <- input_data04 %>% 
  filter(Source == "Gorham et al 2021") %>% 
  select(Core, U_depth_cm, L_depth_cm, OC_perc)

test_bind <- full_join(test, gorham_loi02)
plot(test_bind$X_OC, test_bind$OC_perc) # data are matched


gorham_loi03 <- gorham_loi02 %>% 
  select(Core, L_depth_cm, X_LOI) %>% 
  rename(SOM_perc = X_LOI)

input_data05 <- left_join(input_data04, gorham_loi03, by = c("Core", "L_depth_cm"))


##### editing core locations #####
# email sent 19.07.23 
# After revisiting the dataset and google earth, the coordinates shared initially were slightly off. Below you will find the correct coordinates:
# MCD1: -34.778268° 138.512618°
# MCD2: -34.778287° 138.512621°
# MCD3: -34.778319° 138.512629°

table(input_data05$Source)

test <- input_data05 %>% 
  filter(Core == "MCD1")

input_data06 <- input_data05 %>% 
  mutate(Latitude = case_when(Core == "MCD1" ~ -34.778268,
                              Core == "MCD2" ~ -34.778287,
                              Core == "MCD3" ~ -34.778319,
                              TRUE ~ Latitude),
         Longitude = case_when(Core == "MCD1" ~ 138.512618,
                              Core == "MCD2" ~ 138.512621,
                              Core == "MCD3" ~ 138.512629,
                              TRUE ~ Longitude))


#### export ####

export_data01 <- input_data06 %>% 
  dplyr::select(Source, Site_name, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc,SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Source, Site_name)


plot(export_data02$SOM_perc, export_data02$OC_perc)




## export

path_out = 'reports/03_data_format/data/exported/'
source_name = "Gorham2021 Serrano2019unpublished"

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)
