## import data from Jordi Pagès, unpublished
## from Wales
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.01.23

library(tidyverse)
library(naniar) # replace_with_na
input_file01 <- "reports/03_data_format/data/core_level/Pages_unpublished_email/data_JordiFPages.csv"


input_data01 <- read.csv(input_file01) %>% 
  dplyr::select(1:18)

##### add informational 
source_name <- "Pages unpublished"
author_initials <- "JFP"


input_data02 <- input_data01 %>% 
  mutate(Site_name = paste(author_initials, Core)) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = 1,
         Nation = "Wales") %>% 
  mutate(Method = fct_recode(Method, "EA" = "Elemental analyser"))

### change to decompressed core values

input_data03 <- input_data02 %>% 
  dplyr::rename(U_depth_m_original = U_depth_m,
         L_depth_m_original = L_depth_m) %>% 
  dplyr::rename(U_depth_m = U_deco_depth_m, 
         L_depth_m = L_deco_depth_m) %>% 
  mutate(DOI = "unpublished")


#### outliers ####
#remove values with OC greater than SOM
# note: looking at the data, it seems there is an issue with OC rather than SOM (compared to data around it) 
input_data03$OC_perc[input_data03$OC_perc > input_data03$SOM_perc] <- NA



#check
plot(input_data03$SOM_perc, input_data03$OC_perc)

hist(input_data03$SOM_perc)

#### export ####

export_data01 <- input_data03 %>% 
  dplyr::select(Source,  Site_name, Site, Core, Habitat_type, Country, Nation, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, SOM_perc, BD_reported_g_cm3, DOI)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Nation, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type)


# export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '')
export_df <- export_data02

write.csv(export_df, export_file)


