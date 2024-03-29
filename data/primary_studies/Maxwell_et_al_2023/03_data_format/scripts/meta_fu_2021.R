## import data from Fu et al 2021, Global Change Biology
## export for marsh soil C 
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 04.07.22
#edit 24.01.23 
# using only reported values (no extrapolated values)

library(tidyverse)
library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date

input_file <- "reports/03_data_format/data/meta_analysis/Fu_2021/Fu_2021_SI_for_reformat.csv"

input_data0 <- read.csv(input_file)

input_file2 <- "reports/03_data_format/data/bind/data_compile.csv"

data_compile <- read.csv(input_file2)


##### format data  #####

input_data0[input_data0 == "nd"] <- NA_real_

source_name <- "Fu et al 2021"
author_initials <- "CF"

input_data1 <- input_data0 %>% 
  slice(1:207) %>% 
  rename(Habitat_type = Type,
         Original_source = Source) %>% 
  mutate_if(is.character, as.factor)


input_data1 <- input_data1 %>% 
  group_by(Habitat_type) %>% 
  mutate(id = row_number()) %>% #id number for each ecosystem type
  ungroup() %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Plot = paste(Habitat_type, id),
         Site_name = paste(Source_abbr, Plot)) 


input_data2 <- input_data1 %>% 
  #create Year_collected column from first 4 values of date 
  mutate(Year_collected = str_sub(input_data1$Sampling_date_year.month.date, 1,4)) %>% 
  mutate(Latitude = as.numeric(as.character(Latitude_N)),
         Longitude = as.numeric(as.character(Longitude_E)),
         Country = "China",
         U_depth_m = 0,
         L_depth_m = 1) %>% 
  mutate(accuracy_code = case_when(is.na(accuracy_flag) ~ NA_real_,
                                   accuracy_flag == "estimated from map" ~ 2,
                                   accuracy_flag == "exact" ~ 1,
                                   accuracy_flag == "no location" ~ NA_real_,
                                   accuracy_flag == "" ~ NA_real_),
         #only including OC reported, NOT extrapolated values
          OC_perc = case_when(is.na(SOC_accuracy) ~ NA_real_, 
                  SOC_accuracy == "reported" ~ as.numeric(as.character(SOC_percent)),
                   SOC_accuracy == "extrapolated" ~ NA_real_),
         #only including BP_reported, NOT BD values calcualted from OC or extrapolated
         BD_reported_g_cm3 = case_when(DBD_accuracy == "reported" ~ as.numeric(as.character(DBD_g_cm3)),
                                 TRUE ~ NA_real_))

input_data2$accuracy_code <-as.factor(input_data2$accuracy_code) 



## filter to salt marsh only
input_data3 <- input_data2 %>% 
  filter(Habitat_type == "Salt marsh") %>% 
  filter(SOC_accuracy == "reported") %>% #no extrapolated values
  filter(accuracy_flag != "no location") %>%  # no studies without locations
  droplevels()
  


##compare to other sources studies

fu_sources <- input_data3 %>% 
  dplyr::group_by(Original_source) %>% 
  dplyr::count()

all_sources <- data_compile %>% 
  dplyr::group_by(Source, Original_source) %>% 
  dplyr::count()

data_test <- inner_join(fu_sources, all_sources, by = "Original_source")

#liu et al 2017 found in both (fu et al 2021 and hu et al 2020) - checking source specifics 
# in fu et al 2021, references not detailed; will remove

input_data4 <- input_data3 %>% 
  filter(Original_source != "Liu et al 2017") 

##### prepare for export  #####

## reformat
export_data01 <- input_data4 %>% 
  select(Source, Site_name, Original_source, Site, Plot, Habitat_type, Latitude, Longitude, accuracy_flag, 
         accuracy_code, Country, Year_collected, U_depth_m, L_depth_m, 
         OC_perc, BD_reported_g_cm3)

export_data02 <- export_data01 %>% 
  relocate(Source, Original_source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1111/gcb.15348")

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


