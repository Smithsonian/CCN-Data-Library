## import data from Schile et al 2016, Dryad
## from Schile et al 2017, Ecological Applications
## export for marsh soil C (and previously mangrove soil C)
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 04.07.22

library(tidyverse)
library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date
library(janitor) # to clean names
#library(lubridate) #to clean dates - not good for TOC, etc

input_file01 <- "reports/03_data_format/data/core_level/Schile_2016_DRYAD/Schile_2016_DRYAD_plot-info.csv"

input_file02 <- "reports/03_data_format/data/core_level/Schile_2016_DRYAD/Schile_2016_DRYAD_soil-data.csv"


input_data_plot01 <- read.csv(input_file01, header = T)
input_data_soil01 <- read.csv(input_file02)

##### format data  #####

###plot data

input_data_plot01 <- input_data_plot01 %>% 
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%  #replacing . in columns by _
  mutate_if(is.character, as.factor)

# Converting Date to Date format 
input_data_plot02 <- input_data_plot01 %>%
  mutate(Date = lubridate::dmy(Date)) %>% 
  mutate(Year_collected = lubridate::year(Date), #separate Year, Month, Day
         Month = lubridate::month(Date), 
         Day = lubridate::day(Date))

## changing plated mangrove to planted mangrove
levels(input_data_plot02$Ecosystem)[levels(input_data_plot02$Ecosystem)=="plated mangrove"] <- "planted mangrove"


###soil data

input_data_soil02 <- input_data_soil01 %>% 
  slice(1:1198) %>% 
  dplyr::select(c(Site:notes)) %>%   
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%   #replacing . in columns by _
  mutate_if(is.character, as.factor)


#### Cleaning Site names so that data frames match ####


## plot/site data 

# need to use sapply to allow duplicates in factor levels for make_clean_names
input_data_plot02$Site <- sapply(input_data_plot02$Site, make_clean_names,
                                 USE.NAMES = F)
#converting back to factor
input_data_plot02$Site <- as.factor(input_data_plot02$Site)


## soil/ horizon data

input_data_soil02$Site <- sapply(input_data_soil02$Site, make_clean_names,
                                 USE.NAMES = F)
#converting back to factor
input_data_soil02$Site <- as.factor(input_data_soil02$Site)




## comparing which sites were typed in differently 
a <- as.list(levels(input_data_plot02$Site))
b <- as.list(levels(input_data_soil02$Site))

setdiff(a,b)

#comparing lists a and b manually to determine which not match

levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="jubail_is"] <- "jubail_island"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="jubail_is_east"] <- "jubail_island_east"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="khalba_east"] <- "kalba_east"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="khalba_north"] <- "kalba_north"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="khalba_south"] <- "kalba_south"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="khalba_west"] <- "kalba_west"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="marawah_is"] <- "marawah_island"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="rafiq_is"] <- "rafiq_island"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="fasht_al_basm"] <- "fasht_al_bazam"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="thimiriya"] <- "thumayriyah"
levels(input_data_soil02$Site)[levels(input_data_soil02$Site)=="al_zorah"] <- "ajman_al_zorah"



levels(input_data_plot02$Site)[levels(input_data_plot02$Site)=="bu_tinah_jaoub"] <- "bu_tinah_janoub"


## comparing which sites were typed in differently 
c <- as.list(levels(as.factor(input_data_plot02$Site)))
d <- as.list(levels(as.factor(input_data_soil02$Site)))

setdiff(c,d)



##### add informational  #####

source_name <- "Schile et al 2016"
author_initials <- "LS"

##plot data
input_data_plot03 <- input_data_plot02 %>%
  rename(Habitat_type = Ecosystem,
         accuracy_flag = XYZ_source) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Habitat_type, Plot),
         Country = "United Arab Emirates") %>% 
  mutate(accuracy_code = case_when(is.na(accuracy_flag) ~ NA_real_,
                                   accuracy_flag == "no data" ~ NA_real_,
                                   TRUE~ 1))


## soil data
input_data_soil03 <- input_data_soil02 %>%
  rename(Plot = plot, #to match plot data above
         Habitat_type = Ecosystem) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site, Habitat_type, Plot))


#### horizon data ####

input_data_soil04 <- input_data_soil03 %>% 
  separate(depth_cm_, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%  #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,# cm to m
         SOM_perc = X_loss_on_ignition_LOI_, #LOI percent
         OC_perc = X_organic_carbon_OC_, #percent OC
         IC_perc = X_inorganic_carbon_IC_,
         N_perc = X_nitrogen_N_ ,
         BD_reported_g_cm3 = dry_bulk_density_g_cm3_) %>%  #1 g cm-3 = 1 Mg m-3
  mutate(Method = "EA")
  
input_data_soil04$L_depth_m <- round(input_data_soil04$L_depth_m, 2)


##### prepare for export  #####

## reformat

export_data_soil <- input_data_soil04 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, 
         Emirate, U_depth_m, L_depth_m, Method,
         IC_perc, OC_perc, SOM_perc, BD_reported_g_cm3, N_perc)


export_data_plot <- input_data_plot03 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
       accuracy_flag, accuracy_code, Country, Year_collected)


export_data_merged01 <- full_join(export_data_soil, export_data_plot,  
                                by = c("Source", "Site_name", "Site", "Plot",
                                       "Habitat_type"))  

export_data_merged02 <- export_data_merged01 %>% 
  relocate(Source, Site_name, Site, Plot, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Emirate, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.15146/R3K59Z")


sum(is.na(export_data_plot$Latitude))
sum(is.na(export_data_merged01$Latitude))


##export marsh

export_data_marsh <- export_data_merged02 %>% 
  filter(Habitat_type == "salt marsh" ) %>% 
  droplevels()

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data_marsh

write.csv(export_df, export_file)

