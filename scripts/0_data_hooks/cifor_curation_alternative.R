## CIFOR data ingestion - alternative data format 
# contact: Rose Cheney , cheneyr@si.edu

library(dplyr)
library(tidyverse)
library(readxl)
library(parzer)
library(leaflet)


#helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC
source("scripts/1_data_formatting/cifor_utility_functions.R") # for CIFOR

#curate alternative format CIFOR data 

#create list/join all 
cifor_soil <- synthCIFOR(data_type = "soil")
#skipped -- files in SWAMP data format and brazil site coordinated lists 

cifor_veg <- synthCIFOR(data_type = "vegetation")
#skips files in SWAMP data format 

#geographic coordinate sheets 
geo_brazil <- read_xlsx("data/primary_studies/CIFOR/original/Kauffman_2019_BocaGrande_soil/Geographic Coordinate for SWAMP dataset-Mangrove soil carbon-Brazil-2017.xlsx",2)


### Issues 

#1. some sites only have soil carbon data 
#2. resolve position issues 


## 1. Methods ####

#one methods table for CIFOR database? 
    #Kauffman and Donato 2011 methodology - search for outliers?
    #study id  == filename 

methods <- data.frame()


## 2. Cores ####

#missing lat long?

#Convert and standardize all coords to decimal degrees 
require(parzer)
subset_dms <- cifor_veg[grepl("°", cifor_veg$Latitude),]
fix_coords <- subset_dms %>% 
  separate(Latitude, c(NA, "Latitude"), sep= " ") %>% 
  separate(Longitude, c(NA, "Longitude"), sep =  " ") %>% 
  mutate(Latitude = paste(Latitude, "N", sep = ""),
         Longitude = paste(Longitude, "E", sep = "")) %>% 
  mutate(Latitude = parzer::parse_lat(Latitude),
         Longitude = parzer::parse_lon(Longitude))

#add converted positions back to tbl
subset_dms_out <- cifor_veg %>% filter(!grepl('°', Latitude))
veg <- rbind(subset_dms_out, fix_coords) 
        mutate(Latitude = case_when(grepl("S", Latitude) ~ as.character(parzer::parse_lat(Latitude))),
               TRUE ~ Latitude)


#add brazil position data
#mariupe, marisma missing?
brazil <- geo_brazil %>% mutate(`Site ID` = case_when(Site == "Boca Grande" ~ "BOC",
                                                      Site == "Mangue Sul" ~ "MAN",
                                                      Site == "Salina" ~ "SAL",
                                                      Site == "Baretto" ~ "BAR",
                                                      Site == "Furo Grande" ~ "FURO",
                                                      Site == "Maruipe" ~ "MAR")) %>% 
                                filter(!is.na(`Site ID`)) %>% select(-No, -Site) %>% 
                                mutate(Latitude = as.character(parzer::parse_lat(Latitude)),
                                       Longitude = as.character(parzer::parse_lon(Longitude)))

add_brazil <- left_join(veg, brazil)

  
#function to pull position from biomass table 
plot_position_cores <- function(veg, soil){
  plot_position <- full_join(veg, soil) %>% 
    mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"), #create core id 
           position_method = "other low resolution",) %>% 
    rename(latitude = Latitude, 
           longitude = Longitude,
           study_id = filename,
           site_id = `Site ID`) %>% 
    select(-Plot, -`Sub-plot`) %>% distinct()
}


#curate core table 
#add site location for missing plot lat long 
cores <- plot_position_cores(cifor_soil, add_brazil) %>% 
    left_join(fix_coords) %>% 
  #  full_join(brazil_cores) %>% 
    mutate(position_method = "",
           latitude = case_when(study_id == "SWAMP Data-Soil carbon-Berahan kulon-2019" ~ "6.75938056",
                                study_id == "SWAMP Data-Soil carbon-Timbulsloko-2019" ~ "6.90485278",
                                TRUE ~ latitude),
           longitude = case_when(study_id == "SWAMP Data-Soil carbon-Berahan kulon-2019" ~ "110.54598333",
                                 study_id == "SWAMP Data-Soil carbon-Timbulsloko-2019" ~ "110.50518611",
                                 TRUE ~ longitude)) %>% 
    select(study_id, site_id, core_id, latitude, longitude) %>% distinct()
  


## 3. Depthseries ####
cifor_depthseries <- cifor_soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"),
                                   fraction_carbon = as.numeric(`Carbon content (%)`),
                                   fraction_carbon = fraction_carbon/100,
                                   method_id = "SWAMP") %>%  ## to fix ?
              separate(`Depth interval (cm)`, c("depth_min", "depth_max")) %>% 
              rename(dry_bulk_density = `Bulk density (g/cm3)`,
                     site_id = `Site ID`,
                     study_id = filename) %>% 
              select(study_id, core_id, method_id, site_id, fraction_carbon, dry_bulk_density)

depthseries <- reorderColumns("depthseries", cifor_depthseries) %>% distinct()




## Mapping check 
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3)
















