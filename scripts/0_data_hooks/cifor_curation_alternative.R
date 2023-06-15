## CIFOR data ingestion - alternative data format 
# contact: Rose Cheney , cheneyr@si.edu

library(dplyr)
library(tidyverse)
library(readxl)
library(parzer)
library(leaflet)
library(lubridate)


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

#additional geographic coordinate sheets 
geo_brazil <- read_xlsx("data/primary_studies/CIFOR/original/Kauffman_2019_BocaGrande_soil/Geographic Coordinate for SWAMP dataset-Mangrove soil carbon-Brazil-2017.xlsx",2)



## 1. Methods ####

#Methods table from recommended methodology taken from Kauffman and Donato 2012, cited by CIFOR database 

methods <- data.frame(study_id = NA, #### TO FIX
                      method_id = "SWAMP",
                      coring_method = "gouge auger",
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_flag = "to constant mass",
                      carbon_measured_or_modeled = "measured",
                      fraction_carbon_method = c("EA", "local regression"),
                      fraction_carbon_type = "total carbon", 
                      carbonates_removed = "FALSE")


## 2. Cores ####

#read in site level coords not included in raw data 
missing_coords <- read_xlsx("./data/primary_studies/CIFOR/missing_site_level_coords.xlsx")


## Assign study id to each dataset - grouped by publication or dataset title if none applicable
cifor_veg <- cifor_veg %>% mutate(study_id = case_when(`Site ID` == "BHI" ~ "Bhomia_et_al_2016",
                                                `Site ID` == "BAO"|`Site ID` == "DIA"|`Site ID` == "MOU"| `Site ID` == "SAN" |
                                                  `Site ID` == "DJI"|`Site ID` == "FAM"|`Site ID` == "CAS"|`Site ID`== "JAR"|
                                                  `Site ID`== "SIM" |`Site ID` == "SIM-D"|`Site ID` == "SOU"| `Site ID`== "SOU-D"|
                                                  `Site ID` == "MWA"| `Site ID` == "MWA-S"| `Site ID`== "NDO"| `Site ID` == "PAG"|
                                                  `Site ID` == "BRM10"|`Site ID` == "MRM8"| `Site ID`== "MRT7"|`Site ID`== "MRT9"|
                                                  `Site ID`== "NCM1"| `Site ID`== "NCM4" |`Site ID` == "NCM5"| `Site ID`== "NCT2"|
                                                  `Site ID`== "NCT3" | `Site ID` == "NCT6" ~ "Kauffman_and_Bhomia_2017",
                                                `Site ID` == "BAR"|`Site ID`== "BOC"|`Site ID` == "CAE" |`Site ID` == "CAET" |
                                                  `Site ID`== "FURO" |`Site ID` == "FUR"|`Site ID`== "MAN"|`Site ID` == "SAL"|
                                                  `Site ID`== "MAR" ~ "Kauffman_et_al_2018",
                                                `Site ID` == "Bintuni"|`Site ID`== "BUN"|`Site ID`== "CIL"|`Site ID` == "KBR"|
                                                  `Site ID` == "SEM"|`Site ID` == "TAN" ~ "Murdiyarso_et_al_2015",
                                                `Site ID` == "CA_"| `Site ID` == "CAN" ~ "Vien_et_al_2016",
                                                `Site ID` == "TIM" |`Site ID` == "DEM" ~ "Ardhani_et_al_2020",
                                                `Site ID` == "Arguni, Kaimana"| `Site ID`== "Buruway, Kaimana"|`Site ID` == "Etna, Kaimana"|
                                                  `Site ID`== "Kaimana City" ~"SWAMP Dataset-Mangrove soil carbon-West Papua-2019",
                                               TRUE ~ filename))

cifor_soil <- cifor_soil %>%  mutate(study_id = case_when(`Site ID` == "BHI" ~ "Bhomia_et_al_2016",
                                                            `Site ID` == "BAO"|`Site ID` == "DIA"|`Site ID` == "MOU"| `Site ID` == "SAN" |
                                                              `Site ID` == "DJI"|`Site ID` == "FAM"|`Site ID` == "CAS"|`Site ID`== "JAR"|
                                                              `Site ID`== "SIM" |`Site ID` == "SIM-D"|`Site ID` == "SOU"| `Site ID`== "SOU-D"|
                                                              `Site ID` == "MWA"| `Site ID` == "MWA-S"| `Site ID`== "NDO"| `Site ID` == "PAG"|
                                                              `Site ID` == "BRM10"|`Site ID` == "MRM8"| `Site ID`== "MRT7"|`Site ID`== "MRT9"|
                                                              `Site ID`== "NCM1"| `Site ID`== "NCM4" |`Site ID` == "NCM5"| `Site ID`== "NCT2"|
                                                              `Site ID`== "NCT3" | `Site ID` == "NCT6" ~ "Kauffman_and_Bhomia_2017",
                                                            `Site ID` == "BAR"|`Site ID`== "BOC"|`Site ID` == "CAE" |`Site ID` == "CAET" |
                                                              `Site ID`== "FURO" |`Site ID` == "FUR"|`Site ID`== "MAN"|`Site ID` == "SAL"|
                                                              `Site ID`== "MAR" ~ "Kauffman_et_al_2018",
                                                            `Site ID` == "Bintuni"|`Site ID`== "BUN"|`Site ID`== "CIL"|`Site ID` == "KBR"|
                                                              `Site ID` == "SEM"|`Site ID` == "TAN" ~ "Murdiyarso_et_al_2015",
                                                            `Site ID` == "CA_"| `Site ID` == "CAN" ~ "Vien_et_al_2016",
                                                            `Site ID` == "TIM" |`Site ID` == "DEM" ~ "Ardhani_et_al_2020",
                                                            `Site ID` == "Arguni, Kaimana"| `Site ID`== "Buruway, Kaimana"|`Site ID` == "Etna, Kaimana"|
                                                              `Site ID`== "Kaimana City" ~"SWAMP Dataset-Mangrove soil carbon-West Papua-2019",
                                                            TRUE ~ filename))

#exclude datasets that only have biomass from cores table 
veg <- cifor_veg %>% filter(`Site ID` %in% c("BHI", "ACA", "BAO", "BOC", "BAR", "BOC", "CAE","CIL","DIA", "FAM", "FUR", "MAN",
                                             "MANG", "MAR", "MOU", "SAL", "SAN", "BUN", "KBR", "TAN", "CA_", "SEM", "TEM"))


#separate all lat long by site - make big table 
latlong.biomass <- veg %>% select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude) %>%
  mutate(Plot = case_when(Plot == "Barreto" ~ "Rio Barreto", 
                          Plot == "Boca Grande" ~ "Boca grande",
                          Plot == "Furo do Chato" ~ "Furo de Chato",
                          Plot == "Moundé" ~ "Mounde",
                          Plot == "Salinas" ~ "Salino",
                          `Site ID` == "BUN" ~ paste("Bunaken", Plot, sep = " "),
                          `Site ID` == "KBR" ~ paste("Kubu Raya", Plot, sep = " "),
                          `Site ID` == "SEM" ~ paste("Sembilang", Plot, sep = " "),
                          `Site ID` == "TAN" ~ paste("Tanjung Putting", Plot, sep = " "), 
                          `Site ID` == "TEM" ~ paste("Teminabuan", Plot, sep = " "),
                          `Site ID` == "CA_" ~ paste("_", Plot, sep = ""),
                          TRUE ~ Plot),
         core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) %>% drop_na()
  
latlong.soil <- cifor_soil %>% select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude) %>% 
  mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) 


latlong.full <- rbind(latlong.soil, latlong.biomass) %>% 
                rename(latitude = Latitude, 
                       longitude = Longitude,
                       site_id = `Site ID`) %>% distinct() %>%
                select(-Plot, -`Sub-plot`) %>% 
                left_join(missing_coords) %>% distinct()
               

#function to join veg and soil dataframes
#plotPositionCores <- function(veg, soil){
 # plot_position <- full_join(veg, soil) %>% 
    #mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"), #create core id 
    #       year = str_sub(filename, -4)) %>% 
   # rename(latitude = Latitude, 
   #       longitude = Longitude,
   #         study_id = filename,
   #         salinity =  `Salinity (ppt)`,
   #       site_id = `Site ID`) %>% 
   # select(-Plot, -`Sub-plot`) %>% distinct()
#}


formatCores <- function(soil){
  soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"), #create core id 
         year = str_sub(filename, -4)) %>% 
    rename(latitude = Latitude, 
           longitude = Longitude,
           site_id = `Site ID`) %>% 
    select(-Plot, -`Sub-plot`) %>% distinct()
}

  

#curate core table 
cifor_cores <- formatCores(cifor_soil) %>% 
  mutate(latitude = case_when(site_id == "DEM" ~ "6.75938056",
                              site_id == "TIM" ~ "6.90485278",
                              TRUE ~ latitude),
         longitude = case_when(site_id == "DEM" ~ "110.54598333",
                               site_id == "TIM" ~ "110.50518611",
                               TRUE ~ longitude)) %>% 
      select(study_id, site_id, core_id, latitude, longitude) %>% distinct()

join_cores <- left_join(cifor_cores, latlong.full, by = c("core_id", "site_id")) %>% 
   mutate(latitude = case_when(is.na(latitude.x) ~ latitude.y, TRUE ~ latitude.x),
         longitude = case_when(is.na(longitude.x) ~ longitude.y, TRUE ~ longitude.x)) %>% 
  select(-ends_with('.x'), -ends_with('.y')) %>% distinct()


#Convert and standardize all coords to lat lon
require(parzer)
subset_1 <- join_cores[grepl("°", join_cores$latitude),] %>% 
  separate(latitude, c(NA, "latitude"), sep= " ") %>% 
  separate(longitude, c(NA, "longitude"), sep =  " ") %>% 
  mutate(latitude = paste(latitude, "N", sep = ""),
         longitude = paste(longitude, "E", sep = "")) %>% 
  mutate(latitude = parzer::parse_lat(latitude),
         longitude = parzer::parse_lon(longitude))

out_1 <- join_cores[!grepl("°", join_cores$latitude),]

cores1 <- rbind(subset_1, out_1)


#convert other format to lat lon and replace 
subset_2 <- cores1 %>% filter(grepl("S", latitude)) %>% 
                  mutate(latitude = parzer::parse_lat(latitude),
                         longitude = parzer::parse_lon(longitude))

out_2 <- cores1 %>% filter(!grepl("S", latitude))

cores2 <- rbind(subset_2, out_2) %>% distinct()


#format cores table and add remaining site level position maunally 
cores <- cores2 %>% mutate(latitude = as.numeric(latitude),
                           longitude = as.numeric(longitude),
                           position_method = "other low resolution",
                          # position_notes = case_when(is.na(latitude) ~ "position at site level",  
                                                     # TRUE ~ "position at subplot level"),
                           habitat = "mangrove",
                           core_length_flag = "not specified") %>%
                    fill(salinity) %>% 
                    mutate(salinity_class = case_when(salinity > 34 ~ "saline",
                                                      salinity < 35 ~ "estuarine",
                                                      TRUE ~ salinity),
                           salinity_method = "measurement") %>% 
                    select(-salinity) %>% distinct()




## 3. Depthseries ####
cifor_depthseries <- cifor_soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"),
                                    fraction_carbon = as.numeric(`Carbon content (%)`),
                                    fraction_carbon = fraction_carbon/100,
                                    method_id = "SWAMP") %>%  
              separate(`Depth interval (cm)`, c("depth_min", "depth_max"), sep = "-") %>% 
              rename(dry_bulk_density = `Bulk density (g/cm3)`,
                     site_id = `Site ID`) %>% 
              select(study_id, core_id, method_id, site_id, fraction_carbon, dry_bulk_density, depth_min, depth_max)

depthseries <- reorderColumns("depthseries", cifor_depthseries) %>% distinct()


## 4. Species #####

#species information comes from tree biomass measurments - should this be included??


## 5. Biomass #####
#curate plot and plant table - structure based on current CCN biomass data structure unified

plot <- cifor_veg %>% select(study_id, `Site ID`, `Data collection date (dd/mm/yyyy)`, Plot, Latitude, Longitude, 
                             `AGC summed per plot (MgC/ha)`, `BGC summed per plot (MgC/ha)`) %>% 
            mutate(position_method = "other low resolution",
                   position_notes = "plot level position",
                   harvest_or_allometry = "allometry") %>% 
            rename(site_id = `Site ID`,
                   plot_id = Plot,
                   latitude = Latitude,
                   longitude = Longitude,
                   plot_sediment_carbon = `BGC summed per plot (MgC/ha)`,
                   plot_biomass_carbon = `AGC summed per plot (MgC/ha)`) %>% distinct()


plant <- cifor_veg %>% select(filename, `Site ID`, `Data collection date (dd/mm/yyyy)`, Latitude, Longitude,
                              Plot,`Sub-plot`, `Species name (scientific)`, `Species name (local)`,
                              `DBH (cm)`, `Status (live/1/2/3)`, `wood density (g/cm3)`,`AGB (Mg/ha)`, `BGB (Mg/ha)`, `source for allometry`,
                              `Sub-plot design`) %>%
                    rename(site_id = `Site ID`,
                           latitude = Latitude,
                           longitude = Longitude,
                           plot_id = Plot,
                           diameter_dbh = `DBH (cm)`,
                           decay_class = `Status (live/1/2/3)`,
                           biomass_aboveground = `AGB (Mg/ha)`,
                           biomass_belowground = `BGB (Mg/ha)`,
                           plot_radius = `Sub-plot design`) %>% #divide diameter by 2 - how to handle the square plots??
            mutate(year = year(`Data collection date (dd/mm/yyyy)`))

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~ core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
#testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder

write_csv(methods, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_methods.csv")
write_csv(sites, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_cores.csv")
write_csv(depthseries, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_depthseries.csv")
#write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
#write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")
write_csv(plot, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plot.csv")
write_csv(plant, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plant.csv")














