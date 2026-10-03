## CIFOR data ingestion - alternative data format 
# contact: Rose Cheney , cheneyr@si.edu

library(dplyr)
library(tidyverse)
library(readxl)
library(parzer)
library(leaflet)
library(lubridate)
library(RefManageR)


#helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC
source("scripts/1_data_formatting/cifor_utility_functions.R") # for CIFOR

#curate alternative format CIFOR data 
#skipped -- files in SWAMP data format

#create list/join all 
cifor_soil <- synthCIFOR(data_type = "soil") #sediment
cifor_veg <- synthCIFOR(data_type = "vegetation") #biomass and necromass 

#missing geographic coordinate sheets (brazil sites)
geo_brazil <- read_xlsx("data/primary_studies/CIFOR/original/Kauffman_2019_BocaGrande_soil/Geographic Coordinate for SWAMP dataset-Mangrove soil carbon-Brazil-2017.xlsx",2)
geo_brazil <- geo_brazil %>% dplyr::rename(latitude = Latitude,
                                    longitude = Longitude) %>% select(-No) %>% 
                             mutate(latitude = parse_lat(latitude),
                                    longitude = parse_lon(longitude),
                                    site_id = case_when(Site== "Barreto" ~ "BAR",
                                                        Site == "Boca Grande" ~ "BOC",
                                                        Site == "Caete" ~ "CAET",
                                                        Site == "Furo Do Chato" ~ "FUR",
                                                        Site == "Furo Grande" ~ "FURO",
                                                        Site == "Mangue Sul" ~ "MAN",
                                                        Site == "Maruipe" ~ "MAR",
                                                        Site == "Sa˜o Caetano" ~ "CAE",
                                                        Site == 'Salina' ~ "SAL",
                                                        TRUE ~ "MAR"),
                                    Site = case_when(Site == "Marisma high" ~ "Marisma High",
                                                     Site == "Marisma medium" ~ "Marisma Medium",
                                                     Site == "Marisma low" ~ "Marisma Low",
                                                     TRUE ~ Site)) %>% distinct()


## Depthseries ####


#create depth min and max columns based on formatting of `Depth Interval`, 3 different formatting types
cifor_depthseries <- cifor_soil %>%  
  filter(grepl("-",`Depth interval (cm)`)) %>% 
  separate(`Depth interval (cm)`, c("depth_min", "depth_max"), sep = "-", remove = FALSE) %>% 
  mutate(depth_max = str_remove_all(depth_max, "\\*|\\ |\\(|\\)")) %>% 
  mutate(depth_min = as.numeric(depth_min),
         depth_max = as.numeric(depth_max),
         depth_interval_notes = NA)

cifor_depthseries3 <- cifor_soil %>% 
  filter(!is.na(`Collected sample depth range (cm)`)) %>% 
  separate(`Collected sample depth range (cm)`, c("depth_min", "depth_max"), sep = "-", remove = FALSE) %>% 
  mutate(depth_interval_notes = ifelse(is.na(depth_min), "Reported depth interval categorized as depth_max", NA),
         depth_min = case_when(depth_min == "270" ~100, 
                               depth_min == "125" ~ 100,
                               depth_min == "170" ~ 100,
                               TRUE ~ as.numeric(depth_min)),
         depth_max = case_when(`Collected sample depth range (cm)` == "270" ~ 270, 
                               `Collected sample depth range (cm)` == "125" ~ 125,
                               `Collected sample depth range (cm)` == "170" ~ 170,
                               TRUE ~ as.numeric(depth_max))) %>% 
  drop_na(depth_min) #remove 1 row of "woody debris" data, no core or depth interval


cifor_depthseries2 <- cifor_soil %>% 
  filter(!grepl("-",`Depth interval (cm)`)) %>% filter(is.na(`Collected sample depth range (cm)`)) %>% 
  mutate(`Depth interval (cm)` = str_remove_all(`Depth interval (cm)`, ">"),
         depth_interval_notes = "Recorded depth interval categorized as depth_max", 
         depth_max = as.numeric(`Depth interval (cm)`),
         depth_min = case_when(depth_max == 100 ~ 50,
                               depth_max == 300 ~ 100,
                               depth_max == 270 ~ 100,
                               TRUE ~ depth_max - 50))

#join
depthseries_join <- rbind(cifor_depthseries, cifor_depthseries2, cifor_depthseries3) 

#create core id and standardize variable names
depthseries_core_id <- depthseries_join %>% 
  mutate(subsite_code = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) %>% 
  group_by(`Site ID`, Plot, `Sub-plot`, depth_min) %>% 
  mutate(n_depths = n(),
         core_replicate = 1:n()) %>% 
  ungroup() %>% 
  mutate(core_id = paste(`Site ID`, Plot,`Sub-plot`, core_replicate, sep = "_"),
         fraction_carbon = as.numeric(`Carbon content (%)`),
         fraction_carbon = fraction_carbon/100,
         method_id = "CIFOR",
         dry_bulk_density = as.numeric(`Bulk density (g/cm3)`),
         depth_interval_notes = ifelse(`Depth interval (cm)` == ">100", "depth interval categorized as >100", depth_interval_notes),
         depth_min = ifelse(`Site ID` == "Arguni, Kaimana" & is.na(depth_min), 100, depth_min),
         depth_max = ifelse(`Site ID` == "Arguni, Kaimana" & is.na(depth_max), 300, depth_max),
         core_id = str_remove_all(core_id, "_NA")) %>%
  rename(study_id = filename) %>%
  select(study_id, core_id, method_id, `Site ID`, fraction_carbon, dry_bulk_density, 
         depth_min, depth_max, depth_interval_notes, Plot, `Sub-plot`) 

#spot fix depth interval error 
depthseries <- depthseries_core_id %>% 
  mutate(depth_min = ifelse(depth_min == 1183, 183, depth_min),
         depth_max = ifelse(depth_max == 1233, 233, depth_max),
         site_id = `Site ID`) %>% 
  select(-Plot, -`Sub-plot`, -`Site ID`)

#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


##  Cores ####

#join core id by site, plot, subplot 
core_id <- depthseries_core_id %>% 
  select(core_id, Plot, `Sub-plot`, `Site ID`) %>% distinct()
  
cifor_soil <- left_join(cifor_soil, core_id, by = c("Site ID", "Plot", "Sub-plot"))


#get any missing plot level position data from vegetation table 
latlong.biomass <- cifor_veg %>% 
  select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude, filename) %>%
  filter(`Site ID` != "FUR") %>% filter(`Site ID` != "FURO") %>% 
  mutate(Plot = case_when(Plot == "Barreto" ~ "Rio Barreto", 
                          Plot == "Boca Grande" ~ "Boca grande",
                          Plot == "Furo do Chato" ~ "Furo de Chato",
                          Plot == "Moundé" ~ "Mounde",
                          Plot == "Salinas" ~ "Salino",
                          Plot == "Manguezal Caussau" ~ "Mangizal Cauassu",
                          `Site ID` == "BUN" ~ paste("Bunaken", Plot, sep = " "),
                          `Site ID` == "KBR" ~ paste("Kubu Raya", Plot, sep = " "),
                          `Site ID` == "SEM" ~ paste("Sembilang", Plot, sep = " "),
                          `Site ID` == "TAN" ~ paste("Tanjung Putting", Plot, sep = " "), 
                          `Site ID` == "TEM" ~ paste("Teminabuan", Plot, sep = " "),
                          `Site ID` == "CA_" ~ paste("_", Plot, sep = ""),
                          TRUE ~ Plot)) %>% drop_na() %>%
  select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude)
  
#start cores table with position data pulled from soils table// create core id 
latlong.soil <- cifor_soil %>% 
  select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude, filename, core_id) %>% distinct()


#join to fill in position data from veg
latlong.full <- left_join(latlong.soil, latlong.biomass, by = c("Site ID", "Plot", "Sub-plot")) %>% 
  mutate(Latitude = if_else(is.na(Latitude.x), Latitude.y, Latitude.x),
         Longitude = if_else(is.na(Longitude.x), Longitude.y, Longitude.x)) %>% 
                rename(latitude = Latitude, 
                       longitude = Longitude,
                       site_id = `Site ID`) %>% 
                select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y) %>% distinct()



                  #formatting function
                  #formatCores <- function(soil){
                   # soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) %>% #pull year from study id? 
                      #          rename(latitude = Latitude, 
                      #                 longitude = Longitude,
                       #                site_id = `Site ID`) %>% 
                       #         select(-Plot, -`Sub-plot`) %>% distinct()
                  #}

cifor_cores <- latlong.full %>% 
  mutate(latitude = case_when(site_id == "DEM" ~ "-6.75938056",
                              site_id == "TIM" ~ "-6.90485278",
                              site_id == "DJI" ~ "13.97611667",
                              site_id == "FURO" ~ "S00 50.480",
                              TRUE ~ latitude),
         longitude = case_when(site_id == "DEM" ~ "110.54598333",
                               site_id == "TIM" ~ "110.50518611",
                               site_id == "DJI" ~ "-16.61556667",
                               site_id == "FURO" ~ "W046 38.316",
                               TRUE ~ longitude)) %>% 
      select(filename, site_id,latitude, longitude, Plot, `Sub-plot`, core_id) %>% distinct()



#Convert and standardize all coords to lat long
  #extract each format, parse lat long, join 
require(parzer)
subset_1 <- cifor_cores[grepl("°", cifor_cores$latitude),] %>% 
  separate(latitude, c(NA, "latitude"), sep= " ") %>% 
  separate(longitude, c(NA, "longitude"), sep =  " ") %>% 
  mutate(latitude = paste(latitude, "N", sep = ""),
         longitude = paste(longitude, "E", sep = "")) %>% 
  mutate(latitude = parzer::parse_lat(latitude),
         longitude = parzer::parse_lon(longitude))

out_1 <- cifor_cores[!grepl("°", cifor_cores$latitude),]

cores1 <- rbind(subset_1, out_1) %>% fill()


#convert other format to lat lon and replace 
subset_2 <- cores1 %>% filter(grepl("S", latitude)) %>% 
                  mutate(latitude = parzer::parse_lat(latitude),
                         longitude = parzer::parse_lon(longitude))

out_2 <- cores1 %>% filter(!grepl("S", latitude))

cores2 <- rbind(subset_2, out_2) %>% distinct()


#format cores table and add remaining site level position maunally 
cores_format <- cores2 %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         position_method = "other low resolution",
         position_notes = case_when(is.na(latitude) ~ "position at site level", 
                                    site_id == "DEM"|site_id == "TIM"|site_id == "FURO"|site_id == "DJI" ~ "position at site level",
                        TRUE ~ "position at subplot level"),
         habitat = "mangrove",
         core_length_flag = "not specified") %>%
  rename(study_id = filename) %>% 
  select(-Plot, -`Sub-plot`) %>% distinct()

##add brazil position data and create matching core_id to merge 
geo_brazil_filter <- geo_brazil %>% 
  filter(!Site == "Maruipe") %>% filter(!site_id == "BAR") %>% filter(!site_id == "CAE") %>% 
  filter(!site_id == "BOC") %>% filter(!site_id == "FURO") %>% filter(!site_id == "FURO") %>% 
  filter(!site_id == "MAN") %>% filter(!site_id == "SAL") %>% 
  mutate(subsite_code = paste(site_id, Site,`Sub-Plot`, sep = "_")) %>% 
  group_by(site_id, Site, `Sub-Plot`, latitude, longitude) %>% 
  mutate(core_replicate = 1:n()) %>% 
  ungroup() %>% 
  mutate(core_id = case_when(site_id == "CAET" ~ paste(site_id, "Caete",`Sub-Plot`, core_replicate, sep = "_"),
                             site_id == "MAR" ~ paste(site_id, Site,`Sub-Plot`, core_replicate, sep = "_"),
                             site_id == "FUR" ~ paste(site_id, "Furo de Chato",`Sub-Plot`, core_replicate, sep = "_"))) %>% 
  select(site_id, core_id, latitude, longitude) %>% distinct()



cores_join <- left_join(cores_format, geo_brazil_filter, by = c("site_id", "core_id")) %>% 
  mutate(latitude = if_else(is.na(latitude.x), latitude.y, latitude.x),
         longitude = if_else(is.na(longitude.x), longitude.y, longitude.x)) %>% 
  select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)
  

## add missing lat long, taken from CIFOR metadata 
missing_site_position <- read_xlsx("data/primary_studies/CIFOR/missing_site_position.xlsx") %>% 
  mutate(latitude = parse_lat(latitude),
         longitude = parse_lon(longitude))

cores <- cores_join %>% 
  mutate(latitude = case_when(site_id == "BUN" ~ 1.694528,
                              site_id == "MANG" ~ -4.448883333,
                              site_id == "CA_" ~ 8.64705,
                              site_id == "CAN" ~ 10.48330,
                              site_id == "POR_C" ~ -4.53835,
                              site_id == "POR_CT" ~ -4.539933333,
                              is.na(latitude)|core_id == "BHI_BK1A_1_1" & site_id == "BHI" ~ 20.48278,
                              TRUE ~ latitude),
         longitude = case_when(site_id == "BUN" ~ 124.803806,
                              site_id == "MANG" ~ -37.78628333,
                              site_id == "CA_" ~ 105.11574,
                              site_id == "CAN" ~ 106.94244,
                              site_id == "POR_C" ~ -37.78106667,
                              site_id == "POR_CT" ~ -37.78308333,
                              is.na(longitude)|core_id == "BHI_BK1A_1_1" & site_id == "BHI" ~ 86.69051,
                              TRUE ~ longitude)) %>% 
  left_join(missing_site_position, by = "site_id") %>% 
  mutate(latitude = if_else(is.na(latitude.x), latitude.y, latitude.x),
         longitude = if_else(is.na(longitude.x), longitude.y, longitude.x),
         core_id = str_remove_all(core_id, "_NA"),
         position_notes = ifelse(core_id == "BHI_BK1A_1_1", "position at site level", position_notes)) %>% 
  select(-latitude.x, -latitude.y, -longitude.x, -longitude.y) %>% distinct()

##extract year 
cores <- cores %>% 
  mutate(year = case_when(endsWith(study_id, "2017-Brazil") ~ 2017,
                          endsWith(study_id, "2019") ~ 2019,
                          endsWith(study_id, "2014")| endsWith(study_id, "Senegal") ~ 2014,
                          endsWith(study_id, "India") ~ 2013,
                          endsWith(study_id, "2011")|endsWith(study_id, "2011-Indonesia") ~ 2011,
                          endsWith(study_id, "Vietnam") ~ 2012,
                          endsWith(study_id, "2016-Brazil") ~ 2016,
                          endsWith(study_id, "2017-Indonesia") ~ 2017,
                          endsWith(study_id, "2016-Indonesia") ~ 2016,
                          endsWith(study_id, "2015-Indonesia") ~ 2015,
                          endsWith(study_id, "2018-Indonesia") ~ 2018,
                          site_id == "TAN" ~ 2009))

cores <- reorderColumns("cores", cores) 


## 5. Biomass #####
#curate plot_summary, plant_plot_detail, and plant table - structure based on current draft biomass structure 7/24

#extract dates, multiple formats
curate_veg <- cifor_veg %>% 
  janitor::remove_empty(which = "cols") %>% 
  rename(study_id = filename,
         site_id = `Site ID`) %>% 
  mutate(year = case_when(grepl("2013",`Data collection date (dd/mm/yyyy)`) ~ 2013,
                          grepl("2014",`Data collection date (dd/mm/yyyy)`) ~ 2014,
                          grepl("2012",`Data collection date (dd/mm/yyyy)`) ~ 2012,
                          grepl("2015",`Data collection date (dd/mm/yyyy)`) ~ 2015,
                          grepl("2016",`Data collection date (dd/mm/yyyy)`) ~ 2016,
                          grepl("2017",`Data collection date (dd/mm/yyyy)`) ~ 2017,
                          grepl("2018", `Data collection date (dd/mm/yyyy)`) ~ 2018,
                          grepl("2019", `Data collection date (dd/mm/yyyy)`) ~ 2019,
                          grepl("2009", `Data collection date (dd/mm/yyyy)`) ~ 2009,
                          grepl("2011", `Data collection date (dd/mm/yyyy)`) ~ 2011),
         month = case_when(grepl("-05-",`Data collection date (dd/mm/yyyy)`) ~ 05,
                           grepl("-02-",`Data collection date (dd/mm/yyyy)`) ~ 02,
                           grepl("/02",`Data collection date (dd/mm/yyyy)`) ~ 02,
                           grepl("-03-",`Data collection date (dd/mm/yyyy)`) ~ 03,
                           grepl("-09-",`Data collection date (dd/mm/yyyy)`) ~ 09,
                           grepl("-07-",`Data collection date (dd/mm/yyyy)`) ~ 07,
                           grepl("-06-", `Data collection date (dd/mm/yyyy)`) ~ 06,
                           grepl("-11-", `Data collection date (dd/mm/yyyy)`) ~ 11,
                           TRUE ~ NA),
         day = case_when(grepl("-18",`Data collection date (dd/mm/yyyy)`) ~ 18,
                         grepl("-20",`Data collection date (dd/mm/yyyy)`) ~ 20,
                         grepl("22/",`Data collection date (dd/mm/yyyy)`) ~ 22,
                         grepl("-22",`Data collection date (dd/mm/yyyy)`) ~ 22,
                         grepl("-06",`Data collection date (dd/mm/yyyy)`) ~ 06,
                         grepl("-15",`Data collection date (dd/mm/yyyy)`) ~ 15,
                         grepl("-19",`Data collection date (dd/mm/yyyy)`) ~ 19,
                         grepl("19/",`Data collection date (dd/mm/yyyy)`) ~ 19,
                         grepl("20/",`Data collection date (dd/mm/yyyy)`) ~ 20,
                         grepl("-07",`Data collection date (dd/mm/yyyy)`) ~ 07,
                         grepl("-21",`Data collection date (dd/mm/yyyy)`) ~ 21,
                         grepl("-11",`Data collection date (dd/mm/yyyy)`) ~ 11,
                         grepl("-16",`Data collection date (dd/mm/yyyy)`) ~ 16,
                         grepl("-31",`Data collection date (dd/mm/yyyy)`) ~ 31,
                         grepl("-17",`Data collection date (dd/mm/yyyy)`) ~ 17,
                         grepl("-10",`Data collection date (dd/mm/yyyy)`) ~ 10,
                         grepl("-14",`Data collection date (dd/mm/yyyy)`) ~ 14,
                         grepl("-26",`Data collection date (dd/mm/yyyy)`) ~ 26,
                         grepl("-29",`Data collection date (dd/mm/yyyy)`) ~ 29,
                         grepl("-23",`Data collection date (dd/mm/yyyy)`) ~ 23,
                         grepl("-24",`Data collection date (dd/mm/yyyy)`) ~ 24,
                         grepl("-04",`Data collection date (dd/mm/yyyy)`) ~ 04,
                         grepl("-31",`Data collection date (dd/mm/yyyy)`) ~ 31,
                         grepl("-28",`Data collection date (dd/mm/yyyy)`) ~ 28,
                         grepl("24/",`Data collection date (dd/mm/yyyy)`) ~ 24,
                         grepl("25/",`Data collection date (dd/mm/yyyy)`) ~ 25,
                         grepl("21/",`Data collection date (dd/mm/yyyy)`) ~ 21,
                         grepl("-21",`Data collection date (dd/mm/yyyy)`) ~ 21,
                         grepl("-13",`Data collection date (dd/mm/yyyy)`) ~ 13,
                         grepl("-27",`Data collection date (dd/mm/yyyy)`) ~ 27,
                         TRUE ~ NA))
  
#pull site locations not included in cifor_veg 
site_locations <- latlong.full %>% 
  rename(plot_id = Plot) %>% 
  mutate(sub_plot_id = paste0(plot_id, "_", `Sub-plot`),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  select(site_id, plot_id, sub_plot_id, latitude, longitude) %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))


#curate plot summary table 
plot_summary <- curate_veg %>%  
            mutate(position_method = "other low resolution",
                   position_notes = "plot level position",
                   harvest_or_allometry = "allometry",
                   plot_id = paste0(site_id, "_", Plot),
                   plot_shape = "circular",
                   plot_radius = 3.5,
                   plant_plot_detail_present = "yes",
                   coordinate_obscured_flag = "no obscuring",
                   field_or_manipulation_code = "field",
                   habitat = "mangrove",
                   basal_area_unit = ifelse(!is.na(`Basal area (m2/ha) summed per plot`), "m2/ha", NA),
                   stand_age = case_when(grepl("5 years", notes) ~ 5,
                                         grepl("50 year old", notes) ~ 50,
                                         grepl("25 years old", notes) ~ 25,
                                         grepl("10 years old", notes) ~ 10,
                                         notes == "Regenerated forest (15 years old)" ~ 15,
                                         TRUE ~ NA)) %>% 
             #'soil_core_present' --> if matching study id in 'cores'
            rename(aboveground_biomass = `AGB summed per plot (Mg/ha)`,
                   belowground_biomass = `BGB summed per plot (Mg/ha)`,
                   aboveground_carbon = `AGC summed per plot (MgC/ha)`,
                   belowground_carbon = `BGC summed per plot (MgC/ha)`,
                   basal_area = `Basal area (m2/ha) summed per plot`) %>% 
  select(study_id, site_id, plot_id, plot_shape, plot_radius, coordinate_obscured_flag, year, 
         month, day, stand_age, field_or_manipulation_code, habitat, plant_plot_detail_present, aboveground_biomass,
         belowground_biomass, aboveground_carbon, belowground_carbon, basal_area, basal_area_unit,
         plant_plot_detail_present) %>% distinct() 

#curate plant plot detail 
plant_plot_detail <- curate_veg %>%
  rename(species = `Species name (scientific)`,
         sub_plot_area = `Sub-plot area (ha)`,
         longitude = Longitude,
         latitude = Latitude) %>% 
  separate(species, into = c("genus", "species"), sep = " ") %>% #species not categorized per sub-plot, included in `plant`
  mutate(plot_id = paste0(site_id, "_", Plot),
         plot_shape = "circular",
         plot_radius = 3.5,
         area_unit = "m",
         alive_or_dead = ifelse(`Status (live/1/2/3)` == "live", "live", "dead"),
         harvest_or_allometry = "allometry",
         sub_plot_id = paste0(plot_id, "_", `Sub-plot`),
         allometric_eq_present = "yes",
         plant_measurements_present = "yes") %>% 
  select(study_id, site_id, plot_id, sub_plot_id, sub_plot_area, area_unit, longitude, latitude,
         year, month, day, genus, species, harvest_or_allometry, alive_or_dead, allometric_eq_present, plant_measurements_present) %>% distinct() 


#curate plant 
plant_curate <- curate_veg %>%
  janitor::remove_empty(which = "cols") %>% 
  rename(latitude = Latitude,
         longitude = Longitude,
         diameter = `DBH (cm)`,
         sub_plot_area = `Sub-plot area (ha)`,
         decay_class = `Status (live/1/2/3)`,
         wood_mass = `Wood Mass AG (kg)`,
         wood_density = `wood density (g/cm3)`,
         wood_density_source = `source for density`,
         allometric_eq_id = `source for allometry`,
         aboveground_biomass = `Total AGB (kg)`, 
         root_mass = `BG root mass (kg)`) %>% 
  separate(`Species name (scientific)`, into = c("genus", "species"), sep = " ") %>% 
  mutate(plot_id = paste0(site_id, "_", Plot),
         sub_plot_id = paste0(plot_id, "_", `Sub-plot`),
         basal_area = ifelse(!is.na(`Basal area per ha (m2)`), `Basal area per ha (m2)`, `Basal area per ha`),
         basal_area_unit = ifelse(!is.na(basal_area), "m2", NA),
         decay_class = case_when(decay_class == "live" ~ NA,
                                 decay_class == "L" ~ NA,
                                 TRUE ~ decay_class),
         diameter_method = "dbh",
         diameter_unit = "cm",
         wood_density_unit = "g/cm3",
         alive_or_dead = ifelse(is.na(decay_class), "live", "dead")) %>%
  group_by(site_id, plot_id, sub_plot_id) %>% 
  mutate(plant_rep = 1:n()) %>% 
  ungroup() %>% 
  mutate(plant_id = paste0(sub_plot_id, "_", plant_rep),
         n_plants = 1) %>% # each measurement represents 1 plant 
  select(-`Data collection date (dd/mm/yyyy)`, -`No ID`, -`Sub-plot`, -`AGB summed per plot (Mg/ha)`,
         -`BGB summed per plot (Mg/ha)`, -`AGC summed per plot (MgC/ha)`, -`BGC summed per plot (MgC/ha)`, -plant_rep,
         -`Basal area (m2/ha) summed per plot`, -notes, -`Sub-plot design`, -Macroscale_Typology, -Hydroperiod_Typology)

#spot fix species name --> correcting spelling and fill in genus names 
plant <- plant_curate %>% 
  mutate(allometric_eq_present = ifelse(!is.na(allometric_eq_id), "yes", "no"),
         genus = case_when(grepl("marina", species) ~ "Avicennia",
                           grepl("agolocha", species) & genus == "A" ~ "Aquilaria",
                           grepl("agolocha", species) & genus == "E" ~ "Excoecaria",
                           grepl("alba", species) ~ "Avicennia",
                           grepl("apiculata", species) ~ "Rhizophora", 
                           grepl("conniculatum", species) ~ "Aegiceras", #spell check 
                           grepl("agalorcha", species) ~ "Aquilaria",
                           grepl("trifoliata", species) |grepl("trifuliata", species) ~ "Derris",
                           grepl("spinosa", species) ~ "Delbergia",
                           grepl("decandra", species) ~ "Ceriops",
                           grepl("Achanhus", genus) ~ "Acanthus",
                           grepl("Hibicus", genus) ~ "Hibiscus",
                           grepl("agalocha", species) ~ "Excoecaria",
                           grepl("A", genus) & grepl("cormiculatum", species) ~ "Aegiceras",
                           genus == "Unidentified"| genus == "Unrecorded" ~ NA,
                           TRUE ~ genus),
         species = case_when(grepl("officinalis", species) ~ "officinalis",
                             grepl("ilicifolius", species) ~ "ilicifolius",
                             grepl("decandra", species) ~ "decandra",
                             grepl("alba", species) ~ "alba",
                             grepl("cormiculatum", species) ~ "corniculatum",
                             grepl("trifuliata", species) ~ "trifoliata",
                             TRUE ~ species)) %>% 
  select(study_id, site_id, plot_id, sub_plot_id, plant_id, year, month, day, genus, species, alive_or_dead, n_plants,
         diameter, diameter_method, diameter_unit, basal_area, basal_area_unit, wood_density, wood_density_unit,
         wood_density_source, wood_mass, allometric_eq_id, allometric_eq_present)

#list(unique(plant$genus))

## allometric_eq table 
allometric_eq <- plant %>% 
  select(study_id, allometric_eq_id, genus, species, alive_or_dead) %>% distinct() %>% 
  filter(!is.na(allometric_eq_id))


#Allometric equations cited- 
  # Komiyama et al. 2005
  # Kauffman and Donato 2012
  # Kauffman and Cole 2012
  # Ong et al 2004
  # Comley and McGuinness 2005
  # Tarlan 2005
  # "using equations of Fromard for AG and Rm and those of Smith and Whelan for LR"
  # Komiyama et al. 2010
  # Clough and Scott 1989
  # Poungparn et al. 2002
  # Tamai et al. 1986


  
##. Methods ####

#Methods table from recommended methodology taken from Kauffman and Donato 2012, cited by CIFOR database 
methods <- cores %>% select(study_id) %>% distinct() %>% 
  data.frame(method_id = "CIFOR",
             coring_method = "gouge auger",
             roots_flag = "roots and rhizomes included",
             sediment_sieved_flag = "sediment not sieved",
             compaction_flag = "not specified",
             dry_bulk_density_temperature = 60,
             dry_bulk_density_flag = "to constant mass",
             carbon_measured_or_modeled = "measured",
             fraction_carbon_method = "EA",
             fraction_carbon_type = "total carbon", 
             carbonates_removed = "FALSE",
             carbonate_removal_method = "none specified") 

  

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~ site_id)

#check BHI core locations 
BHI <- cores %>% filter(cores$site_id == "BHI")
leaflet(BHI) %>% 
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
test_numeric_vars(cores)

## 3. Write Curated Data ####

# write data to final folder

write_csv(methods, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_methods.csv")
write_csv(cores, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")
write_csv(depthseries, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_depthseries.csv")

write_csv(plot_summary, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plot_summary.csv")
write_csv(plant_plot_detail, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plant_plot_summary.csv")
write_csv(plant, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plant.csv")
write_csv(allometric_eq, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_allometric_eq.csv")

# MAKE SURE TO RUN THE FINAL SECTION OF CODE AFTER THIS

## JW ----> Archived
## Citations 
#import DOIs
# 
# soil_citations <- read_xlsx("./data/primary_studies/CIFOR/CIFOR_docs/SWAMP_alt_bib.xlsx") %>% rename(doi = DOI) 
# 
# soil_bib_raw <- data_frame()
# for (i in soil_citations$doi) {
#   temp_df <- as.data.frame(GetBibEntryWithDOI(i))
#   soil_bib_raw <- rbind(soil_bib_raw, temp_df) %>% distinct()
# }
# 
# soil_bib <- soil_bib_raw %>% 
#   mutate(study_id = title,
#          bibliography_id = paste(study_id, "data", sep = "_"),
#          publication_type = "primary dataset")
# 
# #write bib
# bib_file <- soil_bib %>%
#   remove_rownames() %>% 
#   select(-c(study_id, publication_type)) %>% 
#   column_to_rownames("bibliography_id")
# 
# 
# WriteBib(as.BibEntry(bib_file), "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.bib")
# 
# write_csv(bib_file, "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.csv")


## JW Spotfixes ####

fixStudyID <- function(df){
  df_corrected <- df %>% mutate(study_id = gsub(" |-", "_", study_id)) 
  return(df_corrected)
}

alt_files <- dir("data/primary_studies/CIFOR/derivative_ALT/", pattern = ".csv")

for(file in alt_files){
  write_csv(
    read_csv(paste0("data/primary_studies/CIFOR/derivative_ALT/", file)) %>% fixStudyID(),
    paste0("data/primary_studies/CIFOR/derivative_ALT/", file)
  )
}

# Bibliography ####

# import DOIs
soil_citations_raw <- read_csv("./data/primary_studies/CIFOR/CIFOR_docs/SWAMP_alt_bib.csv") %>% 
  drop_na(study_id) %>% select(study_id, DOI) %>% rename(doi = DOI) %>% 
  fixStudyID() %>% distinct()

cifor_alt_cores <- read_csv("data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")

soil_citations <- full_join(cifor_alt_cores %>% distinct(study_id), soil_citations_raw) %>% 
  # patch in missing citations
  mutate(doi = case_when(study_id == "SWAMP_Data_Soil_carbon_Barreto_2017_Brazil" ~ "10.17528/CIFOR/DATA.00169",
                         study_id == "SWAMP_Data_Soil_carbon_Caetano_2017_Brazil" ~ "10.17528/CIFOR/DATA.00163",
                         study_id == "SWAMP_Data_Soil_carbon_Acarau_Boca_2016_Brazil"  ~ "10.17528/CIFOR/DATA.00171",
                         study_id == "SWAMP_Data_Soil_carbon_Baouth_2014_Senegal" ~ "10.17528/CIFOR/DATA.00155",
                         study_id == "SWAMP_Data_Soil_carbon_Caete_2017_Brazil" ~ "10.17528/CIFOR/DATA.00164",           
                         study_id == "SWAMP_Data_Soil_carbon_Cauassu_Leste_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00172",
                         study_id == "SWAMP_Data_Soil_carbon_Cauassu_Oeste_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00173", 
                         study_id == "SWAMP_Data_Soil_carbon_Cumbe_Leste_Camaro_2016_Brazil" ~ "10.17528/CIFOR/DATA.00174",
                         study_id == "SWAMP_Data_Soil_carbon_Cumbe_norte_Camarao_2016_Brazil" ~ "10.17528/CIFOR/DATA.00175",
                         study_id == "SWAMP_Data_Soil_carbon_Mangizal_Cauassu_2016_Brazil" ~ "10.17528/CIFOR/DATA.00176",
                         study_id == "SWAMP_Data_Soil_carbon_Manguinho_2016_Brazil" ~ "10.17528/CIFOR/DATA.00177", 
                         study_id == "SWAMP_Data_Soil_carbon_Porto_Ceu_Mangrove_2016_Brazil" ~ "10.17528/CIFOR/DATA.00178",
                         study_id == "SWAMP_Data_Soil_carbon_Porto_Ceu_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00179",
                         study_id == "SWAMP_Data_Soil_carbon_Paga_2014" ~ "10.17528/CIFOR/DATA.00214",
                         study_id == "SWAMP_Data_Soil_carbon_Bunaken_2011" ~ "10.17528/CIFOR/DATA.00141",
                         study_id == "SWAMP_Data_Soil_carbon_Cilacap_2011" ~ "10.17528/CIFOR/DATA.00142",
                         study_id == "SWAMP_Data_Soil_carbon_Sembilang_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00144",
                         study_id == "SWAMP_Data_Soil_carbon_Tanjung_Puting_2009_Indonesia" ~ "10.17528/CIFOR/DATA.00145",
                         study_id == "SWAMP_Data_Soil_carbon_Teminabuan_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00146", 
                         study_id == "SWAMP_Data_Soil_carbon_Timika_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00147",
                         study_id == "SWAMP_Data_Soil_carbon_CanGio_2012_Vietnam" ~ "10.17528/CIFOR/DATA.00148",
                         T ~ doi),
         pub_doi = case_when(grepl("Brazil", study_id) ~ "10.1098/rsbl.2018.0208",
                             grepl("Indonesia", study_id) & doi != "10.17528/cifor/data.00192" ~ "10.1038/nclimate2734",
                             study_id == "SWAMP_Data_Soil_carbon_CanGio_2012_Vietnam" ~ "10.1007/s11273-015-9479-2",
                             grepl("Berahan|Timbulsloko", study_id) ~ "10.13057/biodiv/d211134",
                             doi == "10.17528/cifor/data.00215" | doi == "10.17528/cifor/data.00216" ~ "10.1371/journal.pone.0187749",
                             T ~ NA)) %>% 
  pivot_longer(-study_id, names_to = "publication_type", values_to = "doi") %>% 
  drop_na(doi) %>% 
  mutate(publication_type = recode(publication_type, 
                                   "doi" = "primary dataset",
                                   "pub_doi" = "associated source"),
         doi = tolower(doi))



# loop through and pull
soil_bibs <- data.frame()

for (i in unique(soil_citations$doi)) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(i))
  soil_bibs <- bind_rows(soil_bibs, temp_df)
}

cifor_alt_study_citations <- soil_bibs %>% 
  remove_rownames() %>% 
  full_join(soil_citations %>% mutate(doi = tolower(doi)), multiple = "all") %>% 
  mutate(bibliography_id = case_when(publication_type == "primary dataset" ~ paste0(study_id, "_data"),
                                     publication_type == "associated source" & doi %in% c("10.1371/journal.pone.0187749", "10.17528/cifor/data.00155") ~ paste0("Kauffman_and_Bhomia_", year, "_article"),
                                     T ~ paste0(word(author), "_et_al_", year, "_article")),
         bibliography_id = recode(bibliography_id, "J._et_al_2018_article" = "Kauffman_et_al_2018_article")) %>% 
  filter(study_id != "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil") %>% 
  select(study_id, bibliography_id, publication_type, everything()) %>% 
  # needs manual addition
  add_row(study_id = "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil",
          bibliography_id = "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil_data",
          publication_type = "primary dataset",
          bibtype = "Misc",
          author = "Kauffman, J.B. and Bernardino, A.F. and Ferreira, T.O. and Giovannoni, L.R. and de O. Gomes, L.E. and Romero, D.J. and Jimenez, L.C.Z. and Ruiz, F.",
          publisher = "Center for International Forestry Research (CIFOR)",
          title = "SWAMP Dataset-Mangrove soil carbon-Furo Grande-2017",
          year = "2019",
          doi = "10.17528/CIFOR/DATA.00166",
          url = "https://doi.org/10.17528/CIFOR/DATA.00166"
  )


# write citations
write_csv(cifor_alt_study_citations, "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.csv")

