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

#create list/join all 
cifor_soil <- synthCIFOR(data_type = "soil") 
  
#skipped -- files in SWAMP data format

cifor_veg <- synthCIFOR(data_type = "vegetation")
#skips files in SWAMP data format 

#additional geographic coordinate sheets 
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


#create depth min and max columns based on formatting of Depth Interval column
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

#create core id and curate 
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


depthseries <- depthseries_core_id %>% 
  mutate(depth_min = ifelse(depth_min == 1183, 183, depth_min),
         depth_max = ifelse(depth_max == 1233, 233, depth_max),
         site_id = `Site ID`) %>% 
  select(-Plot, -`Sub-plot`, -`Site ID`)



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
latlong.full <- left_join(latlong.soil, latlong.biomass, by = c("Site ID", "Plot", "Sub-plot"), all.x = TRUE) %>% 
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



#Convert and standardize all coords to lat lon
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


##add year 
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

##  Species #####

#species information comes from tree biomass measurements? 


## 5. Biomass #####
#curate plot and plant table - structure based on current draft biomass structure 7/24

plot_summary <- cifor_veg %>% select(-filename) %>% 
            mutate(position_method = "other low resolution",
                   position_notes = "plot level position",
                   harvest_or_allometry = "allometry",
                   plot_id = paste0(Plot, `Sub-plot`),
                   plot_shape = "circular",
                   plot_radius = 3.5,
                   plant_plot_detail_present = "yes",
                   coordinate_obscured_flag = "no obscuring",
                   field_or_manipulation_code = "field") %>% 
             #'soil_core_present' --> if matching study id in 'cores'
            rename(site_id = `Site ID`,
                   year = `Data collection date (dd/mm/yyyy)`,
                   plot_area = `Sub-plot area (ha)`,
                   aboveground_biomass = `AGB summed per plot (Mg/ha)`,
                   belowground_biomass = `BGB summed per plot (Mg/ha)`) %>% distinct()



plant <- cifor_veg %>% select(-filename) %>%
                    rename(site_id = `Site ID`,
                           latitude = Latitude,
                           longitude = Longitude,
                           plot_id = Plot,
                           diameter_dbh = `DBH (cm)`,
                           decay_class = `Status (live/1/2/3)`,
                           biomass_aboveground = `AGB (Mg/ha)`,
                           biomass_belowground = `BGB (Mg/ha)`,
                           plot_radius = `Sub-plot design`) # %>% #divide diameter by 2 - how to handle the square plots??
          #  mutate(year = year(`Data collection date (dd/mm/yyyy)`))

  
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
#write_csv(sites, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")
write_csv(depthseries, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_depthseries.csv")
#write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
#write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")
#write_csv(plot, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plot.csv")
#write_csv(plant, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plant.csv")

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

