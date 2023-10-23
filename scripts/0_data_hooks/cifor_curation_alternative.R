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
#skipped -- files in SWAMP data format and brazil site coordinated lists 

cifor_veg <- synthCIFOR(data_type = "vegetation")
#skips files in SWAMP data format 

#additional geographic coordinate sheets 
geo_brazil <- read_xlsx("data/primary_studies/CIFOR/original/Kauffman_2019_BocaGrande_soil/Geographic Coordinate for SWAMP dataset-Mangrove soil carbon-Brazil-2017.xlsx",2)

geo_brazil <- geo_brazil %>% rename(latitude = Latitude,
                                    longitude = Longitude,
                                    `Site ID` = Site) %>% select(-No)

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
                      carbonates_removed = "FALSE",
                      carbonate_removal_method = "none specified")


## 2. Cores ####

#read in site level coords not included in raw data 
missing_coords <- read_xlsx("./data/primary_studies/CIFOR/missing_site_level_coords.xlsx")
                                       
#subset out sites that have ONLY biomass -- these are not included in cores table 
veg <- cifor_veg %>% filter(`Site ID` %in% c("BHI", "ACA", "BAO", "BOC", "BAR", "BOC", "CAE","CIL","DIA", "FAM", "FUR", "MAN",
                                             "MANG", "MAR", "MOU", "SAL", "SAN", "BUN", "KBR", "TAN", "CA_", "SEM", "TEM")) %>% distinct()

#separate all lat long by site - big table
latlong.biomass <- veg %>% select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude, filename) %>%
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
  
latlong.soil <- cifor_soil %>% select(`Site ID`, Plot, `Sub-plot`, Latitude, Longitude, filename) %>% 
  mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) %>% drop_na() 


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
  soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_")) %>% #pull year from study id? 
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
      select(filename,site_id, core_id, latitude, longitude) %>% distinct()

join_cores <- left_join(cifor_cores, latlong.full, by = c("core_id", "site_id", "filename")) %>% 
  left_join(missing_coords) %>% 
   mutate(latitude = case_when(is.na(latitude.x) ~ latitude.y, TRUE ~ latitude.x),
         longitude = case_when(is.na(longitude.x) ~ longitude.y, TRUE ~ longitude.x)) %>% 
  select(-ends_with('.x'), -ends_with('.y')) %>% distinct() %>% left_join(geo_brazil)




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

cores1 <- rbind(subset_1, out_1) %>% fill()


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
                    rename(study_id = filename) %>% distinct()

cores <- reorderColumns("cores", cores) %>% select(-`Site ID`, -`Sub-Plot`)





## 3. Depthseries ####
cifor_depthseries <- cifor_soil %>% mutate(core_id = paste(`Site ID`, Plot, `Sub-plot`, sep = "_"),
                                    fraction_carbon = as.numeric(`Carbon content (%)`),
                                    fraction_carbon = fraction_carbon/100,
                                    method_id = "SWAMP") %>%  
              separate(`Depth interval (cm)`, c("depth_min", "depth_max"), sep = "-") %>% 
              rename(dry_bulk_density = `Bulk density (g/cm3)`,
                     site_id = `Site ID`,
                     study_id = filename) %>% 
              select(study_id, core_id, method_id, site_id, fraction_carbon, dry_bulk_density, depth_min, depth_max)

depthseries <- reorderColumns("depthseries", cifor_depthseries) %>% distinct()


## 4. Species #####

#species information comes from tree biomass measurements? not currently including within sediment tables 


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
                           plot_radius = `Sub-plot design`) %>% #divide diameter by 2 - how to handle the square plots??
          #  mutate(year = year(`Data collection date (dd/mm/yyyy)`))


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

write_csv(methods, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_methods.csv")
#write_csv(sites, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")
write_csv(depthseries, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_depthseries.csv")
#write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
#write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")
#write_csv(plot, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plot.csv")
#write_csv(plant, "data/primary_studies/CIFOR/derivative/Author_et_al_YYYY_plant.csv")


## Citations 
#import DOIs

soil_citations <- read_xlsx("./data/primary_studies/CIFOR/SWAMP_bib.xlsx") %>% rename(doi = DOI) 

soil_bib_raw <- data_frame()
for (i in soil_citations$doi) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(i))
  soil_bib_raw <- rbind(soil_bib_raw, temp_df) %>% distinct()
}

soil_bib <- soil_bib_raw %>% 
  mutate(study_id = title,
         bibliography_id = paste(study_id, "data", sep = "_"),
         publication_type = "primary dataset")

#write bib
bib_file <- soil_bib %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


WriteBib(as.BibEntry(bib_file), "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.bib")

write_csv(bib_file, "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.csv")
