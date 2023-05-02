# Reading in CIFOR data
# Henry Betts, BettsH@si.edu

## Load libraries ####
library(readxl)
library(tidyverse)
library(lubridate)
library(leaflet)

source("scripts/1_data_formatting/cifor_utility_functions.R") 
source("scripts/1_data_formatting/qa_functions.R") # for reorderColumns()
source("scripts/1_data_formatting/curation_functions.R") 

## Data table notes ####
# do we want to include any of these in the database?

## Plot
# PROJID = SWAMP v CIFOR identifier

## Soil 
# C_CONT = C content in MgC/ha (depth interval * C_fraction * bulk_density)
# N, N_CONT, CNR = Nitrogen info

## Tree & Sapling
# SGID = equation reference # for specific gravity

## TreeBiomass & SaplingBiomass
# C_CONCID = equation reference # for C concentration (biomass to C-content conversion)
# EQID = equation reference # for biomass calculation



## Questions/to-do list

# remove NAs in veg_table; remake the tables

# scale biomass/C to Mg/ha ?
# define decay class 1, 2, 3

# plot_id = core_id, study_id = filename; should these remain as is?
# make the methods table (i.e., sapling v. tree measurements)
# timezone for as.Date()? -- shouldn't need this, use lubridate
# Plot::PROTOID = DBH, transect, radius info?
# SoilDepth::SDTID as core_length_flag -> look at patterns/ask curator
# Soil::C_CONT --- plot_sediment_carbon for biomass table?
# plot area = tree, sap, und areas? not for plot stem-density




## Veg Function ####

swamp_veg <- synthSWAMP("vegetation")

SWAMP_veg_converter <- function(swamp_type) {
  ref_path <- "./data/primary_studies/CIFOR/CIFOR_docs/Ref_Tables_2020-05-12.xlsx"
  
  ## 1. Create look up tables 
  # site name look up table
  getsiteid <- swamp_type$Site$SITE_NAME 
  names(getsiteid) <- swamp_type$Site$ID
  
  # disturbance impact class look up table
  ref_disturb <- read_excel(ref_path, sheet = "Ref_Disturbance")
  getdisturb <- ref_disturb$DISTURBANCE
  names(getdisturb) <- ref_disturb$ID
  
  # ecological impact class look up table
  ref_ecocond <- read_excel(ref_path, sheet = "Ref_EcologicalCond")
  getecocond <- ref_ecocond$ECO_COND
  names(getecocond) <- ref_ecocond$ID
  
  # geomorphic data look up table
  ref_geo <- read_excel(ref_path, sheet = "Ref_Geomorphic")
  getgeo <- ref_geo$GEOMORP
  names(getgeo) <- ref_geo$ID
  
  # habitat class look up table
  ref_landcov <- read_excel(ref_path, sheet = "Ref_LandCover")
  getlandcov <- ref_landcov$LANDCOV
  names(getlandcov) <- ref_landcov$ID
  
  # tree species look up table
  ref_species <- read_excel(ref_path, sheet = "Ref_Species")
  getspecies <- ref_species$SCIENTIFIC_NAME
  names(getspecies) <- ref_species$ID
  
  # tree alive v. dead look up table
  ref_status <- read_excel(ref_path, sheet = "Ref_TreeStatus")
  getstatus <- ref_status$STATUS_DESCR
  names(getstatus) <- ref_status$ID
  
  # debris class look up table (only present in TreeBiomass - aka modeled - table)
  ref_debris <- read_excel(ref_path, sheet = "Ref_Component")
  getdebris <- paste(ref_debris$COMP, ref_debris$COMPONENT_DESCR, sep = "_") 
  names(getdebris) <- ref_debris$ID
  
  # function to find dominant species, i.e., the mode of a character vector
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # specific gravity look up table
  ref_grav <- read_excel(ref_path, sheet = "Ref_WoodDensity")
  getgrav <- ref_grav$SG
  names(getgrav) <- ref_grav$ID
  
  ## 2. Join Plot, SubPlot, Disturbance, Tree, and Sapling tables 
  subplot_table <- swamp_type$Plot %>% 
    rename(PLOTID = ID,
           plot_latitude = LATITUDE, # specify plot-level position 
           plot_longitude = LONGITUDE) %>% 
    full_join(swamp_type$Subplot, by = c("filename", "PLOTID")) %>% 
    rename(SUBPID = ID,
           subplot_latitude = LATITUDE, # specify subplot-level position
           subplot_longitude = LONGITUDE) %>% 
    mutate(pos_select_lat = ifelse(is.na(plot_latitude), 1, 0), # choose plot- or subplot-level position data depending on presence of NAs
           pos_select_long = ifelse(is.na(plot_longitude), 1, 0),
           latitude = ifelse(pos_select_lat > 0, subplot_latitude, plot_latitude),
           longitude = ifelse(pos_select_long > 0, subplot_longitude, plot_longitude)) %>% 
    full_join(swamp_type$Disturbance, by = c("filename", "SUBPID"))
  
  tree_table <- full_join(subplot_table, swamp_type$Tree %>% # join plot and subplot data to Tree and Sapling tables separately
                            rename(plant_id = ID) %>% 
                            mutate(tree_or_sapling = "tree"), 
                          by = "SUBPID") %>% 
    full_join(swamp_type$TreeBiomass %>% # add plant-specific biomass data
                select(c(TREEID, COMPID, BIOMASS, EQID, C_CONT, C_CONCID)) %>% 
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("BIOMASS", "EQID", "C_CONT", "C_CONCID"), values_fn = list) %>% 
                rename(plant_id = TREEID),
              by = "plant_id") %>% 
    full_join(swamp_type$TreeSubp_C, by = "SUBPID") %>%  # add subplot-specific biomass data
    rename(subplot_AGC = TREE_AGC,
           subplot_BGC = TREE_BGC,
           subplot_BA = TREE_BA)
  
  sap_table <- right_join(subplot_table, swamp_type$Sapling %>% # repeat the same as above for sapling data
                            rename(plant_id = ID) %>% 
                            mutate(tree_or_sapling = "sapling"), 
                          by = "SUBPID") %>% 
    full_join(swamp_type$SaplingBiomass %>% 
                select(c(SAPID, COMPID, BIOMASS, EQID, C_CONT, C_CONCID)) %>% 
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("BIOMASS", "EQID", "C_CONT", "C_CONCID"), values_fn = list) %>% 
                rename(plant_id = SAPID),
              by = "plant_id") %>% 
    full_join(swamp_type$SaplingSubp_C, by = "SUBPID") %>%  # there will be subplots without sapling data
    rename(subplot_AGC = SAP_AGC,
           subplot_BGC = SAP_BGC,
           subplot_BA = SAP_BA)
  
  veg_table <- full_join(tree_table, sap_table, by = intersect(names(tree_table), names(sap_table))) %>% 
    select(c(SUBPID, PLOTID, plant_id, latitude, tree_or_sapling, BIOMASS_AGB, TREE_AGC, DBH, BA))
    
    ## 3. Conform table to database structure 
    mutate(dates = as.Date(MDATE, "%Y/%m/%d", tz = "UTC"),
           year = year(dates),
           month = month(dates),
           day = day(dates),
           site_name = gsub(" ", "_", getsiteid[SITEID]), 
           habitat = getlandcov[LANDCOVID],
           eco_cond = getecocond[ECOID],
           disturb = getdisturb[DISTRUBID],
           geomorphic_id = getgeo[GEOID],
           debris_status = getdebris[COMPID],
           specific_gravity = getgrav[SGID],
           decay_status = getstatus[STATID], 
           impact_class = paste(eco_cond, disturb, sep = "_"),
           species_code = getspecies[SPECID],
           code_type = case_when(grepl("spp", species_code) ~ "Genus",
                                 T ~ "Genus species"),
           site_id = paste(site_name, PLOTID, sep = "_"),
           plot_id = paste(site_id, SUBPID, sep = "_")) %>% 
    group_by(plot_id) %>% 
    mutate(plant_count = n(),
           dominant_species = getmode(species_code)) %>%  
    rename(elevation = ELEVATION,
           study_id = filename,
           position_accuracy = ACCURACY,
           height = HGT,
           diameter_breast_heigh = DBH,
           diameter_base = DBASE,
           basal_area = BA,
           protocol_method = PROTOID,
           plant_biomass_kg = BIOMASS,
           plant_C_kg = C_CONT,
           subplot_total_AGC = plot_AGC,
           subplot_total_BGC = plot_BGC,
           subplot_total_BA = plot_BA,
           deadbreak_height = DEADBREAK_HGT)
}


## Veg Tables ####
plot <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, site_id, plot_id, dominant_species, year, month, day, 
           longitude, latitude, position_accuracy, elevation, plant_count)) %>% 
  distinct()
# include: position/elevation data, is_peak_biomass, field_or_manipulation_code, harvest_or_allometry, ecotype


plant <- swamp_veg %>% 
  select(c(study_id, site_id, plot_id, debris_status, decay_status, geomorphic_id, protocol_method, height, diameter_base,
           diameter_breast_height, basal_area, tree_or_sapling))
# include: biomass..., plot_density, biomass_decay_corrected, decay_3_biomass, biomass_downed_wood, biomass_flag







## Soil Function ####

swamp_soil <- synthSWAMP("soil")

SWAMP_soil_converter <- function(swamp_type) { # use synthSWAMP() output here
  ref_path <- "./data/primary_studies/CIFOR/CIFOR_docs/Ref_Tables_2020-05-12.xlsx"
  
  ## 1. Create look up tables 
  # site name look up table
  getsiteid <- swamp_type$Site$SITE_NAME 
  names(getsiteid) <- swamp_type$Site$ID
  
  # disturbance impact class look up table
  ref_disturb <- read_excel(ref_path, sheet = "Ref_Disturbance")
  getdisturb <- ref_disturb$DISTURBANCE
  names(getdisturb) <- ref_disturb$ID
  
  # ecological impact class look up table
  ref_ecocond <- read_excel(ref_path, sheet = "Ref_EcologicalCond")
  getecocond <- ref_ecocond$ECO_COND
  names(getecocond) <- ref_ecocond$ID
  
  # habitat class look up table
  ref_landcov <- read_excel(ref_path, sheet = "Ref_LandCover")
  getlandcov <- ref_landcov$LANDCOV
  names(getlandcov) <- ref_landcov$ID
  
  # geomorphic data look up table
  ref_geo <- read_excel(ref_path, sheet = "Ref_Geomorphic")
  getgeo <- ref_geo$GEOMORP
  names(getgeo) <- ref_geo$ID
  
  # core length flag look up table
  ref_depth <- read_excel(ref_path, sheet = "Ref_SoilDepth") # biomass
  getcoredepth <- ref_depth$DEPTH_DESCR
  names(getcoredepth) <- ref_depth$ID
  
  ## 2. Join Plot, SubPlot, Disturbance, and Soil tables 
  soil_table <- swamp_type$Plot %>% 
    rename(PLOTID = ID,
           plot_latitude = LATITUDE, # specify plot-level position 
           plot_longitude = LONGITUDE) %>% 
    full_join(swamp_type$Subplot, by = c("filename", "PLOTID")) %>% 
    rename(SUBPID = ID,
           subplot_latitude = LATITUDE, # specify subplot-level position
           subplot_longitude = LONGITUDE) %>% 
    mutate(pos_select_lat = ifelse(is.na(plot_latitude), 1, 0), # choose plot- or subplot-level position data depending on presence of NAs
           pos_select_long = ifelse(is.na(plot_longitude), 1, 0),
           latitude = ifelse(pos_select_lat > 0, subplot_latitude, plot_latitude),
           longitude = ifelse(pos_select_long > 0, subplot_longitude, plot_longitude)) %>% 
    full_join(swamp_type$Disturbance, by = c("filename", "SUBPID")) %>% 
    right_join(swamp_type$Soil, by = c("filename", "SUBPID")) %>% # drop site-level only data
#    mutate(core = paste(SUBPID, SAMP, sep = "_")) %>% # create a column to join to "SoilDepth" (only if including SDTID as core_length_flag)
#    left_join(swamp_type$SoilDepth %>% mutate(core = paste(SUBPID, SAMP, sep = "_")), by = c("core", "SUBPID", "filename")) %>% 
    
    ## 3. Conform table to database structure 
    mutate(dates = as.Date(MDATE, "%Y/%m/%d", tz = "UTC"),
           year = year(dates),
           month = month(dates),
           day = day(dates),
           fraction_carbon = C/100,
           site_name = gsub(" ", "_", getsiteid[SITEID]), 
           habitat = getlandcov[LANDCOVID],
           eco_cond = getecocond[ECOID],
           disturb = getdisturb[DISTRUBID],
           impact_class = paste(eco_cond, disturb, sep = "_"),
           geomorphic_id = getgeo[GEOID],
#           core_length_note = getcoredepth[SDTID], 
#           core_length_flag = ifelse(core_length_note == "Soil depth was greater than depth listed.", 
#                                     "core depth limited by length of corer",
#                                     ifelse(core_length_note =="Measured soil depth.", 
#                                            "core depth represents deposit depth", 
#                                            NA)),
           site_id = paste(site_name, PLOTID, sep = "_"),
           core_id = paste(site_id, SUBPID, sep = "_")) %>% 
    rename(elevation = ELEVATION,
           position_accuracy = ACCURACY,
           depth_min = MIND,
           depth_max = MAXD,
           dry_bulk_density = BD,
           study_id = filename,
           protocol_method = PROTOID) 
}


## Soil Tables #### 
depthseries <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon))

cores <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_accuracy, 
           elevation, habitat, geomorphic_id, protocol_method)) %>% 
  distinct()

impacts <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, impact_class)) %>% 
  distinct()





## Spot check the function ####
# soil: swamp_type <- readExcelWorkbook("./data/primary_studies/CIFOR/original/Sharma_2020_KohKong_soil/KohKong_Soil_2020-10-23.xlsx")
# veg: swamp_type <- readExcelWorkbook("./data/primary_studies/CIFOR/original/Sharma_2020_KohKong_veg/KohKong_Veg_2020-10-23.xlsx")
soil_table_test <- swamp_type$Plot %>% 
  rename(PLOTID = ID,
         plot_latitude = LATITUDE,  
         plot_longitude = LONGITUDE) %>% 
  full_join(swamp_type$Subplot, by = c("PLOTID")) %>% 
  rename(SUBPID = ID,
         subplot_latitude = LATITUDE, 
         subplot_longitude = LONGITUDE) %>% 
  mutate(pos_select_lat = ifelse(is.na(plot_latitude), 1, 0), 
         pos_select_long = ifelse(is.na(plot_longitude), 1, 0),
         latitude = ifelse(pos_select_lat > 0, subplot_latitude, plot_latitude),
         longitude = ifelse(pos_select_long > 0, subplot_longitude, plot_longitude)) %>% 
  full_join(swamp_type$Disturbance, by = c("SUBPID")) %>% 
  right_join(swamp_type$Soil, by = c("SUBPID")) 
















