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


## Questions ####
# More methods data?
# Do we want to include any of these:

## Plot
# PROJID = SWAMP v CIFOR identifier

## Soil 
# N, N_CONT, CNR = Nitrogen info

## Tree & Sapling
# BA (basal area): include equation?

## TreeBiomass & SaplingBiomass (NB: columns renamed with _AGB and _BGB during pivot)
# C_CONCID = equation reference for C concentration (biomass to C-content conversion) 
# EQID = equation reference for biomass calculation
# COMPID = debris part (i.e., stem, branch, leaf, tree, root)


## Variable definitions to check:
# impact_class = eco_cond, disturb
# decay_class = decay_status
# plot_id (veg) = core_id (soil) = SUBPID (subplot id)
# study_id = filename
# transect/radius info = protocol_method_id
# plot_sediment_carbon (veg) = swamp_soil$Soil$C_CONT



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
  getdisturb <- paste(ref_disturb$DISTURBANCE, swamp_type$Disturbance$DISTURBANCE_NOTES, sep = "_")
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
  
  # decay status look up table
  ref_status <- read_excel(ref_path, sheet = "Ref_TreeStatus")
  getstatus <- ref_status$STATUS_DESCR
  names(getstatus) <- ref_status$ID
  
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
                          by = c("filename", "SUBPID")) %>% 
    full_join(swamp_type$TreeBiomass %>% # add plant-specific biomass data
                select(c(TREEID, COMPID, BIOMASS, EQID, C_CONT, C_CONCID, filename)) %>% 
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("BIOMASS", "EQID", "C_CONT", "C_CONCID"), values_fn = list) %>% 
                rename(plant_id = TREEID),
              by = c("filename", "plant_id")) %>% 
    full_join(swamp_type$TreeSubp_C, by = c("filename", "SUBPID")) %>%  # add subplot-specific biomass data
    rename(subplot_AGC = TREE_AGC,
           subplot_BGC = TREE_BGC,
           subplot_BA = TREE_BA) %>% 
    filter(subplot_AGC > 0.00 & subplot_BGC > 0.00) # remove subplots without tree data
  
  sap_table <- right_join(subplot_table, swamp_type$Sapling %>% # repeat the same as above for sapling data
                            rename(plant_id = ID) %>% 
                            mutate(tree_or_sapling = "sapling"), 
                          by = c("filename", "SUBPID")) %>% 
    full_join(swamp_type$SaplingBiomass %>% 
                select(c(SAPID, COMPID, BIOMASS, EQID, C_CONT, C_CONCID, filename)) %>% 
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("BIOMASS", "EQID", "C_CONT", "C_CONCID"), values_fn = list) %>% 
                rename(plant_id = SAPID),
              by = c("filename", "plant_id")) %>% 
    full_join(swamp_type$SaplingSubp_C, by = c("filename", "SUBPID")) %>%  
    rename(subplot_AGC = SAP_AGC,
           subplot_BGC = SAP_BGC,
           subplot_BA = SAP_BA) %>% 
    filter(subplot_AGC > 0.00 & subplot_BGC > 0.00) # remove subplots without sapling data
  
  veg_table <- full_join(tree_table, sap_table, by = intersect(names(tree_table), names(sap_table))) %>% 

  ## 3. Conform table to database structure 
    mutate(year = year(MDATE),
           month = month(MDATE),
           day = day(MDATE),
           site_name = gsub(" ", "_", getsiteid[SITEID]), 
           habitat = getlandcov[LANDCOVID],
           eco_cond = getecocond[ECOID],
           disturb = getdisturb[DISTRUBID],
           geomorphic_id = getgeo[GEOID],
           specific_gravity = getgrav[SGID],
           decay_status = getstatus[STATID], 
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
           diameter_breast_height = DBH,
           diameter_base = DBASE,
           basal_area = BA,
           protocol_method_id = PROTOID,
           plant_biomass_AGB = BIOMASS_AGB,
           plant_biomass_BGB = BIOMASS_BGB,
           plant_C_AGB = C_CONT_AGB,
           plant_C_BGB = C_CONT_BGB,
           deadbreak_height = DEADBREAK_HGT)
}


## Veg Tables ####
plot <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, plot_id, site_id, protocol_method_id, dominant_species, year, month, day, 
           longitude, latitude, position_accuracy, elevation, plant_count)) %>% 
  distinct()

plant <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, site_id, plot_id, plant_id, species_code, code_type, decay_status, 
           habitat, eco_cond, disturb, geomorphic_id, protocol_method_id, height, diameter_base,
           diameter_breast_height, basal_area, plant_biomass_AGB, plant_biomass_BGB, plant_C_AGB, 
           plant_C_BGB, subplot_AGC, subplot_BGC, subplot_BA, deadbreak_height, tree_or_sapling, 
           specific_gravity))



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
  getdisturb <- paste(ref_disturb$DISTURBANCE, swamp_type$Disturbance$DISTURBANCE_NOTES, sep = "_")
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

    ## 3. Conform table to database structure 
    mutate(year = year(MDATE),
           month = month(MDATE),
           day = day(MDATE),
           fraction_carbon = C/100,
           site_name = gsub(" ", "_", getsiteid[SITEID]), 
           habitat = getlandcov[LANDCOVID],
           eco_cond = getecocond[ECOID],
           disturb = getdisturb[DISTRUBID],
           impact_class = paste(eco_cond, disturb, sep = "_"),
           geomorphic_id = getgeo[GEOID],
           site_id = paste(site_name, PLOTID, sep = "_"),
           core_id = paste(site_id, SUBPID, sep = "_")) %>% 
    rename(elevation = ELEVATION,
           position_accuracy = ACCURACY,
           depth_min = MIND,
           depth_max = MAXD,
           dry_bulk_density = BD,
           study_id = filename,
           protocol_method_id = PROTOID) 
}


## Soil Tables #### 
depthseries <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon))

cores <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_accuracy, 
           elevation, habitat, geomorphic_id, protocol_method_id)) %>% 
  distinct()

impacts <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, impact_class)) %>% 
  distinct()





















