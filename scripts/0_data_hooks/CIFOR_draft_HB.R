## CCN Data Library
# Reading in CIFOR SWAMP data
# contact: Henry Betts, BettsH@si.edu

## Set up environment
library(readxl)
library(tidyverse)
library(lubridate)
library(leaflet)

source("scripts/1_data_formatting/cifor_utility_functions.R") 
source("scripts/1_data_formatting/qa_functions.R") # for reorderColumns()
source("scripts/1_data_formatting/curation_functions.R") 


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

  # function to find dominant species, i.e., the mode of a character vector
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # specific gravity look up table
  ref_grav <- read_excel(ref_path, sheet = "Ref_WoodDensity")
  getgrav <- ref_grav$SG
  names(getgrav) <- ref_grav$ID
  
  # C concentration look up table
  ref_conc <- read_excel(ref_path, sheet = "Ref_C_Concentration")
  getconc <- ref_conc$C
  names(getconc) <- ref_conc$ID 
  
  # biomass calculation equation look up table
  ref_eq <- read_excel(ref_path, sheet = "Ref_Equation")
  geteq <- ref_eq$EQUATION
  names(geteq) <- ref_eq$ID
  
  ## 2. Join Plot, SubPlot, Disturbance, Tree, and Sapling tables 
  subplot_table <- swamp_type$Plot %>% 
    rename(PLOTID = ID,
           plot_latitude = LATITUDE, # specify plot-level position 
           plot_longitude = LONGITUDE) %>% 
    full_join(swamp_type$Subplot, by = c("filename", "PLOTID")) %>% 
    rename(SUBPID = ID,
           subplot_latitude = LATITUDE, # specify subplot-level position
           subplot_longitude = LONGITUDE) %>% 
    mutate(pos_select_lat = ifelse(is.na(subplot_latitude), 1, 0), # choose plot- or subplot-level position data depending on presence of NAs
           pos_select_long = ifelse(is.na(subplot_longitude), 1, 0),
           latitude = ifelse(pos_select_lat > 0, paste(plot_latitude, "plot", sep = "_"), paste(subplot_latitude,"subplot", sep = "_")),
           longitude = ifelse(pos_select_long > 0, paste(plot_longitude, "plot", sep = "_"), paste(subplot_longitude, "subplot", sep = "_"))) %>% 
    full_join(swamp_type$Disturbance, by = c("filename", "SUBPID"))
  
  tree_table <- full_join(subplot_table, swamp_type$Tree %>% # join plot and subplot data to Tree and Sapling tables separately
                            rename(plant_id = ID) %>% 
                            mutate(tree_or_sapling = "tree"), 
                          by = c("filename", "SUBPID")) %>% 
    full_join(swamp_type$TreeBiomass %>% # add plant-specific biomass data
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                group_by(TREEID, COMPID) %>% 
                summarise(biomass = sum(BIOMASS), # sum the multiple AGB values to pivot_wider a single value 
                          c_cont = sum(C_CONT)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("biomass", "c_cont")) %>% 
                rename(plant_id = TREEID),
              by = "plant_id") %>% 
    full_join(swamp_type$TreeSubp_C, by = c("filename", "SUBPID")) %>%  # add subplot-specific biomass data
    rename(AGC = TREE_AGC,
           BGC = TREE_BGC) %>% 
    filter(AGC > 0.00 & BGC > 0.00) # remove subplots without tree data
  
  sap_table <- right_join(subplot_table, swamp_type$Sapling %>% # repeat the same as above for sapling data
                            rename(plant_id = ID) %>% 
                            mutate(tree_or_sapling = "sapling"), 
                          by = c("filename", "SUBPID")) %>% 
    full_join(swamp_type$SaplingBiomass %>% 
                mutate(COMPID = case_when(COMPID == 1 ~ "AGB",
                                          COMPID == 2 ~ "AGB",
                                          COMPID == 3 ~ "AGB",
                                          COMPID == 5 ~ "AGB",
                                          COMPID == 7 ~ "BGB",
                                          T ~ NA_character_)) %>% 
                group_by(SAPID, COMPID) %>% 
                summarise(biomass = sum(BIOMASS),
                          c_cont = sum(C_CONT)) %>% 
                pivot_wider(names_from = COMPID, values_from = c("biomass", "c_cont")) %>% 
                rename(plant_id = SAPID),
              by = "plant_id") %>% 
    full_join(swamp_type$SaplingSubp_C, by = c("filename", "SUBPID")) %>%  
    rename(AGC = SAP_AGC,
           BGC = SAP_BGC) %>% 
    filter(AGC > 0.00 & BGC > 0.00) # remove subplots without sapling data
  
  veg_table <- full_join(tree_table, sap_table, by = intersect(names(tree_table), names(sap_table))) %>% 

  ## 3. Conform table to database structure 
    # get look up table values and dates
    mutate(site_name = gsub(" ", "_", getsiteid[SITEID]), 
           ecotype = getlandcov[LANDCOVID],
           ecosystem_condition = getecocond[ECOID],
           disturbance_class = getdisturb[DISTRUBID],
           geomorphic_id = getgeo[GEOID],
           specific_gravity = getgrav[SGID],
           decay_class = ifelse(STATID == 2, 1,
                                ifelse(STATID == 3, 2,
                                       ifelse(STATID == 4, 3, NA_character_))),
           species_code = getspecies[SPECID],
           year = year(MDATE),
           month = month(MDATE),
           day = day(MDATE),
           site_id = paste(site_name, PLOTID, sep = "_"),
           plot_id = paste(site_id, SUBPID, sep = "_")) %>% 
    # group for C summary and species count
    group_by(plot_id) %>% 
    mutate(plant_count = n(),
           dominant_species = getmode(species_code),
           subplot_AGC = sum(AGC), # sum sapling and tree C
           subplot_BGC = sum(BGC),
           plot_biomass_carbon = subplot_AGC + subplot_BGC) %>%  
    ungroup() %>% 
    group_by(ecotype) %>%
    mutate(total_ecosystem_carbon = sum(plot_biomass_carbon)) %>%
    ungroup() %>% 
    # rename and sum variables
    mutate(code_type = case_when(grepl("spp", species_code) ~ "Genus",
                                 T ~ "Genus species"),
           biomass_total = biomass_AGB + biomass_BGB,
           biomass_total_carbon = c_cont_AGB + c_cont_BGB,
           biomass_decay_corrected = ifelse(decay_class == 1, biomass_AGB * .975,
                                            ifelse(decay_class == 2, biomass_AGB * .85,
                                                   ifelse(decay_class == 3, biomass_AGB * .5, NA))),
           study_id = substr(filename, 1, nchar(filename) - 11),
           field_or_manipulation_code = "field",
           harvest_or_allometry = "allometry") %>% 
    # use position formula created above to assign plot- or subplot- level code in "position_level_note"
    separate(col = "latitude", into = c("latitude", "position_level_note"), sep = "_") %>% 
    separate(col = "longitude", into = "longitude", sep = "_") %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    rename(elevation = ELEVATION,
           disturbance_note = DISTURBANCE_NOTES,
           position_accuracy = ACCURACY,
           height = HGT,
           diameter_breast_height = DBH,
           diameter_base = DBASE,
           plant_AGB = biomass_AGB,
           plant_BGB = biomass_BGB,
           plant_AGC = c_cont_AGB,
           plant_BGC = c_cont_BGB)
}


## Veg Tables ####
plot <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, plot_id, site_id, dominant_species, year, month, day, harvest_or_allometry,
           field_or_manipulation_code, longitude, latitude, position_accuracy, position_level_note,
           elevation, plant_count, subplot_AGC, subplot_BGC, plot_biomass_carbon, total_ecosystem_carbon)) %>% 
  distinct()

plant <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, site_id, plot_id, plant_id, species_code, code_type, decay_class, 
           ecotype, ecosystem_condition, disturbance_class, disturbance_note, geomorphic_id, 
           height, diameter_base, diameter_breast_height, plant_AGB, plant_BGB, biomass_total, 
           biomass_decay_corrected, biomass_total_carbon, plant_AGC, plant_BGC, tree_or_sapling, 
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
  
  ## 2. Join Plot, SubPlot, Disturbance, and Soil tables 
  soil_table <- swamp_type$Plot %>% 
    rename(PLOTID = ID,
           plot_latitude = LATITUDE, # specify plot-level position 
           plot_longitude = LONGITUDE) %>% 
    full_join(swamp_type$Subplot, by = c("filename", "PLOTID")) %>% 
    rename(SUBPID = ID,
           subplot_latitude = LATITUDE, # specify subplot-level position
           subplot_longitude = LONGITUDE) %>% 
    mutate(pos_select_lat = ifelse(is.na(subplot_latitude), 1, 0), # choose plot- or subplot-level position data depending on presence of NAs
           pos_select_long = ifelse(is.na(subplot_longitude), 1, 0),
           latitude = ifelse(pos_select_lat > 0, paste(plot_latitude, "plot", sep = "_"), paste(subplot_latitude,"subplot", sep = "_")),
           longitude = ifelse(pos_select_long > 0, paste(plot_longitude, "plot", sep = "_"), paste(subplot_longitude, "subplot", sep = "_"))) %>% 
    full_join(swamp_type$Disturbance, by = c("filename", "SUBPID")) %>% 
    right_join(swamp_type$Soil, by = c("filename", "SUBPID")) %>% # drop site-level only data

    ## 3. Conform table to database structure 
    mutate(year = year(MDATE),
           month = month(MDATE),
           day = day(MDATE),
           fraction_carbon = C/100,
           site_name = gsub(" ", "_", getsiteid[SITEID]), 
           habitat = getlandcov[LANDCOVID],
           ecosystem_condition = getecocond[ECOID],
           disturbance_class = getdisturb[DISTRUBID],
           geomorphic_id = getgeo[GEOID],
           site_id = paste(site_name, PLOTID, sep = "_"),
           core_id = paste(site_id, SUBPID, sep = "_"),
           study_id = substr(filename, 1, nchar(filename) - 11)) %>% 
    separate(col = "latitude", into = c("latitude", "position_level_note"), sep = "_") %>% 
    separate(col = "longitude", into = "longitude", sep = "_") %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    rename(elevation = ELEVATION,
           disturbance_note = DISTURBANCE_NOTES,
           position_accuracy = ACCURACY,
           depth_min = MIND,
           depth_max = MAXD,
           dry_bulk_density = BD,
           impact_class = ecosystem_condition) 
}


## Soil Tables #### 
depthseries <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon))

cores <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_accuracy, 
           position_level_note, elevation, habitat, geomorphic_id)) %>% 
  distinct()

impacts <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, impact_class)) %>% 
  distinct()


## Bibliography ####
library(RefManageR)
library(bibtex)

soil_doi_list <- c("10.17528/CIFOR/DATA.00262", "10.17528/CIFOR/DATA.00265",
                   "10.17528/CIFOR/DATA.00254", "10.17528/CIFOR/DATA.00240",
                   "10.17528/CIFOR/DATA.00239", "10.17528/CIFOR/DATA.00234",
                   "10.17528/CIFOR/DATA.00233", "10.17528/CIFOR/DATA.00232",
                   "10.17528/CIFOR/DATA.00231", "10.17528/CIFOR/DATA.00230",
                   "10.17528/CIFOR/DATA.00229", "10.17528/CIFOR/DATA.00228",
                   "10.17528/CIFOR/DATA.00227", "10.17528/CIFOR/DATA.00226",
                   "10.17528/CIFOR/DATA.00225", "10.17528/CIFOR/DATA.00221")

soil_bib <- data.frame()
for (i in soil_doi_list) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(i))
  soil_bib <- bind_rows(soil_bib, temp_df)
}

veg_doi_list <- c("10.17528/CIFOR/DATA.00261", "10.17528/CIFOR/DATA.00255",
                  "10.17528/CIFOR/DATA.00253", "10.17528/CIFOR/DATA.00218",
                  "10.17528/CIFOR/DATA.00242", "10.17528/CIFOR/DATA.00241",
                  "10.17528/CIFOR/DATA.00219", "10.17528/CIFOR/DATA.00217")

veg_bib <- data.frame()
for (n in veg_doi_list) {
  temp.df <- as.data.frame(GetBibEntryWithDOI(n))
  veg_bib <- bind_rows(veg_bib, temp.df)
}





