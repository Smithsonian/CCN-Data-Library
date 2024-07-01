## CCN Data Library
# Reading in CIFOR SWAMP data (https://data.cifor.org/dataverse/s?q=&types=dataverses&sort=dateSort&order=desc&page=1)
# contact: Henry Betts, BettsH@si.edu

## Set up environment
library(readxl)
library(tidyverse)
library(lubridate)
library(leaflet)

source("scripts/1_data_formatting/cifor_utility_functions.R") 
source("scripts/1_data_formatting/qa_functions.R") # for reorderColumns()
source("scripts/1_data_formatting/curation_functions.R") 

## References and Codes ####

# read in reference workbook
# refs <- readExcelWorkbook("./data/primary_studies/CIFOR/CIFOR_docs/Ref_Tables_2020-05-12.xlsx")

# SWAMP_veg_converter <- function(swamp_type) {
ref_path <- "./data/primary_studies/CIFOR/CIFOR_docs/Ref_Tables_2020-05-12.xlsx"

## 1. Create look up tables 
# C concentration look up table
ref_conc <- read_excel(ref_path, sheet = "Ref_C_Concentration")
getconc <- data.frame(ID = 1:8, 
                      C = ref_conc$C,
                      CITID = ref_conc$CITID)

# disturbance impact class look up table
ref_disturb <- read_excel(ref_path, sheet = "Ref_Disturbance")
getdisturb <- ref_disturb$DISTURBANCE
names(getdisturb) <- ref_disturb$ID

# ecological impact class look up table
ref_ecocond <- read_excel(ref_path, sheet = "Ref_EcologicalCond")
getecocond <- ref_ecocond$ECO_COND
names(getecocond) <- ref_ecocond$ID

# equation for biomass calculation look up table
ref_eq <- read_excel(ref_path, sheet = "Ref_Equation")
geteq <- data.frame(ID = c(1:11, 13:23, 25, 27:43, 1000, 1001),
                    allometric_eq_formula = ref_eq$EQUATION,
                    output_unit = ref_eq$OUT_UNITS,
                    parameter_names = ref_eq$INPUT_PARAMETERS,
                    R2 = ref_eq$R2,
                    RSE = ref_eq$SE,
                    diameter_max = ref_eq$MAXD,
                    diameter_min = ref_eq$MIND,
                    location_description = ref_eq$ORIGIN)

# geomorphic data look up table
ref_geo <- read_excel(ref_path, sheet = "Ref_Geomorphic")
getgeo <- ref_geo$GEOMORP
names(getgeo) <- ref_geo$ID

# specific gravity look up table
ref_grav <- read_excel(ref_path, sheet = "Ref_WoodDensity")
getgrav <- data.frame(ID = 1:84,
                      SG = ref_grav$SG,
                      CITID = ref_grav$CITID)

# landcover/habitat class look up table
ref_landcov <- read_excel(ref_path, sheet = "Ref_LandCover")
getlandcov <- tolower(ref_landcov$LANDCOV)
names(getlandcov) <- ref_landcov$ID

# function to find dominant species, i.e., the mode of a character vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# site name look up table
getsiteid <- swamp_veg$Site$SITE_NAME 
names(getsiteid) <- swamp_veg$Site$ID

# tree species look up table
ref_species <- read_excel(ref_path, sheet = "Ref_Species")
getspecies <-  gsub("All species", "NA", ref_species$SCIENTIFIC_NAME) # No species listed for this code
names(getspecies) <- ref_species$ID

# source citation look up table
ref_source <- read_excel(ref_path, sheet = "Ref_Citation")
getsource <- ref_source$CITATION
names(getsource) <- ref_source$ID


## Vegetation ####

swamp_veg <- synthSWAMP("vegetation")

## Join Plot, SubPlot, Disturbance, Tree, and Sapling tables 
site_country <- swamp_veg$Site %>% rename(SITEID = ID) %>% select(SITEID, COUNTRYID) %>% distinct() %>% 
  left_join(refs$Ref_Country %>% rename(COUNTRYID = ID) %>% select(-WREGID)) %>% select(-COUNTRYID)

# Subplot (we might consider this plot-level)
subplot_table <- swamp_veg$Plot %>% 
  left_join(site_country) %>% 
  rename(PLOTID = ID,
         PLOT_NOTES = NOTES,
         plot_latitude = LATITUDE, # specify plot-level position 
         plot_longitude = LONGITUDE) %>% 
  full_join(swamp_veg$Subplot, by = c("filename", "PLOTID"), multiple = "all") %>% 
  rename(SUBPID = ID,
         SUBPLOT_NOTES = NOTES,
         subplot_latitude = LATITUDE, # specify subplot-level position
         subplot_longitude = LONGITUDE) %>% 
  mutate(position_notes = case_when(is.na(subplot_latitude) & is.na(subplot_longitude) ~ "plot-level",
                                    T ~ "subplot-level")) %>% 
  mutate(
    # pos_select_lat = ifelse(is.na(subplot_latitude), 1, 0), # choose plot- or subplot-level position data depending on presence of NAs
    # pos_select_long = ifelse(is.na(subplot_longitude), 1, 0),
    latitude = case_when(!is.na(subplot_latitude) ~ subplot_latitude, T ~ plot_latitude),
    longitude = case_when(!is.na(subplot_longitude) ~ subplot_longitude, T ~ plot_longitude)) %>% 
  full_join(swamp_veg$Disturbance, by = c("filename", "SUBPID")) %>% 
  # add subplot-specific biomass data
  full_join(swamp_veg$TreeSubp_C, by = c("filename", "SUBPID")) %>% 
  full_join(swamp_veg$SaplingSubp_C, by = c("filename", "SUBPID"))

# nrow(subplot_table %>% distinct(SUBPID, filename)) == nrow(subplot_table)
# rename(subp_AGC = TREE_AGC,
#        subp_BGC = TREE_BGC,
#        subp_BA = TREE_BA) 

# test plot sampling locations
# subplot_table %>% select(latitude, longitude, position_notes) %>% distinct() %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(radius = 2, label = ~position_notes)


# full_join(subplot_table, by = c("filename", "SUBPID"), multiple = "all") 
# there are 11 SUBPIDs in subplot_table not found in Tree table

# let's leave this off for now
# full_join(swamp_veg$TreeBiomass %>% # add plant-specific biomass data
#            mutate(above_or_belowground = case_when(COMPID %in% c(1, 2, 3, 5) ~ "aboveground",
#                                                    COMPID == 7 ~ "belowground",
#                                                    T ~ NA_character_)
#                   # plant_id = paste(TREEID, "tree", sep = "_")
#                   ),
#          by = c("filename", "TREEID")) %>% 
# full_join(swamp_veg$TreeSubp_C, by = c("filename", "SUBPID")) %>% # add subplot-specific biomass data
# rename(subp_AGC = TREE_AGC,
#        subp_BGC = TREE_BGC,
#        subp_BA = TREE_BA) 


# sap_table <- full_join(subplot_table, swamp_type$Sapling %>% # repeat the same as above for sapling data
#                           mutate(plant_id = paste(ID, "sapling", sep = "_")), 
#                         by = c("filename", "SUBPID")) %>% # there are 239 SUBPIDs in subplot_table not found in Sapling table
#   full_join(swamp_type$SaplingBiomass %>% 
#               mutate(above_or_belowground = case_when(COMPID %in% c(1, 2, 3, 5) ~ "aboveground",
#                                                       COMPID == 7 ~ "belowground",
#                                                       T ~ NA_character_),
#                      plant_id = paste(SAPID, "sapling", sep = "_")),
#             by = c("filename", "plant_id")) %>% 
#   full_join(swamp_type$SaplingSubp_C, by = c("filename", "SUBPID")) %>% 
# rename(subp_AGC = SAP_AGC,
#        subp_BGC = SAP_BGC,
#        subp_BA = SAP_BA)

# veg_table <- full_join(tree_table, sap_table) %>% # 5 instances of SUBPIDs that have neither tree nor sapling data but do have subplot data

## 3. Conform table to database structure 
# get look up table values and define terms
clean_subplot <- subplot_table %>%
  rename(elevation = ELEVATION,
         disturbance_note = DISTURBANCE_NOTES,
         disturbance_year = DYEAR,
         position_accuracy = ACCURACY,
         method_id = PROTOID,
         plot_area = TREE_AREA,
         AGC_trees = TREE_AGC,
         BGC_trees = TREE_BGC,
         tree_basal_area = TREE_BA,
         AGC_saplings = SAP_AGC,
         BGC_saplings = SAP_BGC,
         sapling_basal_area = SAP_BA) %>% 
  
  # assign study_ids based on bibliography structure
  mutate(study_id = case_when(grepl("KRE", filename) ~ "Bukoski_et_al_2020",
                              grepl("PPM", filename) ~ "Bukoski_et_al_2020",
                              grepl("Catanauan", filename) ~ "MacKenzie_et_al_2021",
                              grepl("PRE", filename) ~ "Bukoski_et_al_2020",
                              grepl("Koh", filename) ~ "Sharma_et_al_2021",
                              grepl("Prey", filename) ~ "Sharma_et_al_2021",
                              grepl("Rufiji", filename) ~ "Trettin_et_al_2020",
                              grepl("Zambezi", filename) ~ "Trettin_et_al_2020",
                              T ~ filename)) %>% 
  
  # swap out codes using references 
mutate(site_name = gsub(" ", "_", getsiteid[SITEID]), 
       habitat = getlandcov[LANDCOVID],
       ecosystem_condition = getecocond[ECOID],
       disturbance_class = getdisturb[DISTRUBID],
       geomorphic_id = getgeo[GEOID],
       year = year(MDATE),
       month = month(MDATE),
       day = day(MDATE),
       plot_shape = "circular",
       site_id = site_name, # paste(site_name, PLOTID, sep = "_"),
       plot_id = paste(site_id, SUBPID, sep = "_"),
       # plant_plot_detail_present = "yes",
       # detailed_plot_notes = "sapling plot area is nested within the total plot and is 12.57m2",
       # allometric_eq_present = "yes",
       # plant_measurements_present = "yes",
       field_or_manipulation_code = "field",
       harvest_or_allometry = "allometry") %>% 
       # area_unit = "m2") %>% 
       # diameter_method = "dbh",
       # diameter_2_method = "base",
       # diameter_3_method = "basal area",
       # diameter_unit = "centimeters",
       # diameter_2_unit = "centimeters",
       # diameter_3_unit = "m2",
  select_if(~!all(is.na(.))) %>% 
  select(study_id, site_id, plot_id, year, month, day, latitude, longitude, everything()) %>% 
  select(-c(SITEID, LANDCOVID, ECOID, DISTRUBID, GEOID, MDATE, PLOTID, PLOT, MEAS, PROJID, 
            AUTHOR, SUBP, DISTURBN, TOPOID, ID, CITID, subplot_latitude, subplot_longitude, 
            plot_latitude, plot_longitude))
  
  
  # plot level biomass summary across alive/dead and above/belowground
  # group_by(alive_or_dead, above_or_belowground, plot_id) %>% 
  # mutate(aboveground_biomass = case_when(alive_or_dead == "alive" & above_or_belowground == "aboveground" ~ sum(plant_mass_organic_matter),
  #                                 T ~ NA_character_),
  # aboveground_necromass = case_when(alive_or_dead == "dead" & above_or_belowground == "aboveground" ~ sum(plant_mass_organic_matter),
  #                                   T ~ NA_character_),
  # belowground_biomass = case_when(alive_or_dead == "alive" & above_or_belowground == "belowground" ~ sum(plant_mass_organic_matter),
  #                                 T ~ NA_character_)) %>% 
  # ungroup() %>% 
  
# use position formula created above to assign plot- or subplot- level code in "position_method"
# separate(col = "latitude", into = c("latitude", "position_method"), sep = "_") %>% 
#   separate(col = "longitude", into = "longitude", sep = "_") %>% 
#   mutate(latitude = as.numeric(latitude),
#          longitude = as.numeric(longitude),
#          plot_center_latitude = latitude,
#          plot_center_longitude = longitude) %>% 
  
# }


# Combind with tree-level observations
tree_table <- bind_rows(swamp_veg$Tree %>% mutate(plant_id = paste(ID, "tree", sep = "_")), 
                        swamp_veg$Sapling %>% mutate(plant_id = paste(ID, "sapling", sep = "_"))) %>% 
  select(-ID) %>% 

  # join study and plot ids
  full_join(clean_subplot %>% select(study_id, site_id, plot_id, filename, SUBPID)) %>% 
  
  # clean table
  rename(height = HGT,
         height_deadbreak = DEADBREAK_HGT,
         diameter = DBH,
         basal_diameter = DBASE,
         basal_area = BA) %>% 
  mutate(wood_density = getgrav[SGID, "SG"],
    wood_density_source = getsource[getgrav[SGID, "CITID"]],

    decay_class = ifelse(STATID == 1, 0,
                         ifelse(STATID == 2, 1,
                                ifelse(STATID == 3, 2,
                                       ifelse(STATID == 4, 3, NA_character_)))),
    alive_or_dead = case_when(decay_class > 0 ~ "dead",
                              decay_class == 0 ~ "alive",
                              T ~ NA_character_),
    n_plants = 1,
    height_unit = "meters",
    species_code = getspecies[SPECID],
    code_type = case_when(grepl("spp", species_code) ~ "Genus",
                          NA ~ NA_character_,
                          T ~ "Genus species")) %>% 
  select(study_id, site_id, plot_id, plant_id, everything()) %>% 
  select(-c(SAPN, SPECID, STATID, SGID))
    # separate genus and species codes
    # separate(species_code, into = c("genus", "species"), sep = " ") %>% 
    # mutate(species = case_when(grepl("spp", species) ~ NA_character_,
    #                            species == "NA" ~ NA_character_,
    #                            T ~ species),
    #        genus = case_when(genus == "NA" ~ NA_character_,
    #                          T ~ genus)) 

# group for plot level C summary and species count
# plot_smry <- tree_table %>% group_by(plot_id) %>%
#     summarize(tree_n = n(),
#            dominant_species = getmode(species_code))
  

no_plantobs <- tree_table %>% filter(is.na(plant_id)) %>% select_if(~!all(is.na(.)))

# Tree and Sapling Biomass 
tree_biomass <- bind_rows(swamp_veg$TreeBiomass %>% mutate(plant_id = paste(ID, "tree", sep = "_")), 
                          swamp_veg$SaplingBiomass %>% mutate(plant_id = paste(ID, "sapling", sep = "_"))) %>% 
  mutate(above_or_belowground = case_when(COMPID %in% c(1, 2, 3, 5) ~ "aboveground",
                                          COMPID == 7 ~ "belowground",
                                          T ~ NA_character_)) %>% 
  left_join(refs$Ref_Component, by = join_by(COMPID == ID)) %>%
  rename(allometry_notes = NOTES, 
         plant_mass_organic_carbon = C_CONT,
         plant_mass_organic_matter = BIOMASS) %>%
  mutate(allometric_eq_formula = geteq[EQID, "allometric_eq_formula"],
         allometric_eq_formula = case_when(allometric_eq_formula == "NA" ~ NA_character_,
                                           T ~ allometric_eq_formula),
         output_unit = geteq[EQID, "output_unit"],
         parameter_names = geteq[EQID, "parameter_names"],
         R2 = geteq[EQID, "R2"],
         RSE = geteq[EQID, "RSE"],
         wood_density_unit = "g/cm3",
         plant_mass_organic_matter_unit = "kilograms",
         plant_mass_organic_carbon_unit = "kilograms",
         allometric_eq_id = EQID,
         # allometric_eq_id_present = case_when(!is.na(EQID) ~ "yes", # all is yes
         #                                      T ~ "no"),
         diameter_max = geteq[EQID, "diameter_max"],
         diameter_min = geteq[EQID, "diameter_min"],
         location_description = geteq[EQID, "location_description"],
         carbon_conversion_factor = getconc[C_CONCID, "C"],
         carbon_conversion_factor_source = getsource[getconc[C_CONCID, "CITID"]]) %>% 
  select(filename, plant_id, everything()) %>% 
  select(-c(COMP_BIOGRPID, C_CONCID, EQID, ID, TREEID, COMPID, SAPID))

# Output Biomass tables for synthesis 
write_csv(clean_subplot, "data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plots.csv")
write_csv(tree_table, "data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plants.csv")

## Veg Tables ####
# plot_summary <- SWAMP_veg_converter(swamp_veg) %>% 
#   select(c(study_id, site_id, plot_id, plot_area, plot_shape, plot_center_latitude, plot_center_longitude,
#            elevation, year, month, day, field_or_manipulation_code, geomorphic_id, habitat, dominant_species, mean_height, 
#            plant_plot_detail_present)) %>% 
#   distinct()
# 
# plant_plot_detail <- SWAMP_veg_converter(swamp_veg) %>% 
#   select(c(study_id, site_id, plot_id, plot_area, area_unit, latitude, longitude, position_accuracy, position_method, elevation,
#            year, month, day, harvest_or_allometry, mass_n, detailed_plot_notes, allometric_eq_present, plant_measurements_present)) %>% 
#   distinct()
# 
# plant <- SWAMP_veg_converter(swamp_veg) %>% 
#   select(c(study_id, site_id, plot_id, plant_id, year, month, day, genus, species, alive_or_dead, above_or_belowground, n_plants, height, height_unit,
#            diameter, diameter_method, diameter_unit, diameter_2, diameter_2_method, diameter_2_unit, diameter_3, diameter_3_method, 
#            diameter_3_unit,wood_density, wood_density_unit, wood_density_source, plant_mass_organic_matter, plant_mass_organic_matter_unit, 
#            carbon_conversion_factor, carbon_conversion_factor_source, plant_mass_organic_carbon, plant_mass_organic_carbon_unit, 
#            allometric_eq_id, allometric_eq_id_present)) %>% 
#   drop_na(plant_id)
          
## Allometric Equations ####

allometric_eq <- tree_biomass %>% 
  left_join(tree_table %>% select(study_id, plant_id, species_code)) %>% 
  # SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, location_description, allometric_eq_id, allometric_eq_formula, species_code, output_unit, parameter_names, 
           R2, RSE, diameter_max, diameter_min)) %>% 
  drop_na(allometric_eq_formula) %>% 
  distinct()


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
  getlandcov <- tolower(ref_landcov$LANDCOV)
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
           
           # assign study_ids based on bibliography structure
           study_id = case_when(grepl("KRE", filename) ~ "Bukoski_et_al_2020",
                                grepl("PPM", filename) ~ "Bukoski_et_al_2020",
                                grepl("Catanauan", filename) ~ "MacKenzie_et_al_2021",
                                grepl("PRE", filename) ~ "Bukoski_et_al_2020",
                                grepl("Koh", filename) ~ "Sharma_et_al_2021",
                                grepl("Prey", filename) ~ "Sharma_et_al_2021",
                                grepl("Rufiji", filename) ~ "Trettin_et_al_2020",
                                T ~ filename)) %>% 
    separate(col = "latitude", into = c("latitude", "position_method"), sep = "_") %>% 
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
cores <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_accuracy, 
           position_method, elevation, habitat)) %>% 
  distinct() %>% 
  filter(habitat != "peatland") %>% # remove inland cores
  mutate(position_notes = case_when(position_method == "subplot" ~ "position at subplot level",
                                    position_method == "plot" ~ "posiiton at plot level"),
         position_method = "other low resolution") # recode plot and subplot as "other low resolution"

depthseries <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon)) %>%
  filter(study_id %in% cores$study_id) %>% 
  
  # fix instances of switched depth_min and depth_max in original data
  mutate(depth_max = case_when(core_id == "Catanauan_216_714" & depth_min == 49 ~ 49,
                               core_id == "Koh_Kohng_138_384" & depth_min == 191 ~ 191,
                               core_id == "Koh_Kohng_138_386" & depth_min == 184 ~ 184,
                               T ~ depth_max),
         depth_min = case_when(core_id == "Catanauan_216_714" & depth_max == 49 ~ 48,
                               core_id == "Koh_Kohng_138_384" & depth_max == 191 ~ 181,
                               core_id == "Koh_Kohng_138_386" & depth_max == 184 ~ 156,
                               T ~ depth_min))

impacts <- SWAMP_soil_converter(swamp_soil) %>% 
  select(c(study_id, site_id, core_id, impact_class)) %>% 
  distinct() %>% 
  filter(study_id %in% cores$study_id) %>% 
  mutate(impact_class = case_when(impact_class == "Restoration" ~ "restored",
                                  impact_class == "Intact" ~ "natural",
                                  impact_class == "Plantation" ~ "farmed",
                                  impact_class == "Degraded" ~ "degraded",
                                  TRUE ~ impact_class)) #merging vocab with controlled impact classes



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
                   "10.17528/CIFOR/DATA.00225", "10.17528/CIFOR/DATA.00221",
                   "10.17528/CIFOR/DATA.00238")

soil_bib_raw <- data.frame()
for (i in soil_doi_list) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(i))
  soil_bib_raw <- bind_rows(soil_bib_raw, temp_df)
}

soil_bib <- soil_bib_raw %>% 
  mutate(study_id = paste(gsub(" .*$", "", author), # extract first author name
                          ifelse(str_count(author, "and") > 1, "_et_al_", # if more than two authors, write "et_al"
                                 ifelse(str_count(author, "and") == 1, paste("_and_", sub(" .*$", "", sub(".*and ", "", author)), "_", sep = ""), # if two authors, insert second author's name
                                        ifelse(str_count(author, "and") < 1, "_", ""))), 
                          year,
                          sep = ""), 
         bibliography_id = case_when(grepl("Nevados", title) ~ "Hribljan_et_al_2020_losnevados", 
                                     grepl("Antisana", title) ~ "Hribljan_et_al_2020_antisana",
                                     grepl("Sajama", title) ~ "Hribljan_et_al_2020_sajama",
                                     grepl("Tuni", title) ~ "Hribljan_et_al_2020_tuni",
                                     grepl("Cayambe", title) ~ "Hribljan_et_al_2020_cayambe",
                                     grepl("Quilcayhuanca", title) ~ "Hribljan_et_al_2020_quilcayhuanca",
                                     grepl("Peru", title) ~ "Hribljan_et_al_2020_peru",
                                     grepl("Pastoruri", title) ~ "Hribljan_et_al_2020_pastoruri",
                                     grepl("Chingaza", title) ~ "Hribljan_et_al_2020_chingaza",
                                     grepl("Paramo", title) ~ "Hribljan_et_al_2020_paramo",
                                     grepl("Catanauan", title) & grepl("Soil", title) ~ "MacKenzie_et_al_2021_catanauan",
                                     grepl("Koh Kong", title) & grepl("Soil", title) ~ "Sharma_et_al_2021_kohkoh",
                                     grepl("Prey Nob", title) & grepl("Soil", title) ~ "Sharma_et_al_2021_preynob",
                                     grepl("Rufiji", title) & grepl("Soil", title) ~ "Trettin_et_al_2020_rufiji",
                                     grepl("Krabi", title) & grepl("Soil", title) ~ "Bukoski_et_al_2020_krabi",
                                     grepl("Pak", title) & grepl("Soil", title) ~ "Bukoski_et_al_2020_pak",
                                     grepl("Palian", title) & grepl("Soil", title) ~ "Bukoski_et_al_2020_palian",
                                     T ~ NA_character_),
         bibliography_id = case_when(!is.na(bibliography_id) ~ paste(bibliography_id, "data", "soil", sep = "_"),
                                     is.na(bibliography_id) ~ paste(study_id, "data", "soil", sep = "_"),
                                     T ~ NA_character_),
         publication_type = "primary dataset") 

veg_doi_list <- c("10.17528/CIFOR/DATA.00261", "10.17528/CIFOR/DATA.00255",
                  "10.17528/CIFOR/DATA.00253", "10.17528/CIFOR/DATA.00218",
                  "10.17528/CIFOR/DATA.00242", "10.17528/CIFOR/DATA.00241",
                  "10.17528/CIFOR/DATA.00219", "10.17528/CIFOR/DATA.00217")

veg_bib_raw <- data.frame()
for (n in veg_doi_list) {
  temp.df <- as.data.frame(GetBibEntryWithDOI(n))
  veg_bib_raw <- bind_rows(veg_bib_raw, temp.df)
}

veg_bib <- veg_bib_raw %>%
  mutate(study_id = paste(gsub(" .*$", "", author), # extract first author name
                          ifelse(str_count(author, "and") > 1, "_et_al_", # if more than two authors, write "et_al"
                                 ifelse(str_count(author, "and") == 1, paste("_and_", sub(" .*$", "", sub(".*and ", "", author)), "_", sep = ""), # if two authors, insert second author's name
                                        ifelse(str_count(author, "and") < 1, "_", ""))), 
                          year,
                          sep = ""), 
         bibliography_id = case_when(grepl("Catanauan", title) & grepl("Veg", title) ~ "MacKenzie_et_al_2021_catanauan",
                                     grepl("Koh Kong", title) & grepl("Veg", title) ~ "Sharma_et_al_2021_kohkoh",
                                     grepl("Prey Nob", title) & grepl("Veg", title) ~ "Sharma_et_al_2021_preynob",
                                     grepl("Rufiji", title) & grepl("Veg", title) ~ "Trettin_et_al_2020_rufiji",
                                     grepl("Krabi", title) &  grepl("Veg", title) ~ "Bukoski_et_al_2020_krabi",
                                     grepl("Pak", title) & grepl("Veg", title) ~ "Bukoski_et_al_2020_pak",
                                     grepl("Zambezi", title) ~ "Trettin_et_al_2020_zambezi",
                                     grepl("Palian", title) ~ "Bukoski_et_al_2020_palian",
                                     T ~ NA_character_),
         bibliography_id = case_when(!is.na(bibliography_id) ~ paste(bibliography_id, "data", "biomass", sep = "_"),
                                     is.na(bibliography_id) ~ paste(study_id, "data", "biomass", sep = "_"),
                                     T ~ NA_character_),
         publication_type = "primary dataset") 

study_citations <- full_join(veg_bib, soil_bib) %>% 
  mutate(author = gsub("[{]|[}]", "", author),
         # these four datasets share a single associated publication
         study_id = case_when(study_id == "Bukoski_2020" ~ "Bukoski_et_al_2020", 
                              study_id == "Bukoski_and_Elwin_2020" ~ "Bukoski_et_al_2020",
                              study_id == "MacKenzie_et_al_2020" ~ "Bukoski_et_al_2020",
                              study_id == "MacKenzie_et_al_2020" ~ "Bukoski_et_al_2020",
                              T ~ study_id)) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, title, author, doi, url, year) %>% 
  filter(study_id %in% cores$study_id)

bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


# Write curated data ####
write_csv(cores, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_cores.csv")
write_csv(depthseries, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_depthseries.csv")
write_csv(impacts, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_impacts.csv")
write_csv(plant, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plant.csv")
write_csv(plant_plot_detail, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plant_plot_detail.csv")
write_csv(plot_summary, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plot_summary.csv")
write_csv(allometric_eq, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_allometric_eq.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_study_citations.bib")
write_csv(study_citations, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_study_citations.csv")




