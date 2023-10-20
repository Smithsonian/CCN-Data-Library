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
  getlandcov <- tolower(ref_landcov$LANDCOV)
  names(getlandcov) <- ref_landcov$ID
  
  # tree species look up table
  ref_species <- read_excel(ref_path, sheet = "Ref_Species")
  getspecies <-  gsub("All species", "NA", ref_species$SCIENTIFIC_NAME) # No species listed for this code
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
                            mutate(plant_id = paste(ID, "tree", sep = "_")), # distinguish between tree and sapling plant IDs
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
                mutate(plant_id = paste(TREEID, "tree", sep = "_")),
              by = "plant_id") %>% 
    full_join(swamp_type$TreeSubp_C, by = c("filename", "SUBPID")) %>%  # add subplot-specific biomass data
    rename(AGC = TREE_AGC,
           BGC = TREE_BGC) %>% 
    filter(AGC > 0.00 & BGC > 0.00) # remove subplots without tree data
  
  sap_table <- right_join(subplot_table, swamp_type$Sapling %>% # repeat the same as above for sapling data
                            mutate(plant_id = paste(ID, "sapling", sep = "_")), 
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
                mutate(plant_id = paste(SAPID, "sapling", sep = "_")),
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
                                 NA ~ NA_character_,
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
  select(c(study_id, site_id, plot_id, dominant_species, year, month, day, ecotype, harvest_or_allometry,
           field_or_manipulation_code, longitude, latitude, position_accuracy, position_level_note,
           elevation, plant_count, subplot_AGC, subplot_BGC, plot_biomass_carbon, total_ecosystem_carbon)) %>% 
  distinct()

plant <- SWAMP_veg_converter(swamp_veg) %>% 
  select(c(study_id, site_id, plot_id, plant_id, species_code, code_type, decay_class, 
           ecosystem_condition, disturbance_class, disturbance_note, geomorphic_id, 
           height, diameter_base, diameter_breast_height, plant_AGB, plant_BGB, biomass_total, 
           biomass_decay_corrected, biomass_total_carbon, plant_AGC, plant_BGC, specific_gravity))


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
         bibliography_id = paste(study_id, "data", sep = "_"),
         title = substr(title, 9, nchar(title)),
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
         bibliography_id = paste(study_id, "data", sep = "_"),
         title = substr(title, 9, nchar(title)),
         publication_type = "primary dataset") 

study_citations <- full_join(veg_bib, soil_bib) %>% 
  mutate(title = gsub("[{][\\]textendash[}]", "-", title),
         # these four datasets share a single associated publication
         study_id = case_when(study_id == "Bukoski_2020" ~ "Bukoski_et_al_2020", 
                              study_id == "Bukoski_and_Elwin_2020" ~ "Bukoski_et_al_2020",
                              study_id == "MacKenzie_et_al_2020" ~ "Bukoski_et_al_2020",
                              study_id == "MacKenzie_et_al_2020" ~ "Bukoski_et_al_2020",
                              T ~ study_id),
         # assign a unique bibliography_id for each dataset
         bibliography_id = case_when(grepl("Aayambe", title) ~ "Hribljan_et_al_2020_cayambe", 
                                     grepl("Antisana", title) ~ "Hribljan_et_al_2020_antisana",
                                     grepl("Sajama", title) ~ "Hribljan_et_al_2020_sajama",
                                     grepl("Tuni", title) ~ "Hribljan_et_al_2020_tuni",
                                     grepl("Cayambe", title) ~ "Hribljan_et_al_2020_cayambe",
                                     grepl("Quilcayhuanca", title) ~ "Hribljan_et_al_2020_quilcayhuanca",
                                     grepl("Peru", title) ~ "Hribljan_et_al_2020_peru",
                                     grepl("Pastoruri", title) ~ "Hribljan_et_al_2020_pastoruri",
                                     grepl("Chingaza", title) ~ "Hribljan_et_al_2020_chingaza",
                                     grepl("Paramo", title) ~ "Hribljan_et_al_2020_paramo",
                                     grepl("Catanauan & Soil", title) ~ "MacKenzie_et_al_2021_catanauan_soil",
                                     grepl("Catanauan & Veg", title) ~ "MacKenzie_et_al_2021_catanauan_veg",
                                     grepl("Koh Kong & Soil", title) ~ "Sharma_et_al_2021_kohkoh_soil",
                                     grepl("Koh Kong & Veg", title) ~ "Sharma_et_al_2021_kohkoh_veg",
                                     grepl("Prey Nob & Soil", title) ~ "Sharma_et_al_2021_preynob_soil",
                                     grepl("Prey Nob & Veg", title) ~ "Sharma_et_al_2021_preynob_veg",
                                     grepl("Rufiji & Soil", title) ~ "Trettin_et_al_2020_rufiji_soil",
                                     grepl("Rufiji & Veg", title) ~ "Trettin_et_al_2020_rufiji_veg",
                                     grepl("Zambezi", title) ~ "Trettin_et_al_2020_zambezi",
                                     grepl("Palian", title) ~ "Bukoski_et_al_2020_palian",
                                     grepl("Krabi & Soil", title) ~ "Bukoski_et_al_2020_krabi_soil",
                                     grepl("Krabi & Veg", title) ~ "Bukoski_et_al_2020_krabi_veg",
                                     grepl("Pak & Soil", title) ~ "Bukoski_et_al_2020_pak_soil",
                                     grepl("Pak & Veg", title) ~ "Bukoski_et_al_2020_pak_veg",
                                     T ~ bibliography_id)) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, title, author, doi, url, year)

bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


# Write curated data ####
write_csv(cores, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_cores.csv")
write_csv(depthseries, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_depthseries.csv")
write_csv(impacts, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_impacts.csv")
write_csv(plant, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plant.csv")
write_csv(plot, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plot.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_study_citations.bib")
write_csv(study_citations, "./data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_study_citations.csv")


