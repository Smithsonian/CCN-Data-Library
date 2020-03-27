# Author: Jaxine Wolfe

# Hook script for carbon stock dataset referenced in Nahlik and Fennessey (2017)

library(tidyverse)
library(lubridate)
# library(here)

# Download data ####

# scrape data from web
condition <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")
condition_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")

# site information
siteinfo <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo.csv")
siteinfo_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo-meta.txt")

# soil chemistry
soilchem <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem.csv")
soilchem_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem-meta.txt")

# vegetation
veg <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr.csv")
veg_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr-meta.txt")

# hydrology
hydro <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro.csv")
hydro_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro-meta.txt")

# write original data
write_csv(condition, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_cond_stress.csv")
write_csv(soilchem, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_soilchem.csv")
write_csv(siteinfo, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_siteinfo-meta.csv")
write_csv(veg, "./data/primary_studies/Nahlik_Fennessey_2017/original/nwca2011_plant_pres_cvr.csv")

# Data Curation #### 

data_release <- "Nahlik_and_Fennessey_2017"

# Site ####
site_marine <- siteinfo %>%
  filter(SANDT_RSTUDY == "Coastal Watersheds" &
           !is.na(ANALYSIS_LAT)) %>%
  mutate(study_id = data_release, 
         core_id = paste0(SITE_ID, "-", UID),
         salinity_class = recode(CLASS_FIELD_FWSST,
                                 "E2EM" = "estuarine",
                                 "E2SS" = "estuarine",
                                 "PEM" = "palustrine",
                                 "PFO" = "palustrine",
                                 "PSS" = "palustrine",
                                 "PF" = "palustrine",
                                 "PUBPAB" = "palustrine"),
         vegetation_class = recode(CLASS_FIELD_FWSST,
                                   "E2EM" = "emergent",
                                   "E2SS" = "forested to shrub",
                                   "PEM" = "emergent",
                                   "PFO" = "forested",
                                   "PSS" = "scrub shrub",
                                   "PF" = NA_character_,
                                   "PUBPAB" = NA_character_)) %>%
  rename(site_id = SITE_ID, 
         core_latitude = ANALYSIS_LAT,
         core_longitude = ANALYSIS_LON,
         core_date = DATE_COL) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, salinity_class, vegetation_class,
         COE_REGION, CLASS_FIELD_FWSST, CLASS_FIELD_HGM, HUC10_NAME, MAJ_RIVER_BASIN, 
         SANDT_RSTUDY, SANDT_COAST_REG)

site_marine$core_date <- dmy(site_marine$core_date)  

# site <- site_marine %>%
#   select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, salinity_class, vegetation_class,
#          COE_REGION, CLASS_FIELD_FWSST, CLASS_FIELD_HGM, HUC10_NAME, MAJ_RIVER_BASIN, 
#          SANDT_RSTUDY, SANDT_COAST_REG)

# Soil chemistry ####
soil_marine <- soilchem %>%
  mutate(core_id = paste0(SITE_ID, "-", UID),
         study_id = data_release) %>%
  filter(core_id %in% unique(site_marine$core_id)) %>%
  rename(site_id = SITE_ID, 
         core_date = DATE_COL,
         sample_id = SAMPLE_ID, 
         fraction_carbon = TOT_CARBON,
         dry_bulk_density = BULK_DEN_DBF) %>%
  select(study_id, site_id, core_id, sample_id, core_date, fraction_carbon,
         dry_bulk_density, BULK_DEN_DBF_FLAG, DEPTH, DEPTH_FLAG, EC)

soil_marine$core_date <- as.Date(soil_marine$core_date, format = "%m/%d/%Y")

# Cores ####

cores <- soil_marine %>%
  left_join(., site_marine %>% 
              select(site_id, core_latitude, core_longitude, salinity_class, vegetation_class), 
            by = "site_id") %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude,
         salinity_class, vegetation_class) %>%
  distinct()

# Depthseries ####

depthseries <- soil_marine %>%
  select(study_id, site_id, core_id, sample_id, DEPTH, DEPTH_FLAG, 
         dry_bulk_density, BULK_DEN_DBF_FLAG, fraction_carbon)

# Species ####
species_marine <- plants %>%
  mutate(core_id = paste0(SITE_ID, "-", UID),
         study_id = data_release) %>%
  filter(core_id %in% unique(site_marine$core_id) &
           COVER > 20.0) %>%
  select(study_id, SITE_ID, core_id, SPECIES, SPECIES_NAME_ID, SPECIES_COMMENT) %>%
  separate(col = SPECIES, into = c("genus", "species"), sep = " ") %>%
  mutate(species = replace_na(species, "spp.")) %>%
  rename(site_id = SITE_ID,
         species_code = SPECIES_NAME_ID,
         species_notes = SPECIES_COMMENT) %>%
  distinct()

# species_marine$DATE_COL <- dmy(species_marine$DATE_COL)

# Impact ####

impact_marine <- condition %>%
  filter(SITE_ID %in% unique(site_marine$site_id)) %>%
  mutate(study_id = data_release) %>%
  rename(site_id = SITE_ID) %>%
  select(study_id, site_id, contains("STRESS")) %>%
  gather(impact_class, stressor_class, contains("STRESS")) %>%
  filter(stressor_class == "High" |
           stressor_class == "Very High") %>%
  mutate(impact_class = recode(impact_class,
                               "STRESS_DAM" = "impounded",
                               "STRESS_DITCH" = "ditched",
                               "STRESS_VEGREMOVAL" = "invasive plants removed",
                               "STRESS_VEGREPLACE" = "revegitated",
                               "STRESS_FILL" = "sediment added")) %>%
  distinct()

# Methods ####
methods <- data.frame(study_id = "Nahlik_and_Fennessey_2017",
                      coring_method = "push core",
                      sediment_sieve_size = "sediment sieved", 
                      carbon_measured_or_modeled = "?",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      fraction_carbon_method = "EA")

# Investigate Data ####
# map the data
library(maps)

# I suspect NA's are mostly inland
# site_na <- siteinfo %>%
#   filter(is.na(SANDT_RSTUDY) &
#            !is.na(AA_CENTER_LAT))

map <- ggplot() +
  geom_polygon(aes(long, lat, group = group), data = map_data("usa"), fill = "grey50") +
  coord_quickmap()

# Points should all be coastal
map + geom_point(data = site_marine, aes(x = ANALYSIS_LON, y = ANALYSIS_LAT, col = CLASS_FIELD_FWSST)) +
  theme_classic()

map + geom_point(data = site_marine, aes(x = ANALYSIS_LON, y = ANALYSIS_LAT, col = CLASS_FIELD_HGM)) +
  theme_classic()

map + geom_point(data = site_marine, aes(x = ANALYSIS_LON, y = ANALYSIS_LAT, col = salinity_class)) +
  theme_classic()


## Output Curated Data ####

# write to /final in data dir
write.csv(cores, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_cores.csv",
          row.names = FALSE)
write.csv(depthseries, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_depthseries.csv",
          row.names = FALSE)
write.csv(species_marine, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_species.csv",
          row.names = FALSE)
write.csv(impact_marine, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_impact.csv",
          row.names = FALSE)
# write.csv(site, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_site.csv",
          # row.names = FALSE)
# write.csv(methods, "./data/primary_studies/Nahlik_Fennessey_2017/intermediate/Nahlik_Fennessey_2017_methods.csv",
#           row.names = FALSE)
