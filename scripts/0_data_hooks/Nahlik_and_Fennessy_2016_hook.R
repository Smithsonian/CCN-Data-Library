# Authors:
# Amanda M. Nahlik <nahlik.amanda@epa.gov>
# Siobhan Fennessy

# Hook script for carbon stock dataset referenced in Nahlik and Fennessy (2016)

library(tidyverse)
library(lubridate)
library(RefManageR)
library(stringr)
# library(here)

# Download data ####

# Read in data from NWCA 

# condition report (impacts)
condition <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")
condition_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")

# site information (site)
siteinfo <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo.csv")
siteinfo_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo-meta.txt")

# soil chemistry (cores/depthseries)
soilchem <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem.csv")
soilchem_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem-meta.txt")

# vegetation (species)
veg <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr.csv")
veg_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr-meta.txt")

# hydrology
# hydro <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro.csv")
# hydro_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro-meta.txt")

# citations
study_citations <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/Nahlik_Fennessy_2016_citations.csv")

# write original data
# write_csv(condition, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_cond_stress.csv")
# write_csv(soilchem, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_soilchem.csv")
# write_csv(siteinfo, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_siteinfo-meta.csv")
# write_csv(veg, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_plant_pres_cvr.csv")

# Data Curation #### 

data_release <- "Nahlik_and_Fennessy_2016"

## Site ----
site_marine <- siteinfo %>%
  filter(SANDT_RSTUDY == "Coastal Watersheds" &
           !is.na(ANALYSIS_LAT)) %>%
  filter(MAJ_RIVER_BASIN != "Great Lakes" &
           SANDT_COAST_REG != "Great Lakes Region") %>%
  mutate(study_id = data_release, 
         core_date = dmy(DATE_COL),
         core_id = paste0(SITE_ID, "-", UID),
         salinity_class = recode(CLASS_FIELD_FWSST,
                                 "E2EM" = "estuarine",
                                 "E2SS" = "estuarine",
                                 "PEM" = "palustrine",
                                 "PFO" = "palustrine",
                                 "PSS" = "palustrine",
                                 "PF" = "palustrine",
                                 "PUBPAB" = "palustrine"),
         salinity_method = "field observation",
         vegetation_class = recode(CLASS_FIELD_FWSST,
                                   "E2EM" = "emergent",
                                   "E2SS" = "forested to shrub",
                                   "PEM" = "emergent",
                                   "PFO" = "forested",
                                   "PSS" = "scrub shrub",
                                   "PF" = NA_character_,
                                   "PUBPAB" = NA_character_),
         vegetation_method = "field observation") %>%
  rename(site_id = SITE_ID, 
         core_latitude = ANALYSIS_LAT,
         core_longitude = ANALYSIS_LON) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, 
         salinity_class, salinity_method, vegetation_class, vegetation_method,
         COE_REGION, CLASS_FIELD_FWSST, CLASS_FIELD_HGM, HUC10_NAME, MAJ_RIVER_BASIN, 
         SANDT_RSTUDY, SANDT_COAST_REG, STATE_NAME)

# create site_description from relevant columns
# mutate(site_description = paste(site_name, site_raw$site_description, sep="; "))

# extract inundation class and merge with site data
site <- site_marine %>%
  mutate(site_description = paste0(HUC10_NAME, "; ", MAJ_RIVER_BASIN)) %>%
  select(study_id, site_id, site_description, salinity_class, salinity_method, 
         vegetation_class, vegetation_method) %>%
  distinct()
  # mutate(core_id = paste0(SITE_ID, "-", UID),
  #        inundation_notes = "field observation") %>%
  # filter(core_id %in% unique(site_marine$core_id)) %>%
  # rename and recode indundation class and notes 
  # use high_table, estuary channel, or estuary surge presence?
  # select(core_id, HIGH_TABLE, HIGH_TABLE_COMMENT, 
  #        ESTCHANNEL_PRESENT, ESTCHANNEL_PRESENT_COMMENT, 
  #        ESTSURGE_PRESENT, ESTSURGE_PRESENT_COMMENT,
  #        TIDAL_STAGE, inundation_notes) %>%
  # left_join(site_marine, ., by = "core_id") %>%
  # select(-core_longitude, -core_latitude)

## Soil chemistry ----

soil_marine <- soilchem %>%
  mutate(core_id = paste0(SITE_ID, "-", UID),
         study_id = data_release, 
         fraction_carbon = TOT_CARBON/100,
         core_date = as.Date(DATE_COL, format = "%m/%d/%Y")) %>%
  filter(core_id %in% unique(site_marine$core_id)) %>%
  rename(site_id = SITE_ID, 
         sample_id = SAMPLE_ID, 
         dry_bulk_density = BULK_DEN_DBF) %>%
  select(study_id, site_id, core_id, sample_id, core_date, fraction_carbon,
         dry_bulk_density, BULK_DEN_DBF_FLAG, DEPTH, DEPTH_FLAG, EC)

## Cores 
# core_position, core_elevation, core_length_flag?

cores <- soil_marine %>%
  left_join(., site_marine %>% 
              select(site_id, core_latitude, core_longitude, 
                     salinity_class, salinity_method, 
                     vegetation_class, vegetation_method), 
            by = "site_id") %>%
  mutate(core_year = year(core_date),
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(study_id, site_id, core_id, core_year, core_month, core_day, core_latitude, core_longitude,
         salinity_class, salinity_method, vegetation_class, vegetation_method) %>%
  distinct()

## Depthseries

depthseries <- soil_marine %>%
  rename("dry_bulk_density_notes" = "BULK_DEN_DBF_FLAG",
         "depth_max" = "DEPTH") %>%
  mutate(depth_min = c(0, depth_max[1:(nrow(.)-1)]),
         dry_bulk_density_notes = gsub("Db", "Dry bulk density", dry_bulk_density_notes),
         dry_bulk_density_notes = gsub(", g/cc", "", dry_bulk_density_notes),
         dry_bulk_density_notes = recode(dry_bulk_density_notes,
                                         "g/cc" = NA_character_),
         depth_interval_notes = paste0(str_to_sentence(DEPTH_FLAG), " ", dry_bulk_density_notes),
         depth_interval_notes = gsub("NA", "",depth_interval_notes), 
         depth_interval_notes = trimws(depth_interval_notes)) %>%
  select(study_id, site_id, core_id, sample_id, depth_min, depth_max, 
         dry_bulk_density, fraction_carbon, depth_interval_notes)

# test <- depthseries %>%
#   add_count(dry_bulk_density_notes, name = "count") %>%
#   select(dry_bulk_density_notes, count) %>%
#   distinct() %>%
#   mutate(percent = (count/sum(count))*100)

## Species ----

# use this lookup to fill NA species codes in data
# species_lookup <- veg %>% 
#   select(SPECIES, SPECIES_NAME_ID) %>%
#   rename("species_code" = "SPECIES_NAME_ID") %>%
#   distinct() %>%
#   drop_na()

species <- veg %>%
  mutate(core_id = paste0(SITE_ID, "-", UID),
         study_id = data_release) %>%
  filter(core_id %in% unique(site_marine$core_id)) %>%
  # left_join(., species_lookup, by = "SPECIES") %>%
  separate(col = SPECIES, into = c("genus", "species"), sep = " ") %>%
  mutate(species = replace_na(species, "spp"),
         species = recode(species,
                          "S.L." = "spp")) %>%
  filter(species != "(>.5M)" & species != "(GRASS") %>%
  rename(site_id = SITE_ID,
         species_comment = SPECIES_COMMENT) %>%
  add_count(core_id, genus, species, name = "count") %>%
  filter(count > 5) %>%
  mutate(species_code = str_to_sentence(paste0(genus, " ", species))) %>%
  select(study_id, site_id, core_id, species_code) %>%
  distinct()

# lookup to fill NA species notes in data
# sp_notes_lookup <- species_raw %>%
#   select(core_id, genus, species, species_comment) %>%
#   rename("species_notes" = "species_comment") %>%
#   drop_na()
# 
# species <- species_raw %>%
#   left_join(., sp_notes_lookup, 
#             by = c("core_id", "genus", "species")) %>%
#   select(study_id, site_id, core_id, genus, species, species_code, count, species_notes) %>%
#   filter(species != "(>.5M)" & species != "(GRASS") %>%
#   mutate(genus = str_to_title(genus),
#          species = tolower(species), 
#          species_notes = str_to_sentence(species_notes)) %>%
#   distinct() 

# Impact ----

impacts <- condition %>%
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
                               "STRESS_FILL" = "sediment added", 
                               "STRESS_HARD" = "hardening", 
                               "STRESS_NONNATIVE" = "nonnative vegetation",
                               "STRESS_HEAVYMETAL" = "heavy metal", 
                               "STRESS_SOILP" = "trace phosphorus", 
                               "STRESS_ALGT" = "algal toxin")) %>%
  select(-stressor_class) %>%
  distinct()

# Methods ----
methods <- data.frame(study_id = "Nahlik_and_Fennessy_2016",
                      coring_method = "push core",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = 2, 
                      dry_bulk_density_flag = "modeled",
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      fraction_carbon_method = "EA", 
                      fraction_carbon_type = "organic carbon")

## Citations ####

# Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), 
         "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_and_Fennessy_2016.bib")


## QA/QC #####

# map the data
library(maps)

map <- ggplot() +
  geom_polygon(aes(long, lat, group = group), data = map_data("usa"), fill = "grey50") +
  coord_quickmap()

# Points should all be coastal
map + geom_point(data = site_marine, 
                 aes(x = core_longitude, y = core_latitude, col = CLASS_FIELD_FWSST),
                 size = 0.5) +
  theme_classic()
# ggsave("locations_class_field_fwsst.jpg")

map + geom_point(data = site_marine, 
                 aes(x = core_longitude, y = core_latitude, col = CLASS_FIELD_HGM),
                 size = 0.5) +
  theme_classic()
# ggsave("locations_class_field_hgm.jpg")

map + geom_point(data = site_marine, 
                 aes(x = core_longitude, y = core_latitude, col = salinity_class),
                 size = 0.5) +
  theme_classic()
# ggsave("locations_class_field_salinity.jpg")

source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("depthseries", depthseries)
test_colnames("species", species)
test_colnames("methods", methods)

## Output Curated Data ####

# write to derivative folder in data dir
write.csv(cores, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_cores.csv",
          row.names = FALSE)
write.csv(depthseries, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_depthseries.csv",
          row.names = FALSE)
write.csv(species, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_species.csv",
          row.names = FALSE)
write.csv(impacts, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_impacts.csv",
          row.names = FALSE)
write.csv(site, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_site.csv",
          row.names = FALSE)
write.csv(methods, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_methods.csv",
          row.names = FALSE)
