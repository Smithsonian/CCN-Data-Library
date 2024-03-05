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

# DATA DOWNLOAD WORKFLOW ARCHIVED 

# # condition report (impacts)
# condition <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress.csv")
# condition_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_cond_stress-meta.txt")
# 
# # site information (site)
# siteinfo <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo.csv")
# siteinfo_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_siteinfo-meta.txt")
# 
# # soil chemistry (cores/depthseries)
# soilchem <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem.csv")
# soilchem_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_soilchem-meta.txt")
# 
# # vegetation (species)
# veg <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr.csv")
# veg_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_plant_pres_cvr-meta.txt")
# 
# # hydrology
# # hydro <- read_csv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro.csv")
# # hydro_md <- read_tsv("https://www.epa.gov/sites/production/files/2016-10/nwca2011_hydro-meta.txt")
# 
# 
# # write original data
# # write_csv(condition, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_cond_stress.csv")
# # write_csv(soilchem, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_soilchem.csv")
# # write_csv(siteinfo, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_siteinfo-meta.csv")
# # write_csv(veg, "./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_plant_pres_cvr.csv")

# read in data
condition <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_cond_stress.csv")
soilchem <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_soilchem.csv")
siteinfo <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_siteinfo-meta.csv")
veg <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/nwca2011_plant_pres_cvr.csv")

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
                                   # PF = palustrine farmed
                                   "PF" = NA_character_,
                                   # PUBPAB = palustrine unconsolidated bottom/aquatic bed
                                   "PUBPAB" = NA_character_),
         vegetation_method = "field observation") %>%
  # Filter so that all Estuarine salinities and all
  # CLASS_FIELD_HGM == TIDAL
  filter(salinity_class == "estuarine" | CLASS_FIELD_HGM == "TIDAL") %>% 
  rename(site_id = SITE_ID, 
         core_latitude = ANALYSIS_LAT,
         core_longitude = ANALYSIS_LON) %>%
  select(study_id, site_id, core_id, core_date, core_latitude, core_longitude, 
         salinity_class, salinity_method, vegetation_class, vegetation_method,
         COE_REGION, CLASS_FIELD_FWSST, CLASS_FIELD_HGM, HUC10_NAME, MAJ_RIVER_BASIN, 
         SANDT_RSTUDY, SANDT_COAST_REG, STATE_NAME)

sites <- site_marine %>%
  # create site_description from relevant columns
  mutate(site_description = paste0(HUC10_NAME, "; ", MAJ_RIVER_BASIN)) %>%
  select(study_id, site_id, site_description, salinity_class, salinity_method, 
         vegetation_class, vegetation_method) %>%
  distinct()


#renaming salinity class to fit guidance 
  # "palustrine" -> "palustrine C-CAP", in the current guidance, there is no controlled variable for "palustrine"
#sites <- sites %>% 
 # mutate(salinity_class = ifelse(salinity_class == "palustrine", "palustrine C-CAP", salinity_class))


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
              select(site_id, core_id, core_latitude, core_longitude, 
                     salinity_class, salinity_method, 
                     vegetation_class, vegetation_method), 
            by = c("site_id", "core_id")) %>%
  mutate(core_year = year(core_date),
         core_month = month(core_date),
         core_day = day(core_date),
         core_length_flag = "not specified") %>% 
  select(study_id, site_id, core_id, core_year, core_month, core_day, core_latitude, core_longitude,
         salinity_class, salinity_method, vegetation_class, vegetation_method, core_length_flag) %>%
  distinct()

#updating salinity class palustrine to fit current guidance
#cores <- cores %>% 
  #mutate(salinity_class = ifelse(salinity_class == "palustrine", "palustrine C-CAP", salinity_class))

## Depthseries

depthseries <- soil_marine %>%
  rename("dry_bulk_density_notes" = "BULK_DEN_DBF_FLAG",
         "representative_depth_max" = "DEPTH") %>%
  group_by(study_id, site_id, core_id) %>% 
  mutate(representative_depth_min = ifelse(row_number() == 1, 
                            0, 
                            lag(representative_depth_max))) %>%
  ungroup() %>% 
  mutate(dry_bulk_density_notes = gsub("Db", "Dry bulk density", dry_bulk_density_notes),
         dry_bulk_density_notes = gsub(", g/cc", "", dry_bulk_density_notes),
         dry_bulk_density_notes = recode(dry_bulk_density_notes,
                                         "g/cc" = NA_character_),
         depth_interval_notes = paste0(str_to_sentence(DEPTH_FLAG), " ", dry_bulk_density_notes),
         depth_interval_notes = gsub("NA", "",depth_interval_notes), 
         depth_interval_notes = trimws(depth_interval_notes),
         method_id = "single set of methods") %>%
  select(study_id, site_id, core_id, method_id, sample_id, representative_depth_min, representative_depth_max, 
         dry_bulk_density, fraction_carbon, depth_interval_notes)

depthseries$depth_interval_notes[which(depthseries$depth_interval_notes == "")] <- NA

# test <- depthseries %>%
#   add_count(dry_bulk_density_notes, name = "count") %>%
#   select(dry_bulk_density_notes, count) %>%
#   distinct() %>%
#   mutate(percent = (count/sum(count))*100)

## Species ----

species <- veg %>%
  mutate(core_id = paste0(SITE_ID, "-", UID),
         study_id = data_release) %>%
  filter(core_id %in% unique(site_marine$core_id)) %>%
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
                               "STRESS_VEGREPLACE" = "revegetated",
                               "STRESS_FILL" = "sediment added")) %>%
  filter(!(impact_class %in% c("STRESS_HARD", "STRESS_NONNATIVE", "STRESS_HEAVYMETAL",
                           "STRESS_SOILP", "STRESS_ALGT"))) %>%
  select(-stressor_class) %>%
  distinct()

# Methods ----
methods <- data.frame(study_id = "Nahlik_and_Fennessy_2016",
                      method_id = "single set of methods",
                      coring_method = "soil pit",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = 2,
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      fraction_carbon_method = "EA", 
                      fraction_carbon_type = "organic carbon")

## Citations ####

study_citations_raw <- read_csv("./data/primary_studies/Nahlik_Fennessy_2016/original/Nahlik_Fennessy_2016_citations.csv")

study_citations <- study_citations_raw %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Format bibliography
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_and_Fennessy_2016.bib")
write_csv(study_citations, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_study_citations.csv")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "sites", "species", "impacts")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names) # palustrine
testRequired(table_names) # depth min and max (although representative depth is present)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

library(leaflet)
leaflet(cores) %>%
 addTiles() %>%
 addCircles(lng = ~longitude, lat = ~latitude)

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
write.csv(sites, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_sites.csv",
          row.names = FALSE)
write.csv(methods, "./data/primary_studies/Nahlik_Fennessy_2016/derivative/Nahlik_Fennessy_2016_methods.csv",
          row.names = FALSE)
