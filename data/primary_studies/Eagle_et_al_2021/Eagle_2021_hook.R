## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data ingestion

# public data release: https://www.sciencebase.gov/catalog/item/60902e3fd34e93746a710491
# associated paper: https://www.frontiersin.org/articles/10.3389/ffgc.2021.676691/full

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
# library(rgdal)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_PR_age <- read_csv("data/primary_studies/Eagle_et_al_2021/original/Data_PR_AgeModel.csv")
raw_PR <- read_csv("data/primary_studies/Eagle_et_al_2021/original/Data_PR_Cores.csv")
raw_methods <- read_xlsx("data/primary_studies/Eagle_et_al_2021/intermediate/Eagle_2021_material_and_methods.xlsx")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Eagle_et_al_2021"

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>%
  drop_na(study_id) %>% 
  select_if(function(x) {!all(is.na(x))})

## ... Core Depthseries ####

dating_unit <- "disintegrationsPerMinutePerGram"

# d1 <- raw_PR %>% distinct(Site, Replicate, ID, Date, Lat, Lon, Depth_min, Depth_max)
# d2 <- raw_PR_age %>% distinct(Site, Replicate, ID, Date, Lat, Lon, Depth_min, Depth_max)

# expansion when the tables are joined because of different sampling intervals used to 
# measure different things in each core
eagle_ds <- full_join(raw_PR, raw_PR_age) %>%
  mutate(year = year(as.Date(Date, format = "%m/%d/%y")),
         month = month(as.Date(Date, format = "%m/%d/%y")),
         day = day(as.Date(Date, format = "%m/%d/%y"))) %>% 
  rename(longitude = Lon, 
         latitude = Lat,
         site_id = Site, 
         core_id = ID,
         depth_min = Depth_min,
         depth_max = Depth_max,
         dry_bulk_density = DBD,
         delta_c13 = `13C`,
         total_pb210_activity = `210Pb`, 
         total_pb210_activity_se = `210Pb_e`,
         excess_pb210_activity = `210Pbex`, 
         excess_pb210_activity_se = `210Pbex_e`,
         cs137_activity = `137Cs`,
         cs137_activity_se = `137Cs_e`,
         ra226_activity = `226Ra`,
         ra226_activity_se = `226Ra_e`,
         be7_activity = `7Be`,
         be7_activity_se = `7Be_e`,
         age = Year_50,
         age_min = Year_LL,
         age_max = Year_UL) %>% 
  mutate(study_id = id,
         method_id = "single set of methods",
         depth_interval_notes = ifelse(site_id == "San José Lagoon" & !is.na(dry_bulk_density), 
                                       "only one of the two replicates from the San José lagoon was processed for DBD due to human error, so DBD values from one SJ core were used in calculations of C storage and sequestration for both replicates from this site.",
                                       NA_character_),
         fraction_carbon = wtC/100,
         cs137_unit = ifelse(!is.na(cs137_activity), dating_unit, NA),
         pb210_unit = ifelse(!is.na(total_pb210_activity), dating_unit, NA),
         ra226_unit = ifelse(!is.na(ra226_activity), dating_unit, NA),
         be7_unit = ifelse(!is.na(be7_activity), dating_unit, NA))

depthseries <- eagle_ds %>% 
  select(-c(year, month, day, latitude, longitude, wtC, wtN, `15N`,
            Date, Replicate, Depth_mid, contains("_UL"), contains("_LL"),
            contains("_50"))) %>% 
  reorderColumns("depthseries", .)

ggplot(depthseries) +
  # geom_smooth() +
  # geom_point(aes(dry_bulk_density, depth_min, col = core_id), alpha = 0.5)
  geom_point(aes(dry_bulk_density, fraction_carbon))
  # facet_wrap(vars(core_id), scales = "free")

## ... Core-Level ####

cores <- eagle_ds %>%
  distinct(study_id, site_id, core_id, year, month, day, latitude, longitude) %>% 
  mutate(habitat = "mangrove",
         # inundation_class = "", # relative flushing doesn't necessarily indicate where the site is in tidal frame
         inundation_notes = case_when(site_id == "Martin Peña West" ~ "Dredged canal, relative flushing at this site is medium to high",
                                      site_id == "Martin Peña East" ~ "Clogged canal, relative flushing at this site is low",
                                      site_id == "Torrecilla Lagoon" ~ "Relative flushing at this site is medium to high",
                                      site_id == "San José Lagoon" ~ "Relative flushing at this site is medium",
                                      site_id == "Piñones" ~ "Relative flushing at this site is low"),
         vegetation_class = "forested",
         core_length_flag = "core depth limited by length of corer",
         position_accuracy = 5,
         position_method = "other moderate resolution",
         position_notes = "cellphone GPS") %>% 
  reorderColumns('cores', .)

## ... Impacts ####

impacts <- eagle_ds %>%
  select(study_id, site_id, core_id) %>% distinct() %>% 
  mutate(impact_class = case_when(site_id == "Martin Peña West" ~ "tidally restored",
                                  site_id == "Martin Peña East" ~ "tidally restricted",
                                  TRUE ~ NA_character_)) %>% 
  drop_na(impact_class)


## ... Species ####

species <- eagle_ds %>%
  select(study_id, site_id, core_id) %>% distinct() %>% 
  mutate(species_code = ifelse(site_id == "Piñones", "Avicennia germinans",
                          "Rhizophora mangle; Laguncularia racemosa; Avicennia germinans"),
         species_code = strsplit(species_code, split = "; "),
         code_type = "Genus species") %>% 
  unnest(species_code) %>% 
  resolveTaxa(.) %>% 
  select(-resolved_taxa, -name_updated)

## 2. QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

table_names <- c("methods", "cores", "depthseries", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names) # I think we'll be adding selected intervals to controlled variables
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

# Data visualization report
writeDataVizReport(id)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

data_bib <- as.data.frame(GetBibEntryWithDOI("10.5066/P97CAF30"))
pub_bib <- as.data.frame(ReadBib("data/primary_studies/Eagle_et_al_2021/original/Wigand_et_al_2021.bib"))

study_citations <- bind_rows(data_bib, pub_bib) %>%
  mutate(study_id = id,
         bibliography_id = c("Eagle_et_al_2021_data", "Wigand_et_al_2021_article"),
         publication_type = c("primary dataset", "associated source")) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021.bib")
write_csv(study_citations, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_study_citations.csv")

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_excel_csv(cores, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_depthseries.csv")
write_excel_csv(methods, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_methods.csv")
write_excel_csv(species, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_species.csv")
write_excel_csv(impacts, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_impacts.csv")


