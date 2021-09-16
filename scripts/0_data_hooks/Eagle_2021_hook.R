## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data releases
# Puerto Rico cores: https://www.sciencebase.gov/catalog/item/60902e3fd34e93746a710491

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
         fraction_carbon = wtC/100)
# San Jose Lagoon cores have the same DBD

depthseries <- eagle_ds %>% 
  select(-c(year, month, day, latitude, longitude, wtC, wtN, `15N`,
            Date, Replicate, Depth_mid, contains("_UL"), contains("_LL"),
            contains("_50"))) %>% 
  reorderColumns("depthseries", .)

ggplot(depthseries) +
  # geom_smooth() +
  geom_point(aes(dry_bulk_density, fraction_carbon))
  # geom_point(aes(depth_min, dry_bulk_density)) +
  # facet_wrap(vars(core_id), scales = "free")

## ... Core-Level ####

# site info
# Martin Peña West: a mangrove wetland in a dredged canal along the west end of the Caño de Martín Peña. Relative flushing at this site is medium to high.
# Martin Peña East: a mangrove wetland in a clogged canal along the east end of the Caño de Martín Peña, toward the Laguna San José. Relative flushing at this site is low.
# Torrecilla Lagoon: a mangrove wetland in the Torrecilla Lagoon. Relative flushing at this site is medium to high.
# San José Lagoon: a mangrove wetland in the José Lagoon. Relative flushing at this site is medium.
# Piñones: a mangrove wetland within the Piñones Forested Reserve. Relative flushing at this site is low.

cores <- eagle_ds %>%
  distinct(study_id, site_id, core_id, year, month, day, latitude, longitude) %>% 
  mutate(habitat = "mangrove",
         # inundation_class = "", # can relative flushing indicate where the site is in tidal frame?
         inundation_notes = case_when(site_id == "Martin Peña West" ~ "Dredged canal, relative flushing at this site is medium to high.",
                                      site_id == "Martin Peña East" ~ "Clogged canal, relative flushing at this site is low.",
                                      site_id == "Torrecilla Lagoon" ~ "Relative flushing at this site is medium to high.",
                                      site_id == "San José Lagoon" ~ "Relative flushing at this site is medium.",
                                      site_id == "Piñones" ~ "Relative flushing at this site is low."),
         vegetation_class = "forested",
         core_length_flag = "core depth limited by length of corer",
         position_accuracy = 5,
         position_method = "other moderate resolution",
         position_notes = "cellphone GPS") %>% 
  reorderColumns('cores', .)

## ... Impacts ####

impacts <- eagle_ds %>%
  distinct(study_id, site_id, core_id) %>% 
  mutate(impact_class = case_when(site_id == "Martin Peña West" ~ "tidally restored",
                                  site_id == "Martin Peña East" ~ "tidally restricted",
                                  TRUE ~ NA_character_)) %>% 
  drop_na(impact_class)


## ... Species ####

species <- eagle_ds %>%
  distinct(study_id, site_id, core_id) %>% 
  mutate(species_code = ifelse(site_id == "Piñones", "Avicennia germinans",
                          "Rhizophora mangle; Laguncularia racemosa; Avicennia germinans"),
         species_code = strsplit(species_code, split = "; ")) %>% 
  unnest(species_code)


## 2. QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

# if(!file.exists("data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_study_citations.csv")){
# Create bibtex file
dois <- c("10.5066/P97CAF30", "10.3389/ffgc.2021.676691")

data_bibs <- GetBibEntryWithDOI(dois)

study_citations <- as.data.frame(data_bibs) %>%
  mutate(study_id = id,
         bibliography_id = "Eagle_et_al_2021_data",
         publication_type = "primary dataset") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021.bib")
# write_csv(study_citations, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
# write_csv(cores, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_cores.csv") 
# write_csv(depthseries, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_depthseries.csv")
# write_csv(methods, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_methods.csv")
# write_csv(sites, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_sites.csv")
# write_csv(species, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_species.csv")
# write_csv(impacts, "data/primary_studies/Eagle_et_al_2021/derivative/Eagle_et_al_2021_impacts.csv")


