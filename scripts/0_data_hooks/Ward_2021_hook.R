## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data ingestion

# public data release:
# associated paper: 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
# library(rgdal)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
ward_ea <- read_csv("data/primary_studies/Ward_2021/original/PublishedEAData.csv")
ward_cores <- read_csv("data/primary_studies/Ward_2021/original/PublishedCoreData.csv")
# raw_methods <- read_xlsx("data/primary_studies/Ward_et_al_2021/intermediate/Ward_2021_material_and_methods.xlsx")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Ward_et_al_2021"

## ... Methods ####

# curate materials and methods
# methods <- raw_methods %>%
#   drop_na(study_id) %>% 
#   select_if(function(x) {!all(is.na(x))})

## ... Core Data ####

# In addition, we analyzed sediment OC sources for two individual salt marshes
# in Elkhorn Slough (“Elkhorn salt marsh”) and Tomales Bay (“Walker Salt Marsh”)
isotopes <- ward_ea %>%
  rename(site_id = Site,
         depth_min = Top_interval_cm,
         delta_c13 = dC13,
         habitat = Habitat_type) %>% 
  mutate(core_notes = ifelse(Under_wrack_YN == "Yes", 
                             "sediment samples were collected from in areas of salt marsh beneath persient seagrass wrack lines", NA_character_)) %>% 
  select(-Under_wrack_YN, -Surface_Deep_10cm)
# Pan cores were collected from patches of unvegetated sediment
# There's no ID's for these sampling intervals, making it impossible to relate them to the depthseries data...
  
# need to create core IDs => create a sequence from the depthseries
# will these be able to relate the isotope to the rest of the soil data?
# total of 82 sediment cores, 30 discussed previously in O'Donnell 2017
# what is the core date?

soil_data <- ward_cores %>% 
  rename(site_id = Site,
         habitat = `Habitat Type`,
         dry_bulk_density = `Bulk Density (g/cm3)`,
         depth_min = `Top Interval (cm)`) %>% 
  separate(Coordinates, into = c("latitude", "longitude"), sep = ", ") %>% 
  mutate(study_id = id,
         core_id = str_c(site_id, habitat, sep = "_"),
         fraction_organic_matter = `TOM (%)`/100,
         fraction_carbon = `OC (%)`, # organic carbon
         longitude = gsub(" ", "", longitude),
         depth_max = depth_min + 2)
# Newport Bay should have bare sed instead of salt marsh (according to the paper)

# write an if statement to assign core_id
new_id <- 1
# assign the first ID
soil_data$core_id[1] <- str_c(soil_data$core_id[1], new_id, sep = "_")

for(i in 2:nrow(soil_data)){
  
    if(soil_data$depth_min[i - 1] < soil_data$depth_min[i]){
      soil_data$core_id[i] <- str_c(soil_data$core_id[i], new_id, sep = "_")
    } 
    if(soil_data$depth_min[i - 1] > soil_data$depth_min[i]){
      new_id <- new_id + 1
      soil_data$core_id[i] <- str_c(soil_data$core_id[i], new_id, sep = "_")
    }
}
  
# recreate paper figure
ggplot(soil_data, aes(`Mud (%)`, `TOM (%)`, col = habitat)) +
  geom_point(size = 0.5)

## ... Core Depthseries ####

depthseries <- soil_data %>% 
  select(-c(contains("%"), `OC (kg/m3)`, latitude, longitude, habitat)) %>%
  reorderColumns("depthseries", .)

ggplot(depthseries) +
  # geom_smooth() +
  geom_point(aes(dry_bulk_density, fraction_carbon))
# geom_point(aes(depth_min, dry_bulk_density)) +
# facet_wrap(vars(core_id), scales = "free")

## ... Core-Level ####


# there should be 82 cores
cores <- soil_data %>% 
  # lat/lon causing expansion due to excel dragging
  mutate(longitude = case_when(core_id == "Elkhorn Slough_Pan_67" ~ "-121.101",
                               core_id == "Elkhorn Slough_Salt Marsh_64" ~ "-121.77",
                               core_id == "Elkhorn Slough_Salt Marsh_65" ~ "-121.84",
                               core_id == "Elkhorn Slough_Salt Marsh_66" ~ "-121.92",
                               core_id == "Elkhorn Slough_Salt Marsh_68" ~ "-121.111",
                               core_id == "Elkhorn Slough_Salt Marsh_69" ~ "-121.123",
                               core_id == "Elkhorn Slough_Salt Marsh_70" ~ "-121.131",
                               core_id == "Tomales Bay_Salt Marsh_61" ~ "-122.926",
                               core_id == "Tomales Bay_Salt Marsh_62" ~ "-122.934",
                               core_id == "Tomales Bay_Salt Marsh_63" ~ "-122.946",
                               TRUE ~ longitude)) %>%
  distinct(study_id, site_id, core_id, latitude, longitude, habitat) %>% 
  reorderColumns('cores', .)

## ... Impacts ####

# None of our sampling sites were
# actively restored, and, to our knowledge, respective vegetation has persisted through time.

## ... Species ####

# site level species provided in paper

## 2. QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 3, label = ~core_id)

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

# if(!file.exists("data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_study_citations.csv")){
# Read in bibtex files
data_bib <- as.data.frame(GetBibEntryWithDOI("10.5066/P97CAF30"))
pub_bib <- as.data.frame(ReadBib("data/primary_studies/Ward_2021/original/Ward_2021_study_citation.bib"))

study_citations <- bind_rows(data_bib, pub_bib) %>%
  mutate(study_id = id,
         bibliography_id = c("Ward_et_al_2021_data", "Ward_et_al_2021_article"),
         publication_type = c("primary dataset", "associated source")) %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021.bib")
# write_csv(study_citations, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
# write_csv(cores, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_cores.csv") 
# write_csv(depthseries, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_depthseries.csv")
# write_csv(methods, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_methods.csv")
# write_csv(sites, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_sites.csv")
# write_csv(species, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_species.csv")
# write_csv(impacts, "data/primary_studies/Ward_et_al_2021/derivative/Ward_et_al_2021_impacts.csv")


