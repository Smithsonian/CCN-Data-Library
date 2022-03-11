## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data ingestion

# public data release: https://datadryad.org/stash/dataset/doi:10.5061/dryad.m0cfxpp31
# associated paper: https://bg.copernicus.org/articles/18/4717/2021/

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
ward_ea <- read_csv("data/primary_studies/Ward_2021/original/PublishedEAData.csv")
# ward_cores <- read_csv("data/primary_studies/Ward_2021/original/PublishedCoreData.csv")
ward_cores_updated <- read_csv("data/primary_studies/Ward_2021/original/JaxineData.csv")
raw_methods <- read_xlsx("data/primary_studies/Ward_2021/intermediate/Ward_2021_methods.xlsx", sheet = 2)

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Ward_et_al_2021"

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>% 
  slice(2) %>%
  select_if(function(x) {!all(is.na(x))})

## ... Core Data ####

# C isotope data
# In addition, we analyzed sediment OC sources for two individual salt marshes
# in Elkhorn Slough (“Elkhorn salt marsh”) and Tomales Bay (“Walker Salt Marsh”)

# There's no ID's for these sampling intervals, 
# making it impossible to relate them to the depthseries data...
isotopes <- ward_ea %>%
  rename(site_id = Site,
         depth_min = Top_interval_cm,
         delta_c13 = dC13,
         habitat = Habitat_type) %>% 
  mutate(core_notes = ifelse(Under_wrack_YN == "Yes", 
                             "sediment samples were collected from in areas of salt marsh beneath persient seagrass wrack lines", NA_character_)) %>% 
  select(-Under_wrack_YN, -Surface_Deep_10cm)
# Pan cores were collected from patches of unvegetated sediment in salt marshes
  
# Melissa provided an updated table wiht core IDs, the following workflow is archived
# need to create core IDs => create a sequence from the depthseries

# soil_data <- ward_cores %>% 
#   rename(site_id = Site,
#          habitat = `Habitat Type`,
#          dry_bulk_density = `Bulk Density (g/cm3)`,
#          depth_min = `Top Interval (cm)`) %>% 
#   separate(Coordinates, into = c("latitude", "longitude"), sep = ", ") %>% 
#   mutate(study_id = id,
#          core_id = str_c(site_id, habitat, sep = "_"),
#          fraction_organic_matter = `TOM (%)`/100,
#          fraction_carbon = `OC (%)`/100, # organic carbon
#          longitude = gsub(" ", "", longitude),
#          depth_max = depth_min + 2)
# Newport Bay should have bare sed instead of salt marsh (according to the paper)

# write an if statement to assign core_ids based on depth intervals
# assign the first ID
# new_id <- 1
# soil_data$core_id[1] <- str_c(soil_data$core_id[1], new_id, sep = "_")
# 
# for(i in 2:nrow(soil_data)){
#   
#     if(soil_data$depth_min[i - 1] < soil_data$depth_min[i]){
#       soil_data$core_id[i] <- str_c(soil_data$core_id[i], new_id, sep = "_")
#     } 
#     if(soil_data$depth_min[i - 1] > soil_data$depth_min[i]){
#       new_id <- new_id + 1
#       soil_data$core_id[i] <- str_c(soil_data$core_id[i], new_id, sep = "_")
#     }
# }
  
# new data was sent with Core IDs...
soil_data <- ward_cores_updated %>% 
  rename(site_id = Site,
         habitat = `Habitat Type`,
         dry_bulk_density = `Bulk Density (g/cm3)`,
         depth_min = `Top Interval (cm)`) %>% 
  separate(Coordinates, into = c("latitude", "longitude"), sep = ", ") %>% 
  mutate(study_id = id,
         core_id = str_c(site_id, CoreIndexNum, sep = " "),
         fraction_carbon = as.numeric(PercCarbOrg_EA)/100, # measurements labeled "Fail" coerced to NA
         fraction_organic_matter = `TOM (%)`/100,
         fraction_carbon_modeled = `OC (%)`/100, # organic carbon
         longitude = gsub(" ", "", longitude),
         depth_max = depth_min + 2)

# recreate paper figure
ggplot(soil_data, aes(`Mud (%)`, `TOM (%)`, col = habitat)) +
  geom_point(size = 0.5)

ggplot(soil_data) +
  geom_point(aes(dry_bulk_density, fraction_carbon, col = habitat)) +
  # geom_point(aes(depth_min, dry_bulk_density)) +
  facet_wrap(~habitat)

# plot carbon values measured against modeled
ggplot(soil_data) +
  geom_point(aes(fraction_organic_matter, fraction_carbon_modeled, col = "modeled")) +
  geom_point(aes(fraction_organic_matter, fraction_carbon, col = "measured")) +
  ylab("Fraction Carbon")

## ... Core Depthseries ####

# discard modeled values
depthseries <- soil_data %>% 
  select(-c(contains("%"), contains("_EA"), CoreIndexNum, fraction_carbon_modeled,
            `OC (kg/m3)`, latitude, longitude, habitat)) %>%
  reorderColumns("depthseries", .)

## ... Core-Level ####

# total of 82 sediment cores, 30 discussed previously in O'Donnell 2017
cores <- soil_data %>% 
  # lat/lon causing expansion due to excel dragging...
  mutate(longitude = case_when(core_id == "Elkhorn Slough 86" ~ "-121.101", # "Elkhorn Slough_Pan_67"
                               core_id == "Elkhorn Slough 83" ~ "-121.77", # "Elkhorn Slough_Salt Marsh_64"
                               core_id == "Elkhorn Slough 84" ~ "-121.84", #"Elkhorn Slough_Salt Marsh_65"
                               core_id == "Elkhorn Slough 85" ~ "-121.92", # "Elkhorn Slough_Salt Marsh_66"
                               core_id == "Elkhorn Slough 87" ~ "-121.111", # "Elkhorn Slough_Salt Marsh_68"
                               core_id == "Elkhorn Slough 88" ~ "-121.123", # "Elkhorn Slough_Salt Marsh_69"
                               core_id == "Elkhorn Slough 89" ~ "-121.131", # "Elkhorn Slough_Salt Marsh_70"
                               core_id == "Tomales Bay 80" ~ "-122.926", # "Tomales Bay_Salt Marsh_61"
                               core_id == "Tomales Bay 81" ~ "-122.934", # "Tomales Bay_Salt Marsh_62"
                               core_id == "Tomales Bay 82" ~ "-122.946", # "Tomales Bay_Salt Marsh_63"
                               TRUE ~ longitude)) %>%
  distinct(study_id, site_id, core_id, latitude, longitude, habitat) %>% 
  mutate(vegetation_class = case_when(habitat == "Seagrass" ~ "seagrass",
                                      habitat == "Salt Marsh" ~ "emergent",
                                      TRUE ~ NA_character_),
         vegetation_notes = case_when(habitat == "Bare sed" ~ "unvegetated sediments near seagrass meadows",
                                      habitat == "Pan" ~ "unvegetated sediments near salt marshes",
                                      TRUE ~ NA_character_),
         habitat = recode(habitat, "Seagrass" = "seagrass",
                          "Salt Marsh" = "marsh",
                          "Bare sed" = "unvegetated",
                          "Pan" = "unvegetated"),
         position_method = "handheld",
         position_notes = ifelse(habitat == "seagrass", "treat as site-level; cores likely taken by SCUBA, with coordinates taken from boat", NA),
         inundation_class = "low",
         core_length_flag = "core depth limited by length of corer") %>%
  reorderColumns('cores', .)
# need core year at least, I think 2019 for some?
# 

## ... Impacts ####

# None of our sampling sites were actively restored, and, 
# to our knowledge, respective vegetation has persisted through time.

## ... Species ####

# site level species provided in paper
species <- soil_data %>% 
  select(study_id, site_id, habitat) %>% distinct() %>% 
  filter(habitat != "Pan" & habitat != "Bare sed") %>% 
  mutate(species_code = case_when(habitat == "Seagrass" ~ "Zostera marina",
                                  habitat == "Salt Marsh" ~ "Sarcocornia pacifica, Distichlis spicata, Jaumea carnosa"),
         species_code = strsplit(species_code, split = ", "),
         code_type = "Genus species") %>% 
  unnest(species_code) %>%  select(-habitat)

## 2. QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 3, label = ~core_id)

table_names <- c("methods", "cores", "depthseries", "species")

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
data_bib <- data.frame(study_id = id,
                       bibtype = "Misc",
                       author = "Melissa Ward",
                       title = "Data from: Organic carbon, grain size, elemental/isotopic composition",
                       year = "2021",
                       month = "8",
                       url = "https://doi.org/10.5061/dryad.m0cfxpp31",
                       doi = "10.5061/dryad.m0cfxpp31")

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

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021.bib")
write_csv(study_citations, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_cores.csv")
write_csv(depthseries, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_depthseries.csv")
write_csv(methods, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_methods.csv")
# write_csv(sites, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_sites.csv")
write_csv(species, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_species.csv")
# write_csv(impacts, "data/primary_studies/Ward_2021/derivative/Ward_et_al_2021_impacts.csv")


