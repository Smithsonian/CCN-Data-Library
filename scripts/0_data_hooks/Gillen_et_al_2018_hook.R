## CCN Data Library ####

# Curation script for Gillen et al 2018 data curation
# Contact: Henry Betts, BettsH@si.edu
# URL: https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.314
# also available here: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-vcr.314.2
# article: https://esurf.copernicus.org/articles/9/413/2021/

library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Notes:
# AGB data present
# cores - core_id problem with position data

## Read in data ####
agb <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_AboveGroundBiomass.csv", skip = 22)
gps <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_GPS.csv", skip = 22)
loi <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_LOI.csv", skip = 22)
bgb <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_BelowGroundBiomass.csv", skip = 22)
shear <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_ShearStrength.csv", skip = 22)
shear_avg <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_ShearStrength_AvgCompiled.csv", skip = 22)



## Curate ####
species <- agb %>% 
  rename(site_id = Site,
         species_code = Species_Name) %>% 
  mutate(study_id = "Gillen_et_al_2018",
         code_type = case_when(grepl("[.]", species_code) ~ "Genus species",
                               T ~ "Genus"),
         core_id = gsub("-", "_", SampleID),
         habitat = "marsh") %>% 
  filter(!species_code %in% c("Woody Stalk?", "Wild Rice")) %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)

depthseries <- loi %>% 
  rename(site_id = Site,
         dry_bulk_density = BD,
         depth_max = Depth) %>% 
  mutate(fraction_organic_matter = OM/100,
         core_id = paste(site_id, Location, sep = "_"),
         depth_min = case_when(site_id == "Goodwin" ~ depth_max - 1,
                               depth_max <= 53 ~ depth_max - 1,
                               depth_max > 53 ~ depth_max - 3,
                               T ~ NA),
         study_id = "Gillen_et_al_2018",
         method_id = "single set of methods") %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, WC)

cores <- gps %>% 
  rename(site_id = Site,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>% 
  mutate(core_id = paste(site_id, Code, sep = "_"),
         study_id = "Gillen_et_al_2018",
         position_method = "RTK",
         salinity_class = case_when(site_id == "Goodwin" ~ "polyhaline",
                                    site_id ==  "Pamunkey" ~ "fresh",
                                    T ~ NA_character_),
         habitat = "marsh") %>% 
  select(study_id, site_id, core_id, latitude, longitude, position_method, elevation, salinity_class, habitat) %>% 
  distinct()

methods <- data.frame(study_id = "Gillen_et_al_2018",
                      method_id = "single set of methods",
                      coring_method = "russian corer",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = 1,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 6)


uncontrolled_attributes <- data.frame(attribute_name = "WC",
                                      attribute_definition = "Soil water content",
                                      data_type = "numeric",
                                      units = "percent",
                                      study_id = "Gillen_et_al_2018")

study_citations <- data.frame(study_id = "Gillen_et_al_2018", 
                              bibliography_id = "Gillen_et_al_2018_data", 
                              publication_type = "primary dataset", 
                              bibtype = "Misc", 
                              title = "Shear Stress, Biomass, Bulk Density, Organic Matter on the Bank of the York River, VA 2018",
                              author = "Gillen, M.N., T. Messerschmidt and ML. Kirwan",
                              doi = "10.6073/pasta/beed4e91c44eb7297769158f60f898d4", 
                              url = "https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.314", 
                              year = 2018,
                              publisher = "Virginia Coast Reserve Long-Term Ecological Research Project Data Publication") 

bib_file <- study_citations %>%
  remove_rownames() %>%
  select(-c(study_id)) %>%
  column_to_rownames("bibliography_id")


## QAQC ####
leaflet(cores) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## Write files ####
write_csv(cores, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_cores.csv")
write_csv(depthseries, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_depthseries.csv")
write_csv(species, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_species.csv")
write_csv(methods, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_methods.csv")
write_csv(study_citations, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_study_citations.csv")
WriteBib(as.BibEntry(study_citations), "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_study_citations.bib")
