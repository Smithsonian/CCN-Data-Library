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

## Read in data ####
agb <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_AboveGroundBiomass.csv", skip = 22)
gps <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_GPS.csv", skip = 22)
loi <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_LOI.csv", skip = 22)
bgb <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_BelowGroundBiomass.csv", skip = 22)
shear <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_ShearStrength.csv", skip = 22)
shear_avg <- read_csv("./data/primary_studies/Gillen_et_al_2018/original/MGillen_ShearStrength_AvgCompiled.csv", skip = 22)



## Curate ####
depthseries <- loi %>% 
  rename(dry_bulk_density = BD,
         depth_max = Depth) %>% 
  mutate(site_id = paste(Site, Location, sep = "_"),
         fraction_organic_matter = OM/100,
         core_id = site_id,
         depth_min = case_when(site_id == "Goodwin" ~ depth_max - 1,
                               depth_max <= 53 ~ depth_max - 1,
                               depth_max > 53 ~ depth_max - 3,
                               T ~ NA),
         study_id = "Gillen_et_al_2018",
         method_id = "single set of methods") %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)

species <- agb %>% 
  rename(site_id = Site,
         species_code = Species_Name) %>% 
  mutate(study_id = "Gillen_et_al_2018",
         species_code = case_when(species_code == "S. alterniflora" ~ "Spartina alterniflora",
                                  species_code == "D. spicata" ~ "Distichlis spicata",
                                  species_code == "S. patens" ~ "Spartina patens",
                                  species_code == "Z. aquatica" ~ "Zizania aquatica",
                                  species_code == "P. virginica" ~ "Plantago virginica",
                                  species_code == "S. cynosauroides" ~ "Sporobolus cynosuroides",
                                  species_code == "S. americanus" ~ "Schoenoplectus americanus",
                                  species_code == "B. laevis" ~ "Bidens laevis",
                                  species_code == "Polygnum" ~ "Polygonum species",
                                  species_code == "Polygonum" ~ "Polygonum species",
                                  species_code == "Scirpus" ~ "Scirpus species",
                                  species_code == "Peltandra" ~ "Peltandra species",
                                  T ~ species_code),
         code_type = case_when(grepl("species", species_code) ~ "Genus",
                               T ~ "Genus species"),
         core_id = gsub("-", "_", SampleID),
         habitat = "marsh") %>% 
  filter(!species_code %in% c("Woody Stalk?", "Wild Rice")) %>% 
  # filter(core_id %in% unique(depthseries$core_id)) %>% # keep cores not in depthseries table for AGB reference
  select(study_id, site_id, core_id, species_code, code_type, habitat)

cores <- gps %>% 
  rename(latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>% 
  mutate(site_id = paste(Site, Code, sep = "_"),
         core_id = site_id) %>% 
  group_by(site_id, core_id) %>% 
  summarise(elevation = mean(elevation),
            latitude = mean(latitude),
          # n_cores_taken = n(),
            longitude = mean(longitude)) %>% 
  mutate(study_id = "Gillen_et_al_2018",
         position_method = "RTK",
         elevation_method = "RTK",
         elevation_notes = "elevation selected as mean of a cluster of 3 to 14 cores taken at each location",
         position_notes = "coordinates selected as mean of a cluster of 3 to 14 cores taken at each location",
         salinity_class = case_when(site_id == "Goodwin" ~ "polyhaline",
                                    site_id ==  "Pamunkey" ~ "fresh",
                                    T ~ NA_character_),
         habitat = "marsh") %>% 
  # filter(core_id %in% unique(depthseries$core_id)) %>% # keep cores not in depthseries table for AGB reference
  select(study_id, site_id, core_id, latitude, longitude, position_method, 
         position_notes, elevation, elevation_method, elevation_notes, salinity_class, habitat) 

methods <- data.frame(study_id = "Gillen_et_al_2018",
                      method_id = "single set of methods",
                      coring_method = "russian corer",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = 1,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 6)

plot_raw <- agb %>% 
  mutate(site_id = paste(Site, Location, sep = "_")) %>% 
  left_join(cores) %>% 
  rename(plot_center_latitude = latitude,
         plot_center_longitude = longitude,
         plot_id = SampleID) %>% 
  mutate(plant_plot_detail_present = "yes",
         soil_core_present = "yes") 

plot_summary <- plot_raw %>% 
  select(study_id, site_id, plot_id, plot_center_latitude, plot_center_longitude, position_notes, elevation,
         elevation_method, elevation_notes, habitat, plant_plot_detail_present, soil_core_present)

plant_plot_detail <- plot_raw %>% 
  rename(latitude = plot_center_latitude,
         longitude = plot_center_longitude,
         mass = DryWeight,
         mass_n = N_Stems) %>% 
  mutate(harvest_or_allometry = "harvest",
         genus = case_when(Species_Name == "S. alterniflora" ~ "Spartina",
                           Species_Name == "D. spicata" ~ "Distichlis",
                           Species_Name == "S. patens" ~ "Spartina",
                           Species_Name == "Z. aquatica" ~ "Zizania",
                           Species_Name == "P. virginica" ~ "Plantago",
                           Species_Name == "S. cynosauroides" ~ "Sporobolus",
                           Species_Name == "S. americanus" ~ "Schoenoplectus",
                           Species_Name == "B. laevis" ~ "Bidens",
                           Species_Name == "Polygnum" ~ "Polygonum",
                           Species_Name == "Polygonum" ~ "Polygonum",
                           Species_Name == "Scirpus" ~ "Scirpus",
                           Species_Name == "Peltandra" ~ "Peltandra",
                           T ~ Species_Name),
         species = case_when(Species_Name == "S. alterniflora" ~ "alterniflora",
                             Species_Name == "D. spicata" ~ "spicata",
                             Species_Name == "S. patens" ~ "patens",
                             Species_Name == "Z. aquatica" ~ "aquatica",
                             Species_Name == "P. virginica" ~ "virginica",
                             Species_Name == "S. cynosauroides" ~ "cynosuroides",
                             Species_Name == "S. americanus" ~ "americanus",
                             Species_Name == "B. laevis" ~ "laevis",
                             Species_Name == "Polygnum" ~ "species",
                             Species_Name == "Polygonum" ~ "species",
                             Species_Name == "Scirpus" ~ "species",
                             Species_Name == "Peltandra" ~ "species",
                             T ~ Species_Name),
         above_or_belowground = "above",
         allometric_eq_present = "no",
         plant_measurements_present = "no") %>% 
  select(study_id, site_id, plot_id, longitude, latitude, position_method, position_notes, elevation, elevation_method,
         elevation_notes, harvest_or_allometry, genus, species, above_or_belowground, mass, mass_n, allometric_eq_present, 
         plant_measurements_present)
  
           

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
table_names <- c("cores", "depthseries", "methods", "plot_summary", "plant_plot_detail")

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
write_csv(methods, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_methods.csv")
write_csv(study_citations, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_study_citations.csv")
write_csv(plot_summary, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_plot_summary.csv")
write_csv(plant_plot_detail, "./data/primary_studies/Gillen_et_al_2018/derivative/Gillen_et_al_2018_plant_plot_detail.csv")


## Write Data Visualization Report ####
rmarkdown::render(input = "data/primary_studies/Gillen_et_al_2018/Gillen_et_al_2018_datavis.Rmd",
                  output_file = "Gillen_et_al_2018_datavis.html",
                  output_dir = "data/primary_studies/Gillen_et_al_2018")
