## CCN Data Library ####

# Curation script for Messerschmidt et al 2020 data curation
# Contact: Henry Betts, BettsH@si.edu
# URL: https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.342
# also here: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-vcr.342.3

## Set up workspace
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


## Read in data ####
positions <- read_csv("./data/primary_studies/Messerschmidt_et_al_2020/original/Levee_Transects.csv") 
FOM <- read_csv("./data/primary_studies/Messerschmidt_et_al_2020/original/Levee_Cores.csv") 

## Curate ####
depthseries <- FOM %>% 
  rename(depth_min = Core_Top,
         depth_max = Core_Bot) %>% 
  mutate(fraction_organic_matter = org_matter_percent/100,
         study_id = "Messerschmidt_et_al_2020",
         method_id = "single set of methods",
         site_id = case_when(Site == "BF" ~ "Belvin_Farm",
                             Site == "CSL" ~ "Captain_Sinclair_Landward",
                             Site == "CSS" ~ "Captain_Sinclair_Seaward",
                             Site == "EP" ~ "Eagle_Point_Plantation",
                             Site == "KC" ~ "Kings_Creek",
                             T ~ NA_character_),
         core_id = paste(Site, Point_ID, sep = "_")) %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, fraction_organic_matter)


cores_raw <- positions %>% 
  rename(latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>% 
  mutate(site_id = case_when(SITE == "BF" ~ "Belvin_Farm",
                             SITE == "CSL" ~ "Captain_Sinclair_Landward",
                             SITE == "CSS" ~ "Captain_Sinclair_Seaward",
                             SITE == "EP" ~ "Eagle_Point_Plantation",
                             SITE == "KC" ~ "Kings_Creek",
                             T ~ NA_character_),
         species_code = case_when(grepl("alt|Alt", Veg_species) ~ "Spartina alterniflora",
                                  grepl("dist", Veg_species) ~ "Distichlis spicata",
                                  grepl("junc", Veg_species) ~ "Juncus roemerianus",
                                  grepl("pat", Veg_species) ~ "Spartina patens",
                                  grepl("sal|succ", Veg_species) ~ "Salicornia virginica",
                                  T ~ NA_character_),
         code_type = ifelse(!is.na(species_code), "Genus species", NA_character_),
         habitat = case_when(grepl("upland", Veg_species) ~ "upland",
                             grepl("bare|pond", Veg_species) ~ "unvegetated",
                             !is.na(species_code) ~ "marsh",
                             T ~ NA_character_),
         salinity_class = case_when(salinity < .5 ~ "fresh",
                                    salinity < 5 ~ "oligohaline",
                                    salinity < 18 ~ "mesohaline",
                                    T ~ NA_character_),
         salinity_method = ifelse(!is.na(salinity_class), "measurement", NA_character_),
         study_id = "Messerschmidt_et_al_2020",
         elevation_datum = "NAVD88",
         core_id = paste(SITE, Point_ID, sep = "_"),
         
         # lat/long switched for Belvin Farm
         BF_latitude = longitude,
         BF_longitude = latitude,
         latitude = case_when(core_id == "BF_116" ~ 37.29064, # approximate based on mapping pattern
                              core_id == "BF_418" ~ 37.29087, 
                              core_id == "EP_214" ~ 37.32131,
                              site_id == "Belvin_Farm" ~ BF_latitude,
                              T ~ latitude),
         longitude = case_when(core_id == "BF_116" ~ -76.40004,
                               core_id == "BF_418" ~ -76.39961, 
                               core_id == "EP_214" ~ -76.46033, 
                               site_id == "Belvin_Farm" ~ BF_longitude,
                               T ~ longitude),
         position_method = case_when(core_id == "BF_116" ~ "other low resolution", 
                                     core_id == "BF_418" ~ "other low resolution", 
                                     core_id == "EP_214" ~ "other low resolution", 
                                     T ~ "RTK"),
         position_notes = case_when(core_id == "BF_116" ~ "approximate coordinate chosen based on mapping pattern", 
                                    core_id == "BF_418" ~ "approximate coordinate chosen based on mapping pattern", 
                                    core_id == "EP_214" ~ "approximate coordinate chosen based on mapping pattern", 
                                    T ~ NA_character_)) %>% 
  filter(core_id %in% unique(depthseries$core_id))
  

cores <- cores_raw %>% 
  select(study_id, site_id, core_id, latitude, longitude, position_method, position_notes, elevation, elevation_datum,
         salinity_class, salinity_method, habitat)



species <- cores_raw %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)

study_citations <- data.frame(study_id = "Messerschmidt_et_al_2020", 
                              bibliography_id = "Messerschmidt_et_al_2020_data",
                              publication_type = "primary dataset", 
                              bibtype = "Misc", 
                              title = "Levee Soil Characteristics of Gloucester County, VA",
                              author = "Messerschmidt, T.C., ML. Kirwan and E. Hall",
                              doi = "10.6073/pasta/e2aeeef555de4ced1f3e8676131d6850", 
                              url = "https://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.342", 
                              year = 2020,
                              month = "sep",
                              day = 30,
                              publisher = "Virginia Coast Reserve Long-Term Ecological Research Project Data Publication")

bib_file <- study_citations %>%
  remove_rownames() %>%
  select(-c(study_id)) %>%
  column_to_rownames("bibliography_id")


## QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "species")

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
write_csv(cores, "./data/primary_studies/Messerschmidt_et_al_2020/derivative/Messerschmidt_et_al_2020_cores.csv")
write_csv(depthseries, "./data/primary_studies/Messerschmidt_et_al_2020/derivative/Messerschmidt_et_al_2020_depthseries.csv")
write_csv(species, "./data/primary_studies/Messerschmidt_et_al_2020/derivative/Messerschmidt_et_al_2020_species.csv")
write_csv(study_citations, "./data/primary_studies/Messerschmidt_et_al_2020/derivative/Messerschmidt_et_al_2020_study_citations.csv")
WriteBib(as.BibEntry(study_citations), "./data/primary_studies/Messerschmidt_et_al_2020/derivative/Messerschmidt_et_al_2020_study_citations.bib")


## Write Data Visualization Report ####
rmarkdown::render(input = "data/primary_studies/Messerschmidt_et_al_2020/Messerschmidt_et_al_2020_datavis.Rmd",
                  output_file = "Messerschmidt_et_al_2020_datavis.html",
                  output_dir = "docs/dataviz_reports/")



