## CCN Data Library ####

## Soil core data curation script for Hamzeh and Lahijani 2022
## contact: Henry Betts, BettsH@si.edu
## URL: https://doi.org/10.1007/s12237-021-01037-7


## 1. Set up ####

# Load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

# Load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# Link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# Read in data
cores_raw <- read_xlsx("./data/primary_studies/Hamzeh_and_Lahijani_2022/original/12237_2021_1037_MOESM1_ESM.xlsx", sheet = 1)
soil_raw <- read_xlsx("./data/primary_studies/Hamzeh_and_Lahijani_2022/original/12237_2021_1037_MOESM1_ESM.xlsx", sheet = 2)
# veg_raw <- read_xlsx("./data/primary_studies/Hamzeh_and_Lahijani_2022/original/12237_2021_1037_MOESM1_ESM.xlsx", sheet = 3) # This only contains data redundant to cores_raw  
methods_raw <- read_xlsx("./data/primary_studies/Hamzeh_and_Lahijani_2022/original/Hamzeh_and_Lahijani_2022_methods.xlsx", sheet = 2)

## 2: Clean data ####
depthseries <- soil_raw %>% 
  slice_head(n = 413) %>% 
  separate(Depth, into = c("depth_min", "depth_max"), sep = "-") %>% 
  rename(elevation = Elev.) %>% 
  mutate(study_id = "Hamzeh_and_lahijani_2022",
         method_id = "single set of methods",
         site_id = Cre,
         dbd_fom_fc = paste(`Drydensity (1)`, "/", `LOI(1)`, "/", `Carbon1(%)`,"_1,", 
                            `Drydensity (2)`, "/", `LOI(2)`, "/", `Carbon2(%)`, "_2,",
                            `Drydensity (3)`, "/", `LOI(3)`, "/", `Carbon3(%)`, "_3")) %>% 
  separate_rows(dbd_fom_fc, sep = ",") %>% 
  mutate(core_id = ifelse(grepl("_1", dbd_fom_fc), paste(site_id, "_1", sep = ""),
                          ifelse(grepl("_2", dbd_fom_fc), paste(site_id, "_2", sep = ""),
                                 ifelse(grepl("_3", dbd_fom_fc), paste(site_id, "_3", sep = ""), site_id)))) %>% 
  separate(dbd_fom_fc, into = c("dry_bulk_density", "fraction_organic_matter", "fraction_carbon"), sep = "/") %>% 
  mutate(fraction_carbon = as.numeric(gsub("_\\d", "", fraction_carbon))/100,
         dry_bulk_density = as.numeric(gsub(" ", "", dry_bulk_density))/100,
         fraction_organic_matter = as.numeric(gsub(" ", "", fraction_organic_matter))/100) %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter) # Drop fraction_carbon (modeled)
  
elevation_grab <- soil_raw %>% 
  slice_head(n = 413) %>% 
  separate(Depth, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(top_elevation = ifelse(depth_min == 0, Elev., NA)) %>% 
  drop_na(top_elevation)

cores <- cores_raw %>% 
  rename(site_id = Name,
         latitude = Y,
         longitude = X,
         habitat = Habitat) %>% 
  mutate(date = as.Date(date, "%d.%m.%Y"),
         core_id = paste(site_id, "_1,", site_id, "_2,", site_id, "_3", sep = ""), # Expand out core_ids to map the site-level position data
         habitat = gsub("dwarf", "mangrove", habitat),
         year = year(date),
         month = month(date),
         day = day(date),
         study_id = "Hamzeh_and_lahijani_2022",
         core_length_flag = "core depth represents deposit depth",
         habitat = substr(habitat, 4, nchar(habitat)),
         position_method = "other low resolution",
         position_notes = "position data is provided only at the site level") %>% 
  separate_rows(core_id, sep = ",") %>% 
  left_join(elevation_grab, c('site_id' = 'Cre')) %>% 
  rename(elevation = top_elevation) %>% 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, elevation, position_notes, 
         habitat, core_length_flag)

species <- cores %>% 
  mutate(species_code = "Avicennia marina",
         code_type = "Genus species") %>% 
  select(study_id, site_id, core_id, species_code, code_type, habitat)

methods <- methods_raw %>% 
  select(where(notAllNA))

  
## 3. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "species", "methods")

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
testIDs(cores, depthseries, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)


## 4. Bibliography ####
study_citation <- data.frame(study_id = "Hamzeh_and_lahijani_2022",
                             bibliography_id = "Hamzeh_and_lahijani_2022_data",
                             title = "Soil and Vegetative Carbon Sequestration in Khuran Estuary Mangroves, Strait of Hormoz, During the Last 18 Centuries",
                             author = "Hamzeh, M. A.; Lahijani, H. A.",
                             publication_type = "primary dataset",
                             doi = "10.1007/s12237-021-01037-7",
                             url = "https://doi.org/10.1007/s12237-021-01037-7",
                             bibtype = "Misc",
                             year = "2022",
                             month = "jan",
                             day = "16")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


## 5. Write curated data ####
write_csv(cores, "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_cores.csv") 
write_csv(depthseries, "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_depthseries.csv")
write_csv(species, "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_species.csv")
write_csv(methods, "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_methods.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_study_citations.bib")
write_csv(study_citation, "data/primary_studies/Hamzeh_and_Lahijani_2022/derivative/Hamzeh_and_Lahijani_2022_study_citations.csv")



