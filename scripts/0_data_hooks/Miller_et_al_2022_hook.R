## CCN Data Library ####

## Soil core data curation script for Rodriguez et al 2022
## contact: Henry Betts, BettsH@si.edu

## Associated article: Carbon accumulation rates are highest at young and expanding salt marsh edges
## DOI: https://doi.org/10.1038/s43247-022-00501-x

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(sf)
library(leaflet)
library(skimr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html


## Step 1: Setting up the datasets ####

cores_raw <- read_xlsx("./data/primary_studies/Miller_et_al_2022/original/Supplementary_Data.xlsx", sheet = 1)
depthseries_raw <- read_xlsx("./data/primary_studies/Miller_et_al_2022/original/Supplementary_Data.xlsx", sheet = 2)
species_raw <- read_csv("./data/primary_studies/Miller_et_al_2022/intermediate/Table_1.csv", skip = 1) # this is the converted .csv file (.../Miller_et_al_2022/intermediate) made from the original .docx (.../Miller_et_al_2022/original)
methods_raw <- read_csv("./data/primary_studies/Miller_et_al_2022/intermediate/Methods.csv")

## Cores
cores_set_up <- cores_raw %>% 
  fill(Site, `Core Number`) %>% 
  separate(col = 'Site', c(NA, 'site_id'), sep = '; ') %>% 
  rename(core_id = `Core Number`,
         elevation_cores = `Elevation (m; NAVD88)`,
         c14_age = 'Radiocarbon Age (yr BP)',
         c14_age_se = 'Age Error') %>% 
  unite(col = core_id, site_id : core_id, sep = '_', remove = FALSE) %>% 
  distinct(core_id, .keep_all = TRUE) %>% # this removes all of the duplicate rows due to multiple calibrated age ranges per core ID
  mutate(core_id = str_replace_all(string = core_id, pattern = '-', replacement = '_'),
         site_id = str_replace_all(string = site_id, pattern = '-', replacement = '_'),
         study_id = "Miller_et_al_2022",
         elevation_datum = "NAVD88",
         elevation_method = 'RTK',
         salinity_method = 'field observation',
         vegetation_class = "emergent",
         vegetation_method = "field observation",
         core_length_flag = 'core depth represents deposit depth',
         habitat = "marsh",
         salinity_class = 'estuarine',
         inundation_method = 'field observation',
         inundation_class = ifelse(site_id %in% c("SRE_U", "SRE_D", "NPR_D"), "low", "high"),
         position_method = "RTK",
         elevation_accuracy = "0.025",
         position_accuracy = "0.01",
         #c14_age_se = na_if(c14_age_se, 'n/a'),
         #c14_age = na_if(c14_age, 'n/a'),
         #c14_material = ifelse(is.na(c14_age), "NA", "plant fragment")
         )
  
# C14 age table
c14_table <- cores_raw %>% 
  fill(Site, `Core Number`) %>% 
  separate(col = 'Site', c(NA, 'site_id'), sep = '; ') %>% 
  rename(core_id = `Core Number`,
         c14_age = 'Radiocarbon Age (yr BP)',
         c14_age_se = 'Age Error') %>% 
  unite(col = core_id, site_id : core_id, sep = '_', remove = FALSE) %>% 
  distinct(core_id, .keep_all = TRUE) %>% 
  mutate(study_id = "Miller_et_al_2022",
         method_id = "single set of methods",
         c14_age_se = na_if(c14_age_se, 'n/a'),
         c14_age = na_if(c14_age, 'n/a'),
          c14_material = ifelse(is.na(c14_age), "NA", "plant fragment"),
         core_id = str_replace_all(string = core_id, pattern = '-', replacement = '_'),
         site_id = str_replace_all(string = site_id, pattern = '-', replacement = '_')) %>%
  filter(!is.na(c14_age)) %>% 
  separate(col = "Collection Interval (cm)", into = c("depth_min", "depth_max"), sep = "-") 
  

## Species #### 
species_set_up <- species_raw[-c(23:25), ] %>% # eliminating metadata rows
  separate(col = "Site Name; \nCore #", into = "site_id", sep = ";", remove = FALSE) %>% 
  rename(species_code = 'Species',
         core_id = "Site Name; \nCore #",
         easting = Easting,
         northing = Northing,
         year = "Sampling\nYear (CE)") %>% 
  mutate(study_id = "Miller_et_al_2022",
         code_type = 'Genus',
         species_code = paste(species_code, "", sep = ' sp.'),
         habitat = 'marsh',
         core_id = gsub(pattern = '; ', replacement = '_', core_id),
         core_id = str_replace_all(string = core_id, pattern = '-', replacement = '_'),
         core_id = ifelse(core_id == 'NB_3', "NB_3b", core_id), # core_id NB_3 in this dataset is named NB_3b in other datasets; correcting the name here
         site_id = str_replace_all(string = site_id, pattern = '-', replacement = '_'),
         zone = ifelse(site_id == "SRE_U", 17,
                       ifelse(site_id == "SRE_D", 17, 18))) %>% 
  UTM_to_DD()

## Depthseries
depthseries_set_up <- depthseries_raw %>% 
  separate(col = "Interval", into = c("depth_min", "depth_max"), sep = "-") %>% 
  rename(core_id = "Core Name",
         dry_bulk_density = `Dry Bulk Density (g cm-3)`) %>% 
  mutate(study_id = "Miller_et_al_2022", 
         fraction_organic_matter = `LOI (%)`/100,
         fraction_carbon = `Carbon (%)`/100,
         method_id = "single set of methods") %>% 
  separate(col = core_id, into = c("site_id_1", "site_id_2", "site_id_3"), sep = "-", remove = FALSE) %>% 
# there will be a warning message of missing pieces: this is OK as not all core_id data have 2 "-"; corrected below
  mutate(site_id = ifelse(is.na(site_id_3), site_id_1, paste(site_id_1, site_id_2, sep = "_")),
         core_id = str_replace_all(string = core_id, pattern = '-', replacement = '_')) 


## Step 2: Merging and trimming to create final data frames ####

# Adding lat., long., & year from 'species' to 'cores'
cores <- full_join(cores_set_up, species_set_up) %>% 
  select(c(study_id, site_id, core_id, year, latitude, longitude, position_accuracy, position_method, elevation_cores, 
           elevation_datum, elevation_method, elevation_accuracy, salinity_class, salinity_method, vegetation_class, vegetation_method, 
           habitat, inundation_class, inundation_method, core_length_flag)) %>%  
  rename(elevation = elevation_cores)

# Adding c14 age from 'cores' to 'depthseries'  
depthseries <- full_join(depthseries_set_up, c14_table) %>% 
  select(c(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, 
           c14_age, c14_age_se, c14_material))
# don't include (modeled) fraction_carbon  
    
# Trimming species data
species <- select(species_set_up, c(study_id, site_id, core_id, species_code, code_type, habitat))
     
# Trimming methods data
methods <- methods_raw %>% 
  select(c(study_id, method_id, coring_method, sediment_sieved_flag, sediment_sieve_size, compaction_flag, dry_bulk_density_temperature, 
           dry_bulk_density_time, dry_bulk_density_flag, loss_on_ignition_temperature, loss_on_ignition_time, loss_on_ignition_flag, 
           carbon_measured_or_modeled, carbonates_removed, carbonate_removal_method, fraction_carbon_method, fraction_carbon_type, 
           c14_counting_method, dating_notes, age_depth_model_reference))
    

## Step 3: QAQC ####

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
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)


## Step 4: Writing Curated Data ####

write_csv(cores, "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_cores.csv") 
write_csv(depthseries, "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_depthseries.csv")
write_csv(species, "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_species.csv")
write_csv(methods, "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_methods.csv")


## Step 5: Bibliography ####

citation_article <- data.frame(study_id = "Miller_et_al_2022",
                              bibliography_id = "Miller_et_al_2022_article",
                              title = "Carbon accumulation rates are highest at young and expanding salt marsh edges",
                              author = "Carson B. Miller, Antonio B. Rodriguez, Molly C. Bost, Brent A. McKee and Nathan D. McTigue ",
                              doi = "10.1038/s43247-022-00501-x",
                              url = "https://doi.org/10.1038/s43247-022-00501-x", 
                              bibtype = "Article",
                              journal = "Communications Earth & Environment",
                              publication_type = 'associated source',
                              year = "2022",
                              month = "aug",
                              day = '2')

citation_data <- data.frame(study_id = "Miller_et_al_2022",
         bibliography_id = "Miller_et_al_2022_data",
         title = "Salt marsh radiocarbon and loss on Ignition data",
         author = "Miller, Carson; Rodriguez, Antonio; Bost, Molly",
         publication_type = "primary dataset",
         doi = "10.6084/m9.figshare.20137649.v2",
         url = 'https://doi.org/10.6084/m9.figshare.20137649.v2',
         bibtype = "Misc",
         year = "2022",
         month = "jun",
         day = '23')

study_citations <- bind_rows(citation_article, citation_data)

bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Miller_et_al_2022/derivative/Miller_et_al_2022_study_citations.csv")
