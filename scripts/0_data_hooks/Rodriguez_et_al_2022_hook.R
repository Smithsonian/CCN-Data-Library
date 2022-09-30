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

cores_raw <- read_xlsx("./data/primary_studies/Rodriguez_et_al_2022/original/Supplementary_Data.xlsx", sheet = 1)
depthseries_raw <- read_xlsx("./data/primary_studies/Rodriguez_et_al_2022/original/Supplementary_Data.xlsx", sheet = 2)
species_raw <- read_csv("./data/primary_studies/Rodriguez_et_al_2022/intermediate/Table_1.csv") # this is the converted .csv file (.../Rodriguez_et_al_2022/intermediate) made from the original .docx (.../Rodriguez_et_al_2022/original)
methods_raw <- read_csv("./data/primary_studies/Rodriguez_et_al_2022/intermediate/Methods.csv")

# Note that each of the following data frame set ups need to be run as a whole because variables are sequentially edited

## Cores
cores_set_up <- cores_raw %>% 
  fill(Site, `Core Number`) %>% 
  separate(col = 'Site', c(NA, 'site_id'), sep = '; ') %>% 
  rename(core_id = `Core Number`,
         elevation_cores = `Elevation (m; NAVD88)`,
         c14_age = 'Radiocarbon Age (yr BP)',
         c14_age_se = 'Age Error') %>% 
  unite(col = "core_id", site_id : core_id, sep = '_', remove = FALSE) %>% 
  distinct(core_id, .keep_all = TRUE) %>% 
  mutate(core_id = str_replace_all(string = cores$core_id, pattern = '-', replacement = '_'),
         site_id = str_replace_all(string = cores$site_id, pattern = '-', replacement = '_'),
         study_id = "Rodriguez_et_al_2022",
         elevation_datum = "NAVD88",
         elevation_method = 'RTK',
         salinity_method = 'field observation',
         vegetation_class = "emergent",
         vegetation_method = "field observation",
         core_length_flag = 'core depth represents deposit depth',
         habitat = "marsh",
         salinity_class = 'estuarine',
         inundation_method = 'field observation',
         inundation_class = ifelse(site_id %in% c("SRE_U", "SRE_D", "NPR_D"), "low", "high"))
  

## Species
species_set_up <- species_raw[-c(1, 24:26), ] %>% # eliminating metadata rows
  rename(core_id = 'Table 1: Site information and salt marsh unit measurements.', species_code = '...2', 
         development_mode = '...3', year = '...4', easting = '...5', northing = '...6', elevation_species = '...7', 
         thickness = '...8', age = '...9', stock = '...10') %>% 
  separate(col = core_id, into = "site_id", sep = ";", remove = FALSE) %>% 
  mutate(study_id = "Rodriguez_et_al_2022",
         code_type = 'genus',
         habitat = 'marsh',
         core_id = gsub('; ', '_', species$core_id),
         core_id = str_replace_all(string = species$core_id, pattern = '-', replacement = '_'),
         core_id = ifelse(core_id == 'NB_3', "NB_3b", core_id), # core_id NB_3 in this dataset is named NB_3b in other datasets; correcting the name here
         site_id = str_replace_all(string = species$site_id, pattern = '-', replacement = '_'),
         zone = ifelse(site_id == "SRE_U", 17,
                       ifelse(site_id == "SRE_D", 17, 18))) %>% 
  UTM_to_DD(core_id = core_id, easting = easting, northing = northing, zone = zone)


## Depthseries
depthseries_set_up <- depthseries_raw %>% 
  separate(col = "Interval", into = c("depth_min", "depth_max"), sep = "-") %>% 
  rename(core_id = "Core Name",
         dry_bulk_density = `Dry Bulk Density (g cm-3)`) %>% 
  mutate(study_id = "Rodriguez_et_al_2022", 
         fraction_organic_matter = `LOI (%)`/100,
         fraction_carbon = `Carbon (%)`/100) %>% 
  select(-c(`Dry mass (g)`,`Ignited mass (g)`,`LOI (%)`,`Carbon (%)`, `g C m-2`, `± g C m-2\r\n(95% CI)`)) %>% 
  separate(col = core_id, into = c("site_id_1", "site_id_2", "site_id_3"), sep = "-", remove = FALSE) %>% 
# there will be a warning message of missing pieces: this is OK as not all core_id data have 2 "-"; corrected below
  mutate(site_id = ifelse(is.na(site_id_3), site_id_1, paste(site_id_1, site_id_2, sep = "_")),
         core_id = str_replace_all(string = depthseries$core_id, pattern = '-', replacement = '_'))


## Step 2: Adding/subtracting required columns from data frame set ups to add to final data frames ####

# Adding lat., long., & year from 'species' to 'cores'
cores <- full_join(cores_set_up, species_set_up) %>% 
  select(-c(`Collection Interval (cm)`, `Fraction Modern`, 'FM Error',c14_age, c14_age_se, `Calibrated Age Range (CE)`, 
            Certainty, `Median Probability`, `Comments:`, species_code, development_mode, elevation_species, thickness, 
            age, stock, code_type, easting, northing, zone)) %>% 
  rename(elevation = elevation_cores)


# Adding c14 age from 'cores' to 'depthseries'  
depthseries <- full_join(depthseries_set_up, cores_set_up) %>% 
  select(-c(site_id_1, site_id_2, site_id_3, elevation_cores, 'Collection Interval (cm)', 'Fraction Modern', 'FM Error', 
            'Calibrated Age Range (CE)', 'Certainty', 'Median Probability', 'Comments:', elevation_datum, elevation_method, 
            salinity_class, salinity_method, core_length_flag, vegetation_class, vegetation_method, habitat, inundation_method,
            inundation_class))


species <- select(species_set_up, -c(development_mode, year, easting, northing, elevation_species, thickness, age, 
                                     stock, zone, latitude, longitude))

methods <- select(methods_raw, -c(method_id, roots_flag, dry_bulk_density_sample_volume, dry_bulk_density_sample_mass, 
                                  loss_on_ignition_sample_mass, loss_on_ignition_sample_volume, carbon_profile_notes, 
                                  cs137_counting_method, pb210_counting_method, excess_pb210_rate, excess_pb210_model, 
                                  ra226_assumption, age_depth_model_notes))

## Step 3: QAQC ####

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
testIDs(cores, depthseries, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## Step 4: Writing Curated Data ####

write_csv(cores, "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_cores.csv") 
write_csv(depthseries, "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_depthseries.csv")
write_csv(species, "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_species.csv")
write_csv(methods, "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_methods.csv")


## Step 5: Bibliography ####

citation_article <- data.frame(study_id = "Rodriguez_et_al_2022",
                              bibliography_id = "Rodriguez_et_al_2022_article",
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

citation_data <- data.frame(study_id = "Rodriguez_et_al_2022",
         bibliography_id = "Rodriguez_et_al_2022_data",
         title = "Salt marsh radiocarbon and loss on Ignition data",
         author = "Rodriguez, Antonio; Miller, Carson; Bost, Molly",
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

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Rodriguez_et_al_2022/derivative/Rodriguez_et_al_2022_study_citations.csv")



