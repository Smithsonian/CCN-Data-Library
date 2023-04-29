## CCN Data Library ########

## Data hook script for Morrissette et al 2023
## contact: Jaxine Wolfe; wolfejax@si.edu 

# load necessary libraries
library(tidyverse)
# library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
# library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html

# load in data 
methods_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_methods.csv")
plots_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_plots.csv")
depthseries_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_depthseries.csv")
biomass_raw <- read_csv("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_biomass.csv")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Morrissette_et_al_2023"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

methods <-  methods_raw

#reorder columns 
methods <- reorderColumns("methods", methods)

## ... Plot and Core Level ####
core_plot <- plots_raw %>% 
  rename(plot_notes = site_description) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         ecotype = tolower(ecotype),
         salinity_class = assignSalinityClass(salinity),
         salinity_method = ifelse(!is.na(salinity_class), "measurement", NA),
         vegetation_class = ifelse(habitat == "mangrove", "forested", "seagrass")) 
  
# divvy into core and plot level tables
cores <- core_plot %>% 
  select(-c(transect_id, plot_id, section_n, max_depth, pH, ORP, tree_count, dominant_species, 
            salinity, protection_status, protection_notes, ecosystem_health,
            inundation_notes, plot_notes, contains("_carbon")))

plots <- core_plot %>%
  select(-c(core_id, section_n, max_depth, pH, ORP, salinity,
            protection_status, protection_notes, ecosystem_health, inundation_notes, 
            # retain biomass and carbon calculations?
            contains("_carbon")))

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(method_id = "single set of methods",
         fraction_organic_matter = soil_organic_matter/100,
         fraction_carbon = soil_organic_carbon/100) %>% 
  select(-c(transect_id, plot_id, contains("carbon_density"), contains("carbon_stock"), contains("soil_"))) %>% 
  select(study_id, site_id, method_id, everything())

# depthseries <- reorderColumns("depthseries", depthseries)

## ... Biomass ####

biomass <- biomass_raw %>% 
  rename(diameter_crown = canopy_width,
         height = tree_height,
         debris_count = debris_number) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         plot_area = pi*(plot_radius^2)) %>% 
  select(-c(transect_id))

# which attributes to drop/retain?
# add? plot shape is circle

## ... Species ####

species <- plots_raw %>% 
  rename(species_code = dominant_species) %>% 
  mutate(plot_id = str_c(site_id, transect_id, plot_id, sep = "_"),
         species_code = strsplit(species_code, split = ", ")) %>% 
  select(study_id, site_id, plot_id, habitat, species_code) %>% 
  unnest(species_code) %>% 
  mutate(code_type = "Genus species")

sort(unique(species$species_code))

## ... Impacts ####

# create impacts table from plot level
impacts <- plots_raw %>% 
  select(contains("_id"), ecosystem_health) %>% 
  distinct()
# unique impacts at the site and transect level

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species")

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
      #testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_methods.csv")
  #write_csv(sites, "data/primary_studies/Morrissette_et_al_2023/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_species.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####
  
# read in data and article citations
release_bib <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.13043054")) %>% 
  mutate(bibliography_id = "Morrissette_et_al_2023_data", publication_type = "primary dataset")
pub_bib <- as.data.frame(ReadBib("data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023.bib")) %>% 
  mutate(bibliography_id = "Morrissette_et_al_2023_article", publication_type = "primary source")

study_citation <- bind_rows(release_bib, pub_bib) %>% 
  mutate(study_id = id) %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, bibtype, everything())
  
Morrissette_bib <- study_citation %>% select(-study_id, -publication_type) %>%   
                  column_to_rownames("bibliography_id")

write_csv(study_citation, "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023_study_citations.csv")
WriteBib(as.BibEntry(Morrissette_bib), "data/primary_studies/Morrissette_et_al_2023/derivative/Morrissette_et_al_2023.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
