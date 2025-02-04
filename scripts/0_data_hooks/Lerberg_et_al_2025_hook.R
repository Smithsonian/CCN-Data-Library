## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Lerberg et al 2025: Carbon storage and accretion in Sweet Hall Marsh, Virginia

## Dataset: https://doi.org/10.25573/serc.27637311.v1

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html


#load in data 
methods_raw <- read_csv("data/primary_studies/Lerberg_et_al_2025/original/Lerberg_et_al_2025_methods.csv")
cores_raw <- read_csv("data/primary_studies/Lerberg_et_al_2025/original/Lerberg_et_al_2025_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Lerberg_et_al_2025/original/Lerberg_et_al_2025_depthseries.csv")

  
## 1. Curation ####

id <- "Lerberg_et_al_2025"


## ... Methods ####

methods <- methods_raw %>% 
  select(-c(scale_error, detector_time, attenuated_counts_time)) 

methods <- reorderColumns("methods", methods)



## ... Cores ####

cores <- cores_raw %>% 
  mutate(vegetation_class = case_when(grepl("Forest", core_id) ~ "forested",
                                      TRUE ~ "emergent"), 
         vegetation_method = "field observation",
         salinity_class = "estuarine", #check? 
         salinity_method = "field observation") %>% 
  select(-c(pb210_accretion_rate, cs137_accretion_rate))

cores <- reorderColumns("cores", cores)

## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(fraction_organic_matter = percent_organic_matter/100,
         excess_pb210_activity = excess_pb210_activity_corrected, #using author-corrected values for dates
         cs137_activity = cs137_activity_corrected) %>% 
  select(-c(percent_organic_matter, pan_weight, pan_wet_sample, pan_dry_sample, pan_combustion_sample, porosity, 
            percent_gravel, percent_very_coarse_sand, percent_coarse_sand, percent_med_sand, percent_fine_sand, 
            percent_very_fine_sand, percent_total_sand, percent_silt, percent_clay, days_of_decay,
            petri_dish_size, sample_mass, sample_mass_re, total_pb210_net_peak_area, total_pb210_counts_re, 
            total_pb210_counts_sec, total_pb210_counts_sec_ae, total_pb210_counts_sec_re, total_pb210_attenuated_counts,
            total_pb210_attenuated_counts_re, total_pb210_attenuated_counts_sec, total_pb210_attenuated_counts_sec_ae,
            bi214_net_peak_area, bi214_counts_re, excess_pb210_activity_corrected, cs137_activity_corrected,
            cs137_net_peak_area, excess_pb210_activity_stdev, excess_pb210_activity_ae, pb214_295_counts_re, 
            pb214_295_net_peak_area, pb214_295_counts_re, pb214_352_net_peak_area, pb214_352_counts_re,
            pb210_support_stdev, pb210_support_ae, cs137_counts_re, pb210_support_value, pb210_support_stdev, 
            pb210_support_se, pb210_support_ae)) #remove uncontrolled variables, convert errors to se 
  
#using values from pb214 295 keV or pb214 352 keV? 

depthseries <- reorderColumns("depthseries", depthseries)


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries")

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
write_csv(methods, "data/primary_studies/Lerberg_et_al_2025/derivative/Lerberg_et_al_2025_methods.csv")
write_csv(cores, "data/primary_studies/Lerberg_et_al_2025/derivative/Lerberg_et_al_2025_cores.csv")
write_csv(depthseries, "data/primary_studies/Lerberg_et_al_2025/derivative/Lerberg_et_al_2025_depthseries.csv")

## 4. Bibliography ####

# There are three ways to approach this:
    # 1) download the article citation directly to the study's folder
    # 2) create the study citation in the curation script and output it to the data release folder
    # 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

# example study citation creation:
# study_citation <- data.frame(bibliography_id = "Spera_et_al_2020",
#                              title = "Spatial and temporal changes to a hydrologically-reconnected coastal wetland: Implications for restoration",
#                              author = "Alina C. Spera and John R. White and Ron Corstanje",
#                              bibtype = "Article",
#                              doi = "10.1016/j.ecss.2020.106728",
#                              url = "https://doi.org/10.1016/j.ecss.2020.106728", 
#                              journal = "Estuarine, Coastal and Shelf Science",
#                              year = "2020") %>% 
#     column_to_rownames("bibliography_id")
# 
# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_associated_publications.bib")

study_citation <- data.frame(study_id = id, 
                             bibliography_id = "Lerberg_et_al_2025",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Carbon storage and accretion in Sweet Hall Marsh, Virginia",
                             author = "Scott B. Lerberg, Alex Demeo, Hank Brooks, Erin Reilly, Jennifer Connell, Katherine Kivimaki, Allyson Boggess, James Holmquist",
                             doi = "10.25573/serc.27637311",
                             url = "https://doi.org/10.25573/serc.27637311.v1",
                             year = "2025")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Lerberg_et_al_2025/derivative/Lerberg_et_al_2025_study_citations.bib")
write_csv(study_citation, "data/primary_studies/Lerberg_et_al_2025/derivative/Lerberg_et_al_2025_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
