## CCN Data Library ########

## Soil core data curation script for Snedden 2021 USGS dataset, Soil properties and soil radioisotope activity across Breton Sound basin wetlands (2008-2013)
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://www.sciencebase.gov/catalog/item/60078ab4d34e162231fb1ce7

# load necessary libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCCN-Community-Resources/soil_carbon_guidance.html


#load in data 
data_raw <- read_csv("data/primary_studies/Snedden_2021/original/Breton_Sound_Soil_Properties_Radioisotope_Activity.csv")
coords <- read_csv("data/primary_studies/Snedden_2021/original/Breton_Sound_Core_Coordinates.csv")
cores_metadata <- read_xlsx("data/primary_studies/Snedden_2021/original/breton_sound_cores_legend.xlsx")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Snedden_2021"


## ... Methods ####
methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "push core", 
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_flag = "not specified",
                      carbon_measured_or_modeled = "measured",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 24,
                      carbonates_removed = "FALSE",
                      carbonate_removal_method = "not specified",
                      fraction_carbon_method = "not specified",
                      fraction_carbon_type = "total carbon",
                      cs137_counting_method = "gamma")


#reorder columns 
methods <- reorderColumns("methods", methods)



## ... Cores ####
cores <- coords %>%  rename("site_id" = "Site ID") %>% 
                       mutate(study_id = id,
                              core_id = paste("Breton", site_id, sep = "_"),
                              site_id = "Breton_Sound_Basin",
                              vegetation_class = "emergent",
                              vegetation_method = "field observation",
                              salinity_class = "estuarine",
                              salinity_method = "field observation",
                              habitat = "marsh",
                              core_length_flag = "not specified",
                              position_method = "other high resolution",
                              position_notes = "specified resolution of 0.0001 degrees") %>% 
                      mutate(dates = ymd(coords$DateCollected),
                             year = year(dates),
                             month = month(dates),
                             day = day(dates)) %>% 
                      rename(latitude = Latitude,
                             longitude = Longitude) %>% 
                      select(-dates, -'DateCollected')


#reorder columns 
cores <- reorderColumns("cores", cores)
  

  
## ... Depthseries #### 
#### Depth interval codes --> "2-centimeter increments starting with 0-2 cm and increasing with depth"

depthseries <- data_raw %>% rename("site_id" = `Site ID`) %>% 
                            mutate(study_id = id,
                                   method_id = "single set of methods",
                                   core_id = paste("Breton", site_id, sep = "_"),
                                   site_id = "Breton_Sound_Basin") %>% 
                                   mutate(cs137_unit = "disintegrationsPerminutePerGram",
                                          fraction_organic_matter = `Loss on Ignition`/100,
                                          fraction_carbon = `Percent TC`/100) %>% 
                                  rename(dry_bulk_density = `Bulk Density`,
                                         cs137_activity = `137Cs`,
                                         cs137_activity_se = `137Cs error`) %>% 
                                   select(-`Percent TC`, -`Loss on Ignition`)

#assign depth intevals based on 'depth interval code'
depthseries <- depthseries %>% rename(depth_code = "Depth Interval Code") %>% 
                               mutate(depth_min = case_when(depth_code == 1 ~ 0, depth_code == 2 ~ 2,
                                                            depth_code == 3 ~ 4, depth_code == 4 ~ 6,
                                                            depth_code == 5 ~ 8, depth_code == 6 ~ 10,
                                                            depth_code == 7 ~ 12, depth_code == 8 ~ 14,
                                                            depth_code == 9 ~ 16, depth_code == 10 ~ 18,
                                                            depth_code == 11 ~ 20, depth_code == 12 ~ 22,
                                                            depth_code == 13 ~ 24, depth_code == 14 ~ 26,
                                                            depth_code == 15 ~ 28, depth_code == 16 ~ 30,
                                                            depth_code == 17 ~ 32, depth_code == 18 ~ 34,
                                                            depth_code == 19 ~ 36, depth_code == 20 ~ 38,
                                                            depth_code == 21 ~ 40, depth_code == 22 ~ 42,
                                                            depth_code == 23 ~ 44, depth_code == 24 ~ 46,
                                                            depth_code == 25 ~ 48, depth_code == 26 ~ 50,
                                                            depth_code == 27 ~ 52, depth_code == 27 ~ 54,
                                                            depth_code == 28 ~ 56, depth_code == 29 ~ 58),
                                      depth_max = depth_min + 2) %>% 
                                   select(-depth_code) %>% 
                                  mutate(cs137_activity = ifelse(cs137_activity == ".", NA, cs137_activity),
                                         cs137_activity_se = ifelse(cs137_activity_se == ".", NA, cs137_activity_se))
      
#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)

## ... Sites ####

## ... Species ####

## ... Impacts ####


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
testIDs(cores, depthseries, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
  #testNumericCols(depthseries) function not working 
test_numeric_vars(depthseries) 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Snedden_2021/derivative/Snedden_2021_methods.csv")
  #write_csv(sites, "data/primary_studies/Weston_et_al_2020/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Snedden_2021/derivative/Snedden_2021_cores.csv")
write_csv(depthseries, "data/primary_studies/Snedden_2021/derivative/Snedden_2021_depthseries.csv")
  #write_csv(species, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_species.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Snedden 2021",
                             title = "Soil properties and soil radioisotope activity across Breton Sound basin wetlands (2008-2013)",
                             author = "Gregg A. Snedden",
                             bibtype = "Misc", 
                             doi = "https://doi.org/10.5066/P9XWAXOT",
                             url = "https://www.sciencebase.gov/catalog/item/60078ab4d34e162231fb1ce7",
                             year = "2021") %>% 
                  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Snedden_2021/derivative/Snedden_2021.bib")
write_csv(study_citation, "data/primary_studies/Snedden_2021/derivative/Snedden_2021_study_citation.csv")
# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
