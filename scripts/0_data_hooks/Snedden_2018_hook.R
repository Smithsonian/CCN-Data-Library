## CCN Data Library ########

## Soil core data curation script for Snedden 2018 USGS dataset, Soil properties, soil radioisotope activity, and end-of-season belowground biomass across Barataria basin wetlands (2016)
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://www.sciencebase.gov/catalog/item/5a4eae82e4b0d05ee8c6649
## Environmental and positional data from CRMS database : https://lacoast.gov/crms2/home.aspx

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
data_raw <- read_csv("data/primary_studies/Snedden_2018/original/barataria_soils_radioisotope_activity_tc_tn.csv")
coords <- read_xlsx("data/primary_studies/Snedden_2018/original/site_coords.xlsx")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Snedden_2018"


## ... Methods ####
methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "push core", 
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_flag = "not specified",
                      loss_on_ignition_flag = "not specified",
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = "FALSE",
                      carbonate_removal_method = "none specified",
                      fraction_carbon_method = "not specified",
                      fraction_carbon_type = "total carbon",
                      cs137_counting_method = "gamma",
                      pb210_counting_method = "gamma")


#reorder columns 
methods <- reorderColumns("methods", methods)



## ... Cores ####
cores <- data_raw %>% dplyr::select(`Site ID`,`Date Collected`, `Depth Interval Code`) %>% 
                       rename("site_id" = "Site ID") %>% 
                       mutate(study_id = id,
                              core_id = paste(site_id, 1, sep = "_"), #assign core id
                              vegetation_class = "emergent",
                              vegetation_method = "field observation",
                              habitat = "marsh",
                              core_length_flag = "not specified",
                              position_method = "other high resolution",
                              position_notes = "coordinates were likely high quality but may refer to a gerneal
                              area rather than individual core location") %>% 
                      mutate(dates = ymd(data_raw$`Date Collected`),
                             year = year(dates),
                             month = month(dates),
                             day = day(dates)) %>% 
                      select(-dates, -'Date Collected', - 'Depth Interval Code')

#add site coords --> ###accessed using CRMS database https://lacoast.gov/crms2/home.aspx
cores <- merge(cores, coords) %>% distinct() %>% 
        mutate(salinity_class = "estuarine",
               salinity_method = "measurement",
               inundation_class = "low",
               inundation_method = "field observation") %>% 
        select(-elevation,-elevation_notes)

#reorder columns 
cores <- reorderColumns("cores", cores)
  

  
## ... Depthseries #### 
#### Depth interval codes --> "2-centimeter increments starting with 0-2 cm and increasing with depth"

depthseries <- data_raw %>% mutate(study_id = id,
                                   method_id = "single set of methods") %>% 
                                   mutate(cs137_unit = "disintegrationsPerMinutePerGram",
                                          pb210_unit = "disintegrationsPerMinutePerGram",
                                          fraction_organic_matter = `Percent Organic Matter`/100,
                                          fraction_carbon = `Percent TC`/100) %>% 
                                  rename(site_id = `Site ID`,
                                         dry_bulk_density = `Bulk Densisty`,
                                         cs137_activity = `137Cs`,
                                         cs137_activity_se = `137Cs error`,
                                         total_pb210_activity = `210Pb total`,
                                         excess_pb210_activity = `210Pb unsupported`,
                                         excess_pb210_activity_se = `210Pb unsupported error`) %>%
                                   mutate(core_id = paste(site_id, 1, sep = "_")) %>% 
                                   select(-`Percent TN`, -`Date Collected`, -`Percent Organic Matter`, 
                                          -`Percent TC`, -`210Pb supported`)

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
                                                            depth_code == 27 ~ 52) ,
                                      depth_max = depth_min + 2) %>% 
                                   select(-depth_code)
      
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
write_csv(methods, "data/primary_studies/Snedden_2018/derivative/Snedden_2018_methods.csv")
  #write_csv(sites, "data/primary_studies/Weston_et_al_2020/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Snedden_2018/derivative/Snedden_2018_cores.csv")
write_csv(depthseries, "data/primary_studies/Snedden_2018/derivative/Snedden_2018_depthseries.csv")
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

study_citation <- data.frame(study_id = id,
                             bibliography_id = "Snedden_2018_article",
                             publication_type = "primary dataset",
                             title = "Dataset: Soil properties, soil radioisotope activity, and end-of-season belowground biomass across Barataria basin wetlands (2016)",
                             author = "Gregg A. Snedden",
                             bibtype = "Misc", 
                             doi = "https://doi.org/10.5066/F7BK1BJ8",
                             url = "https://www.sciencebase.gov/catalog/item/5a4eae82e4b0d05ee8c6649c",
                             year = "2018") 

WriteBib(as.BibEntry(study_citation %>% column_to_rownames("bibliography_id")), "data/primary_studies/Snedden_2018/derivative/Snedden_2018.bib")
write_csv(study_citation, "data/primary_studies/Snedden_2018/derivative/Snedden_et_al_2018_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
