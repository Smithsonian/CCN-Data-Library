## CCN Data Library ########

## Soil core data curation script for "Fractionation of Organic Carbon and Stock Measurement in the Sundarbans Mangrove Soils of Bangladesh" (2021)
# Sayada Momotaz Akther, Md Mahfuz Islam, Md Faruque Hossain, Zakia Parveen
## contact: Rose Cheney, cheneyr@si.edu 


## https://www.scirp.org/pdf/ajcc_2021123016360562.pdf
# original values within written paper 


# load necessary libraries
library(tidyverse)
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
cores_raw <- read_xlsx("data/primary_studies/Akther_et_al_2021/original/akther_cores.xlsx")
depthseries_raw <- read_xlsx("data/primary_studies/Akther_et_al_2021/original/akther_depthseries.xlsx")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Akther_et_al_2021"


## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "gouge auger",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment sieved",
                      sediment_sieve_size = 2,
                      compaction_flag = "corer limits compaction", #"relatively undisturbed"
                      dry_bulk_density_flag = "air dried to constant mass",
                      loss_on_ignition_flag = "not specified", 
                      carbonates_removed = FALSE,
                      carbonate_removal_method = "none specified",
                      carbon_measured_or_modeled = "measured",
                      fraction_carbon_type = "organic carbon",
                      fraction_carbon_method = "wet oxidation") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####


## ... Cores ####

cores <- cores_raw %>% mutate(study_id = id,
                              site_id = "Sundarbans",
                              year = 2021, # year of publication
                              position_method = "other moderate resolution", 
                              salinity_class = "estuarine",
                              salinity_method = "field observation",
                              vegetation_class = "forested",
                              vegetation_method = "field observation",
                              habitat = "mangrove",
                              core_length_flag = "not specified")

cores <- reorderColumns("cores", cores)



## ... Depthseries #### 

depthseries <- depthseries_raw %>% mutate(study_id = id,
                                           site_id = "Sundarbans",
                                          method_id = "single set of methods") %>% 
                                  rename(core_id = Soil,
                                         fraction_carbon = `OC (%)`,
                                         dry_bulk_density = `BD (g·cm−3)`) %>% 
                                 separate(`Depth (cm)`, c("depth_min", "depth_max")) %>% 
                                 separate(fraction_carbon, c("fraction_carbon", "se"), sep = " ± ") %>% 
                                 mutate(fraction_carbon = as.numeric(fraction_carbon)/100,
                                        depth_min = as.numeric(depth_min),
                                        depth_max = as.numeric(depth_max)) %>% #percent to fraction
                                 select(-ph, -`Eh (mV)`, -se)

#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)



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
write_csv(methods, "data/primary_studies/Akther_et_al_2021/derivative/Akther_et_al_2021_methods.csv")
  #write_csv(sites, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Akther_et_al_2021/derivative/Akther_et_al_2021_cores.csv")
write_csv(depthseries, "data/primary_studies/Akther_et_al_2021/derivative/Akther_et_al_2021_depthseries.csv")
  #write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
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
                             bibliography_id = "Akther_et_al_2021_article",
                             title = "Fractionation of Organic Carbon and Stock Measurement in the Sundarbans Mangrove Soils of Bangladesh",
                             author = "Sayada Momotaz Akther, Md Mahfuz Islam, Md Faruque Hossain, Zakia Parveen",
                             bibtype = "Article", 
                             publication_type = "primary dataset",
                             doi = "https://doi.org/10.4236/ajcc.2021.104028",
                             url = "https://www.scirp.org/journal/paperinformation.aspx?paperid=114407",
                             journal = "American Journal of Climate Change",
                             year = "2021")
                  

#write bib           
WriteBib(as.BibEntry(study_citation %>% column_to_rownames("bibliography_id")), "data/primary_studies/Akther_et_al_2021/derivative/Akther_et_al_2021.bib")
write_csv(study_citation, "data/primary_studies/Akther_et_al_2021/derivative/Akther_et_al_2021_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
