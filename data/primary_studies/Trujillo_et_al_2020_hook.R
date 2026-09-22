## CCN Data Library ########

## Soil core data curation script for Trujillo et al 2020
## Sediment characteristics of the mangrove forest of Bonaire, Dutch Caribbean. PANGAEA, https://doi.org/10.1594/PANGAEA.910431,
## contact: Rose Cheney, cheneyr@si.edu 

## Associated paper link - https://doi.org/10.1016/j.ecss.2020.106888


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

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Trujillo_et_al_2020"



## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "russian corer",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_flag = "not specified", 
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = FALSE,
                      fraction_carbon_method = "EA",
                      fraction_carbon_type = "organic carbon") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####



## ... Cores ####

#create from core ids in depthseries and then fill in from there 

cores <- reorderColumns("cores", cores)


## ... Depthseries #### 

##sites seemed to be averaged in paper for depthseries values, each site had 3 cores taken?


## ... Species ####
#included in paper


## ... Impacts ####
#from associated paper?


impacts <- reorderColumns("impacts", impacts)



## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "impacts") 

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
write_csv(methods, "data/primary_studies/")
  #write_csv(sites, "data/primary_studies/AUTHOR_ET_AL_YYYY/derivative/AUTHOR_ET_AL_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/")
write_csv(depthseries, "data/primary_studies/")
 #write_csv(species, "data/primary_studies/AUTHOR_ET_AL_YYYY/derivative/AUTHOR_ET_AL_YYYY_species.csv")
#write_csv(impacts,"data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Trujillo_et_al_2020",
                             title = "",
                             author = "",
                             bibtype = "Article", 
                             publication_type = "",
                             doi = "",
                             url = "",
                             year = "2020") %>% 
                  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Trujillo_et_al_2020/Trujillo_et_al_2020.bib")
write_csv(study_citations, "data/primary_studies/Trujillo_et_al_2020/Trujillo_et_al_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
