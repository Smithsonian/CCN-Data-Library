## CCN Data Library ########

## Soil core data curation script for <insert dataset name>
## contact: Your Name, your email

## Notes about the dataset 
## Link to the data release and associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in  original data by inserting the path to the dataset
    # if you opened this Rstudio session from the CCRCN-Data-Library.RProj
    # the root working directory will start in the folder where the RProj resides
    # Tip: use tab to autocomplete the file path
# Note: data is usually provided in xlsx, but if .csv use read_csv()

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

original_data <- read_xlsx("")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Author_et_al_year"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table

## ... Sites ####

# if necessary, curate site-level data

## ... Cores ####

# curate core-level data table

## ... Depthseries ####

# curate core depthseries data table

## ... Species ####

# if provided at the site or core-level, curate taxa table
# Note: this information may need to be isolated from another table

## ... Impacts ####

# if provided, curation table of anthropogenic impacts

## 2. QAQC ####

## Mapping
leaflet(core_data) %>%
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
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_methods.csv")
write_csv(sites, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_cores.csv")
write_csv(depthseries, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_depthseries.csv")
write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####

# There are three ways to approach this:
    # 1) download the article citation directly to the study's folder
    # 2) create the study citation in the curation script and output it to the data release folder
    # 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

# example study citation creation:
# study_citations <- data.frame(study_id = "Spera_et_al_2020",
#                              bibliography_id = "Spera_et_al_2020_data",
#                              publication_type = "primary dataset",
#                              bibtype = "Article",
#                              title = "Spatial and temporal changes to a hydrologically-reconnected coastal wetland: Implications for restoration",
#                              author = "Alina C. Spera and John R. White and Ron Corstanje",
#                              doi = "10.1016/j.ecss.2020.106728",
#                              url = "https://doi.org/10.1016/j.ecss.2020.106728", 
#                              journal = "Estuarine, Coastal and Shelf Science",
#                              year = "2020") %>% 
# 
# study_bib <- study_citations %>% 
#     select(-study_id, -publication_type) %>% 
#     distinct() %>% 
#     column_to_rownames("bibliography_id")
# 
# write_csv(study_citations, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_study_citations.csv")
# WriteBib(as.BibEntry(study_bib), "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
