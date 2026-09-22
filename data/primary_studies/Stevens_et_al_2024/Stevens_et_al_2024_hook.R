## CCN Data Library ####

## Soil core data curation script for Stevens et al 2024
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read files ####
species <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_species.csv")
impacts <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_impacts.csv")
methods <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_methods.csv")
cores <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_cores.csv") %>% 
  # jaxine edit
  mutate(vegetation_class = recode(vegetation_class, "seagrass" = "emergent"))
depthseries <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_depthseries.csv")
sites <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_sites.csv")
study_citations <- read.csv("data/primary_studies/Stevens_et_al_2024/original/Stevens_et_al_2024_study_citations.csv") 

#study id
id <- "Stevens_et_al_2024"

#curate depthseries units 
depthseries <- depthseries %>% 
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA),
         pb210_unit = ifelse(!is.na(excess_pb210_activity), "disintegrationsPerMinutePerGram", NA))

depthseries <- reorderColumns("depthseries", depthseries)


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "sites", "depthseries", "impacts", "species")

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


## 3. Write data vis report ####
writeDataVizReport(id)
# error message -> Quitting from lines 152-192 [210Pb depth profiles] (data_visualization_report.Rmd)
#Error in `if (...) NULL`:
# ! the condition has length > 1

## 4. Write Curated Data ####

## Write files ####
write_csv(cores, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_depthseries.csv")
write_csv(methods, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_methods.csv")
write_csv(impacts, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_impacts.csv")
write_csv(species, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_species.csv")
write_csv(sites, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_sites.csv")
write_csv(study_citations, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_study_citations.csv")

## 5. Bibliography ####
thesis_citation <- dataset_citation <- data.frame(study_id = id,
                                                  bibliography_id = "Stevens_20",
                                                  publication_type = "article",
                                                  bibtype = "Thesis", 
                                                  title = "Examining Coastal Marsh Sedimentation in Northeastern North Carolina",
                                                  author = "Jessica Strand",
                                                  type = "Master's Thesis",
                                                  institution = "Eastern Carolina University",
                                                  year = "20")



dataset_citation <- data.frame(study_id = id,
                               bibliography_id = id,
                               publication_type = "primary dataset",
                               bibtype = "Misc", 
                               title = "Dataset: Examining Coastal Marsh Sedimentation in Northeastern North Carolina",
                               author = "Jessica Strand, D. Reide Corbett",
                               doi = "10.25573/serc.24991359",
                               url = "https://doi.org/10.25573/serc.24991359",
                               year = "2024")

study_citations <- full_join(dataset_citation, thesis_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Strand_et_al_2024/derivative/Strand_et_al_2024.bib")
write_csv(study_citations, "data/primary_studies/Strand_et_al_2024/derivative/Strand_et_al_2024_study_citations.csv")

