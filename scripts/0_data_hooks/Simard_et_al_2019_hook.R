## CCN Data Library ########

## Soil core data curation script for Simard et al 20?? biomass data
## contact: wolfejax@si.edu

## Links to dataset and associated publication: 

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
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# load in data 
raw_treedata <- read_csv("data/primary_studies/Simard_et_al_2019_biomass/original/North_South_America_tree_measurements.csv",
                     na = "-9999")

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Simard_et_al_2019"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

treedata <- raw_treedata %>% 
  rename(habitat = biome,
         plot_id = plot_name,
         latitude = lat1, longitude = lon1) %>% 
  mutate(study_id = id,
         site_id = str_c(region, subregion, sep = "_"))

## ... Plot-level ####
plot <- treedata %>% 
  select(study_id, site_id, plot_id, habitat, date, 
           contains("lat"), contains("lon")) %>% 
  distinct() %>% 

  mutate(longitude = ifelse(longitude > 0, longitude * -1, longitude),
         year = year(as.Date(date)),
         month = month(as.Date(date)),
         day = day(as.Date(date))) %>% 
  select(-date) %>% 
  select(study_id, site_id, plot_id, year, month, day, everything())

leaflet(plot) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, label = ~site_id)


## ... Biomass ####
# is the ID unique? dont think so

biomass <- treedata %>% 
  # select_if(~!all(is.na(.))) %>% 
  rename(tree_id = ID,
         diameter_dbh = dbh) %>%
  mutate(biomass_flag = case_when(live == 1 ~ "live",
                                  live == 0 ~ "dead")) %>%
  select(-c(habitat, contains("lat"), contains("lon"), region, subregion, date,
            collected_by, digitized_by)) %>% arrange(tree_id) %>% add_count(tree_id)


## 2. QAQC ####

ggplot(biomass, aes(height, col = site_id)) +
  geom_density() + geom_rug() + facet_wrap(~site_id)

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
write_csv(methods, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_methods.csv")
#write_csv(sites, "data/primary_studies/Weston_et_al_2020/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_cores.csv")
write_csv(depthseries, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_species.csv")
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
                             bibliography_id = "Weston_et_al_2022_data",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Recent Acceleration of Coastal Wetland Accretion Along the U.S. East Coast",
                             author = "Nathaniel B Weston and Elise Rodriguez and Brian Donnelly and Elena Solohin and Kristen Jezycki and Sandra Demberger 
                             and Lori Sutter and James T. Morris and Scott C. Neubauer and Christopher B Craft",
                             doi = "10.25573/serc.13043054.v1",
                             url = "https://smithsonian.figshare.com/articles/dataset/Dataset_Recent_Acceleration_of_Coastal_Wetland_Accretion_Along_the_U_S_East_Coast/13043054",
                             year = "2022")

weston_bib <- study_citation %>% select(-study_id, -publication_type) %>%   
  column_to_rownames("bibliography_id")

write_csv(study_citation, "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020_study_citations.csv")
WriteBib(as.BibEntry(weston_bib), "data/primary_studies/Weston_et_al_2020/derivative/Weston_et_al_2020.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
