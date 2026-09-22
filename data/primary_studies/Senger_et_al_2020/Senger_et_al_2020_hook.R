## CCN Data Library ########

## Soil core data curation script for Senger, Florian; Gillis, Lucy Gwen; Engel, Sabine (2020): 
## Sediment characteristics of the mangrove forest of Bonaire, Dutch Caribbean. PANGAEA, https://doi.org/10.1594/PANGAEA.910431,
## contact: Rose Cheney, cheneyr@si.edu 


## Dataset: https://doi.org/10.1594/PANGAEA.910431,
## Associated paper: https://www.sciencedirect.com/science/article/abs/pii/S0048969720353468?via%3Dihub
# data published separately through PANGEA 
# includes AGB and BGB


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
#import raw data without additional metadata rows 
data <- read_xlsx("data/primary_studies/Senger_et_al_2020/original/Sediment-Senger-etal-2020.xlsx", skip = 55) 


## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Senger_et_al_2020"


## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "gouge auger",
                      roots_flag = "roots and rhizomes separated",
                      compaction_flag = "not specified",
                      sediment_sieved_flag = "sediment not sieved",
                      dry_bulk_density_temperature = 40,
                      dry_bulk_density_time = 72,
                      dry_bulk_density_flag = "time approximate",
                      loss_on_ignition_flag = "not specified", 
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      carbon_measured_or_modeled = "measured",
                      fraction_carbon_type = "organic carbon",
                      fraction_carbon_method = "EA") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####


## ... Cores ####

cores <- data %>% select(Latitude, Longitude, Site, `Date/Time`,`Sample ID`) %>% 
                  rename(latitude = Latitude,
                         longitude = Longitude,
                         core_id = `Sample ID`,
                         site_id = Site) %>% 
                  na.omit(core_id) %>% #remove replicate depthseries samples
                  mutate(study_id = id,
                         position_method = "handheld",
                         salinity_class = "saline", 
                         salinity_method = "field observation",
                         habitat = "mangrove", 
                         vegetation_class = "forested",
                         vegetation_method = "field observation", 
                         core_length_flag = "not specified",
                         dates = ymd(`Date/Time`),
                         year = year(dates),
                         month = month(dates),
                         day = day(dates)) %>% 
                  select(-dates, -`Date/Time`)

                        
cores <- reorderColumns("cores", cores)

## ... Depthseries #### 

depthseries <- data %>% select(Site, `Sample ID`,`Depth top [m]`, `Depth bot [m]`, 
                              `TOC [%] (Average of replicates)`) %>% 
                        rename(core_id = `Sample ID`,
                               site_id = Site,
                               depth_min = `Depth top [m]`,
                               depth_max = `Depth bot [m]`) %>%
                        mutate(study_id = id,
                               method_id = "single set of methods",
                               fraction_carbon = `TOC [%] (Average of replicates)`/100,
                               depth_max = recode(depth_max,`0.05` = 5)) %>% 
                        fill(site_id, core_id) %>% 
                        select(-`TOC [%] (Average of replicates)`) 


#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####
#from associated paper, sites are dominated by 3 species

species <- cores %>% select(study_id, site_id, habitat) %>% distinct() %>% 
                     mutate(code_type = "Genus species",
                            species_code = "Rhizophora mangle & Avicennia germinans & Laguncularia racemosa") %>% 
                     separate(species_code, c("species_code", "species_code2", "species_code3"), sep = " & ") %>% 
                     pivot_longer(!c(study_id, site_id, habitat, code_type), 
                                  names_to = NULL, values_to = "species_code")

species <- reorderColumns("species", species)

## ... Impacts ####

impacts <- data %>% select(Site,Status, `Sample ID`) %>% 
            rename(site_id = Site,
                   impact_class = Status,
                   core_id = `Sample ID`) %>% 
            mutate(study_id = id,
                   impact_class = recode(impact_class, Intact = "natural"),
                   impact_class = recode(impact_class, Degraded = "degraded")) %>% 
            na.omit(site_id)

impacts <- reorderColumns("impacts", impacts)



## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species", "impacts") 

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
write_csv(methods, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_methods.csv")
  #write_csv(sites, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_sites.csv")
write_csv(cores, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_cores.csv")
write_csv(depthseries, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_species.csv")
write_csv(impacts, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Senger_et_al_2020_dataset",
                             title = "Sediment characteristics of the mangrove forest of Bonaire, Dutch Caribbean",
                             author = "Senger, Florian; Gillis, Lucy Gwen; Engel, Sabine",
                             bibtype = "Misc", 
                             publication_type = "primary dataset",
                             doi = "https://doi.org/10.1594/PANGAEA.910431",
                             url = "https://doi.pangaea.de/10.1594/PANGAEA.910431",
                             year = "2020") %>% 
                  column_to_rownames("bibliography_id")

study_citation_article <- data.frame(bibliography_id = "Senger_et_al_2021_paper",
                                     title = "Impacts of wetland dieback on carbon dynamics: A comparison between intact and degraded mangroves",
                                     author = "D.F. Senger, D.A. Saavedra Hortua, S. Engel, M. Schnurawa, N. Moosdorf, L.G. Gillis",
                                     bibtype = "Article",
                                     doi = "https://doi.org/10.1016/j.scitotenv.2020.141817",
                                     url = "https://www.sciencedirect.com/science/article/pii/S0048969720353468",
                                     journal = "Science of the Total Environment",
                                     publication_type = "associated source",
                                     year = "2021") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("Senger_et_al_2020", "Senger_et_al_2021_paper"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020.bib")
write_csv(study_citations, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
