## CCN Data Library ########

## Soil core data curation script for "Soil Carbon within the Mangrove Landscape in Rufiji River Delta, Tanzania" (2022)
# Zhaohua Dai, Carl C. Trettin, Mwita M. Mangora & Wenwu Tang 
## contact: Rose Cheney, cheneyr@si.edu 


## Dataset: https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00221
## Associated paper: https://link.springer.com/article/10.1007/s13157-022-01608-9#Abs1
# data through CIFOR database, published through dataverse


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
sitedata <- read_csv("data/primary_studies/Dai_et_al_2022/original/RufijiRiverDelta_Soil_2019-12-30/SiteData.csv")
plot <- read_csv("data/primary_studies/Dai_et_al_2022/original/RufijiRiverDelta_Soil_2019-12-30/Plot.csv")
subplot <- read_csv("data/primary_studies/Dai_et_al_2022/original/RufijiRiverDelta_Soil_2019-12-30/Subplot.csv")
soil <- read_csv("data/primary_studies/Dai_et_al_2022/original/RufijiRiverDelta_Soil_2019-12-30/Soil.csv") 
soilsubp_CN <- read_csv("data/primary_studies/Dai_et_al_2022/original/RufijiRiverDelta_Soil_2019-12-30/SoilSubp_CN.txt")



## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Dai_et_al_2022"


## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "gouge auger",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_temperature = 105,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_flag = "not specified", 
                      carbonates_removed = FALSE,
                      carbonate_removal_method = "none specified",
                      carbon_measured_or_modeled = "measured",
                      fraction_carbon_type = "organic carbon",
                      fraction_carbon_method = "EA") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####


## ... Cores ####

cores <- subplot %>% select(ID, LATITUDE, LONGITUDE) %>% 
                     mutate(study_id = id,
                            site_id = "Rufiji_River_Delta",
                            habitat = "mangrove",
                            vegetation_class = "forested",
                            vegetation_method = "field observation",
                            year = 2015, #sampling year, exact date unknown
                            core_id = paste(site_id, ID, sep = "_"),
                            salinity_class = "estuarine",
                            salinity_method = "field observation",
                            position_method = "other low resolution",
                            position_notes = "position corresponds to approximate center of subplot") %>% #lowest resolution coords in published dataset 
                     rename(latitude = LATITUDE,
                            longitude = LONGITUDE) %>% 
                     select(-ID) %>% distinct()
                     

cores <- reorderColumns("cores", cores)



## ... Depthseries #### 

#core_id = site + subplotid 
depthseries <- soil %>% select(-AREA, -VOL, -DWT) %>% 
                        mutate(study_id = id,
                               site_id = "Rufiji_River_Delta",
                               method_id = "single set of methods",
                               core_id = paste(site_id, SUBPID, sep = "_"),
                               fraction_carbon = C/100) %>% 
                        rename(depth_min = MIND,
                               depth_max = MAXD,
                               dry_bulk_density = BD) %>% 
                        select(-ID, -SUBPID, -INTD, -C, -N, -C_CONT, -N_CONT, -CNR, -SAMP)


#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)



## ... Species ####


## ... Impacts ####
# all sites in study coded as "intact"
impacts <- subplot %>% select(ID, ECOID) %>% 
                       mutate(study_id = id,
                              site_id = "Rufiji_River_Delta",
                              core_id = paste(site_id, ID, sep = "_"),
                              impact_class = case_when(ECOID == 1 ~ "natural")) %>% 
                       select(-ID, -ECOID)

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
write_csv(methods, "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022_methods.csv")
  #write_csv(sites, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022_cores.csv")
write_csv(depthseries, "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022_depthseries.csv")
  #write_csv(species, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_species.csv")
write_csv(impacts, "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Trettin_et_al_2020_dataset",
                             title = "SWAMP Dataset-Mangrove soil carbon-Rufiji River Delta-2016",
                             author = "Trettin, C.; Dai, Z.; Mangora, M.; Lagomasino, D.; Lee, S.K.; Tang, W.; Fatoyinbo, T.",
                             bibtype = "Misc", 
                             publication_type = "primary dataset",
                             doi = "https://doi.org/10.17528/CIFOR/DATA.00221",
                             url = "https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00221",
                             year = "2020") %>% 
                  column_to_rownames("bibliography_id")

study_citation_article <- data.frame(bibliography_id = "Dai_et_al_2022",
                                     title = "Soil Carbon within the Mangrove Landscape in Rufiji River Delta, Tanzania",
                                     author = "Zhaohua Dai, Carl C. Trettin, Mwita M. Mangora & Wenwu Tang",
                                     bibtype = "Article",
                                     doi = " https://doi.org/10.1007/s13157-022-01608-9",
                                     url = "https://link.springer.com/article/10.1007/s13157-022-01608-9",
                                     journal = "Wetlands",
                                     publication_type = "associated source",
                                     year = "2022") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("Trettin_et_al_2020_dataset", "Dai_et_al_2022"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022.bib")
write_csv(study_citations, "data/primary_studies/Dai_et_al_2022/derivative/Dai_et_al_2022_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
