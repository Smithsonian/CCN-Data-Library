## CCN Data Library ########

## Soil core data curation script for Lafratta et al 2018 dataset: Importance of habitat selection for Blue Carbon projects: Doubtful additionality in a seagrass case study
## contact: Rose Cheney, cheneyr@si.edu 


## Dataset: https://ro.ecu.edu.au/datasets/38/
## Associated paper:https://doi.org/10.1016/j.ocecoaman.2020.105295


# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(sp)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCCN-Community-Resources/soil_carbon_guidance.html


#load in data 
#import raw data without additional metadata rows 
data <- read_xlsx("data/primary_studies/Lafratta_et_al_2018/original/Dataset_Lafratta_et_al._BiolLett_dataset_for_RO.xlsx", skip = 1) 


## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Lafratta_et_al_2018"


#format original data 
data <- data %>% rename(core_id = `core ID`,
                        site_id = Location,
                        se = ...19) 

## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "none specified",
                      roots_flag = "roots and rhizomes included",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "compaction quantified",
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_flag = "not specified",
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      fraction_carbon_method = "EA",
                      fraction_carbon_type = "organic carbon",
                      pb210_counting_method = "alpha",
                      excess_pb210_rate = "depth",
                      excess_pb210_model = "CFCS",
                      ra226_assumption = "selected samples")


#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####

## ... Cores ####

cores <- data %>% select(site_id, core_id, latitude, longitude,Habitat) %>% 
                  na.omit(Location) %>% 
                  rename(habitat = Habitat) %>% 
                  mutate(study_id = id,
                         year = 2014,
                         vegetation_class = "seagrass",
                         vegetation_method = "field observation",
                         core_length_flag = "not specified",
                         position_method = " ",
                         salinity_class = "saline",
                         salinity_method = "field observation",
                         inundation_class = "low",
                         inundation_method = "field observation",
                         core_notes = "cores collected at 5m depth in seagrass meadows",
                         latitude = as.numeric(char2dms(latitude, chd = "°", chm = "'", chs = "\"")),
                         longitude = as.numeric(char2dms(longitude, chd = "°", chm = "'", chs = "\""))) %>% distinct()


cores <- reorderColumns("cores", cores)


## ... Depthseries #### 
#depth intervals are separated into "low resolution" >21 cm and "high resolution" < 21 cm downcore samples 
depthseries <- data %>% select(site_id, core_id, `Dry bulk density`, `Organic carbon`, `Total -210Pb`, se,
                               δ13C,`cm compressed`, `cm decompressed`) %>% 
                        filter(!is.na(site_id)) %>% 
                        separate(`cm compressed`, c("depth_min", "depth_max"), sep = "-", fill = "right") %>% 
                        mutate(study_id = id,
                               method_id = "single set of methods",
                               dry_bulk_density = as.numeric(`Dry bulk density`),
                               fraction_carbon = as.numeric(`Organic carbon`)/100, #percent to fraction
                               total_pb210_activity = as.numeric(`Total -210Pb`),
                               pb210_unit = "becquerelsPerKilogram",
                               depth_min = as.numeric(depth_min),
                               compaction_fraction = depth_min/`cm decompressed`, #check?
                               depth_interval = case_when(core_id == "1Rs" ~ 1,
                                                          core_id == "3Rc" ~ 1,
                                                          core_id == "6Rc" ~ 1,
                                                          core_id == "3B" ~ 1,
                                                          TRUE ~ 2),
                               depth_max = case_when(depth_interval = 1 & depth_min <= 21 ~ depth_min +0.5,
                                                     depth_interval = 1 & depth_min > 21 ~ depth_min +1,
                                                     depth_interval = 2 & depth_min <= 21 ~ depth_min +1,
                                                     TRUE ~ depth_min + 4),
                               dry_bulk_density = as.numeric(dry_bulk_density),
                               total_pb210_activity = as.numeric(total_pb210_activity),
                               total_pb210_activity = se,
                               delta_c13 = as.numeric(δ13C)*1000) %>% #convert to ppm 
                        select(-depth_interval, -`cm decompressed`, -`Organic carbon`, -se, -`Dry bulk density`, -`Total -210Pb`, -δ13C)



#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

species <- data %>% select(site_id, core_id, Habitat, meadow) %>% 
                    na.omit(core_id) %>% 
                    rename(habitat = Habitat) %>% 
                    mutate(study_id = id,
                           species_code = case_when(meadow == "Bare" ~ "previously vegetated soils",
                                          TRUE ~ "Posidonia australis"),
                           code_type = case_when(species_code == "Posidonia australis" ~ "Genus species",
                                        TRUE ~ "description")) %>%
                    select(!meadow) %>% distinct()

species <- reorderColumns("species", species)

## ... Impacts ####

impacts <- data %>% select(site_id, core_id, meadow) %>% 
                    na.omit(core_id) %>% 
                    rename(impact_class = meadow) %>% distinct() %>% 
                    mutate(study_id = id,
                           impact_class = case_when(impact_class == "Resilient" ~ "natural",
                                                    impact_class == "Recovered" ~ "restored",
                                                    impact_class == "Bare" ~ "disturbed"))

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
write_csv(methods, "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018_methods.csv")
  #write_csv(sites, "data/primary_studies/Senger_et_al_2020/derivative/Senger_et_al_2020_sites.csv")
write_csv(cores, "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018_cores.csv")
write_csv(depthseries, "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018_depthseries.csv")
write_csv(species, "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018_species.csv")
write_csv(impacts, "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Lafratta_et_al_2018_dataset",
                             title = "Importance of habitat selection for Blue Carbon projects: Doubtful additionality in a seagrass case study",
                             author = "Anna Lafratta, Oscar Serano, Pere Masque, Miguel-Angel Mateo, Milena Fernandes, Sam Gaylard, Paul Lavery",
                             bibtype = "Misc", 
                             publication_type = "primary dataset",
                             doi = "10.25958/5b57cce84b1ce",
                             url = "https://ro.ecu.edu.au/datasets/38/",
                             year = "2018") %>% 
                  column_to_rownames("bibliography_id")

study_citation_article <- data.frame(bibliography_id = "Lafratta_et_al_2020",
                                     title = "Challenges to select suitable habitats and demonstrate ‘additionality’ in Blue Carbon projects: A seagrass case study",
                                     author = "A.Lafratta, O.Serrano, P.Masqué, M.A.Mateo, M.Fernandes, S.Gaylard, P.S.Lavery",
                                     bibtype = "Article",
                                     doi = "https://doi.org/10.1016/j.ocecoaman.2020.105295",
                                     url = "https://www.sciencedirect.com/science/article/pii/S0964569120302052?via%3Dihub",
                                     journal = "Ocean & Coastal Management",
                                     publication_type = "associated source",
                                     year = "2020") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("Lafratta_et_al_2018", "Lafratta_et_al_2020_paper"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Lafratta_et_al_2018/derivative/Lafratta_et_al_2018.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
