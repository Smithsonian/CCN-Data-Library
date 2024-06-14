## CCN Data Library ########
## contact: Rose Cheney, cheneyr@si.edu 

## Soil core data curation script for Johnson et al 2024 marsh dataset: Sediment carbon content of Maine salt marshes

#link to dataset: https://doi.org/10.25573/serc.17018816 


# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(knitr)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html


#load in data 
methods_raw <- read_csv("data/primary_studies/Johnson_et_al_2024_marsh/original/johnson_et_al_2024_materials_and_methods.csv")
cores_raw <- read_csv("data/primary_studies/Johnson_et_al_2024_marsh/original/johnson_et_al_2024_cores.csv")
depthseries_raw <- read_csv("data/primary_studies/Johnson_et_al_2024_marsh/original/johnson_et_al_2024_depthseries.csv")
species_raw <- read_csv("data/primary_studies/Johnson_et_al_2024_marsh/original/johnson_et_al_2024_species.csv")
study_citations_raw <- read_csv("data/primary_studies/Johnson_et_al_2024_marsh/original/johnson_et_al_2024_associated_publications.csv")
  
  
## 1. Curation ####

id <- "Johnson_et_al_2024_marsh"


## ... Methods ####

methods <- methods_raw %>% 
  mutate(method_id = study_id,
         study_id = id,
         dry_bulk_density_temperature = (dry_bulk_density_temperature_max + dry_bulk_density_temperature_min)/2,
         dry_bulk_density_time = ifelse(is.na(dry_bulk_density_time), (dry_bulk_density_time_max + dry_bulk_density_time_min)/2, dry_bulk_density_time),
         carbon_profile_notes = case_when(!is.na(dry_bulk_density_temperature) & !is.na(carbon_profile_notes) ~ paste0(carbon_profile_notes, " ", "Samples dried at 60-80 C. Time approximatley 24-48 hours."),
                                          !is.na(dry_bulk_density_temperature) ~ "Samples dried at 60-80 C. Time approximately 24-48 hours.")) %>% 
  select(-dry_bulk_density_time_min, -dry_bulk_density_time_max, -dry_bulk_density_temperature_min, -dry_bulk_density_temperature_max)

methods <- reorderColumns("methods", methods)


## ... Cores ####

cores <- cores_raw %>% 
  mutate(study_id = id,
         site_id = str_replace(site_id, " ", "_"),
         core_id = str_replace(core_id, " ", "_"))

cores <- reorderColumns("cores", cores)



## ... Depthseries ####

depthseries <- depthseries_raw %>% 
  mutate(method_id = study_id,
         study_id = id,
         core_id = str_replace(core_id, " ", "_"),
         site_id = str_replace(site_id, " ", "_")) %>%  #update study id to published dataset id
  select(-carbon_density, -fraction_nitrogen, -fraction_h2o, 
         -cn_ratio, -nitrogen_density, -N_umoles, -C_umoles, -delta_n15) #remove uncontrolled variables

depthseries <- reorderColumns("depthseries", depthseries)



## ... Species ####

species <- species_raw %>% 
  mutate(study_id = id,
         code_type = "Genus species",
         core_id = str_replace(core_id, " ", "_"),
         site_id = str_replace(site_id, " ", "_"))


species <- reorderColumns("species", species)


## ... Impacts ####

#create impacts table from core notes 

impacts <- cores %>% 
  select(study_id, site_id, core_id, core_notes) %>% 
  mutate(core_id = str_replace(core_id, " ", "_"),
         site_id = str_replace(site_id, " ", "_"),
         impact_class = case_when(grepl("ditch", core_notes) ~ "ditched",
                                  grepl("Tidally restored", core_notes) ~ "tidally restored",
                                  grepl("restrict", core_notes) ~ "tidally restricted",
                                  grepl("tidal flow was restored", core_notes) ~ "restored", 
                                  grepl("No anthropogenic", core_notes) ~ "natural",
                                  grepl("dug channels", core_notes) ~ "ditched",
                                  grepl("no history", core_notes) ~ "natural", 
                                  grepl("Sampled cores were", core_notes) ~ "natural", # check if this should be natural or diked and drained 
                                  TRUE ~ NA))


impacts <- reorderColumns("impacts", impacts)


#remove impacts from cores table before writing csvs 
cores <- cores %>% select(-core_notes)


## 2. QAQC ####

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
write_csv(methods, "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh_methods.csv")
write_csv(cores, "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh_cores.csv")
write_csv(depthseries, "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh_depthseries.csv")
write_csv(species, "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh_species.csv")

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
                             bibliography_id = "Johnson_et_al_2024_marsh_data",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Sediment carbon content of Maine salt marshes",
                             author = "Beverly J Johnson, Ashley Kulesza, Margaret Pickoff, Daniel Stames, Cailene Gunn, Brianna Karboski, Cameron Russ, Jaxine Wolfe, Phil Dostie",
                             doi = "10.25573/serc.17018816",
                             url = "https://doi.org/10.25573/serc.17018816",
                             year = "2024")

WriteBib(as.BibEntry(study_citation), "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh.bib")
write_csv(study_citation, "data/primary_studies/Johnson_et_al_2024_marsh/derivative/Johnson_et_al_2024_marsh_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/

## ... Write data vis report in docs folder ####
writeDataVizReport(id)

