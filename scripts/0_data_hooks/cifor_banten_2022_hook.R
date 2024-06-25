## CIFOR data ingestion - Banten 2022 Dataset, Royna et al 2024
# contact: Rose Cheney , cheneyr@si.edu

library(dplyr)
library(tidyverse)
library(readxl)
library(parzer)
library(leaflet)
library(lubridate)
library(RefManageR)


#helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC
source("scripts/1_data_formatting/cifor_utility_functions.R") # for CIFOR

#curate new CIFOR dataset 

soil <- read_xlsx("data/primary_studies/CIFOR/Banten_2022/SWAMP Data-Soil carbon-Pulau Dua Banten-2022.xlsx", sheet = 2)
veg1 <- read_xlsx("data/primary_studies/CIFOR/Banten_2022/SWAMP Data-Trees-Pulau Dua Banten-2022.xlsx", sheet = 2)
veg2 <- read_xlsx("data/primary_studies/CIFOR/Banten_2022/SWAMP Data-Trees-Pulau Dua Banten-2022.xlsx", sheet = 3)
necromass <- read_xlsx("data/primary_studies/CIFOR/Banten_2022/SWAMP Data-Necromass-Pulau Dua Banten-2022.xlsx", sheet = 2)
flux <- read_xlsx("data/primary_studies/CIFOR/Banten_2022/SWAMP Data-GHG flux-Pulau Dua Banten-2023.xlsx", sheet = 2)


# assign study id
id <- "Royna_et_al_2024" #<- pulled from dataset citation


# curate soils data for depthseries and cores
soil_curate <- soil %>% 
  fill(`Land Use`, .direction = "down") %>% 
  rename(depth = `Sample Depth Range (cm)`) %>% 
  mutate(study_id = id,
         site_id = "BAN",
         `Land Use` = case_when(`Land Use` == "dense silvofishery pond" ~ "Dense_Silvofishery",
                          `Land Use` == "sparse silvofishery pond" ~ "Sparse_Silvofishery",
                          `Land Use` == "non silvofishery pond" ~ "Pond",
                          TRUE ~ `Land Use`),
         core_id = paste(site_id, `Land Use`, Plot, sep = "_"),
         year = 2022)


## ... Methods ####
#Methods table from recommended methodology taken from Kauffman and Donato 2012, cited by CIFOR database 
methods <- data.frame(
             study_id = id,
             method_id = "CIFOR",
             coring_method = "gouge auger",
             roots_flag = "roots and rhizomes included",
             sediment_sieved_flag = "sediment not sieved",
             compaction_flag = "not specified",
             dry_bulk_density_temperature = 60,
             dry_bulk_density_flag = "to constant mass",
             carbon_measured_or_modeled = "measured",
             fraction_carbon_method = "EA",
             fraction_carbon_type = "total carbon", 
             carbonates_removed = "FALSE",
             carbonate_removal_method = "none specified") 




## ... Cores #####

#site bounding latitude between -6.1847 to -6.20139
#site bounding longitude between 106.1939 to 106.2206


cores <- soil_curate %>% 
  select(study_id, site_id, core_id, Plot, `Salinity (ppt)`) %>% 
  mutate(habitat = "mangrove",
         salinity_class = "estuarine", #range from 14-29 ppt
         salinity_method = "measurement",
         latitude = -6.1847,
         longitude = 106.1939,
         position_method = "other low resolution",
         position_notes = "site-level position, minimun listed bounding longitude and latitude",
         year = 2022) %>% 
  select(-Plot, -`Salinity (ppt)`) %>% distinct()


cores <- reorderColumns("cores", cores)


## ... Depthseries ####

depthseries <- soil_curate %>% 
  select(-`Land Use`, -Plot) %>% 
  rename(dry_bulk_density = `Soil Bulk Density (g/cm^3)`) %>% 
  mutate(fraction_carbon = `%C`/100,
         method_id = "single set of methods") %>% 
  separate_wider_delim(cols = depth, names = c("depth_min", "depth_max"), delim = "-") %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon)  #select for controlled variables 

#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)



## ... Impacts ####
#based off of associated paper and land use class listed 

impacts <- soil_curate %>% select(study_id, site_id, core_id, `Land Use`) %>% 
  mutate(impact_class = case_when(`Land Use` == "Fringe" ~ "natural",
                                  `Land Use` == "Interior" ~ "natural",
                                  `Land Use` == "Pond" ~ "natural",
                                  grepl("Silvofishery", `Land Use`) ~ "farmed")) %>% 
  select(-`Land Use`) %>% distinct()


impacts <- reorderColumns("impacts", impacts)


## ... Plot summary ####


## ... Plants ####



## allometric_eq table 
allometric_eq <- plant %>% 
  select(study_id, allometric_eq_id, genus, species, alive_or_dead) %>% distinct() %>% 
  filter(!is.na(allometric_eq_id))




## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~ site_id)


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
#testNumericCols(depthseries)
test_numeric_vars(depthseries) ##testNumericCols producing error message 
test_numeric_vars(cores)

## 3. Write Curated Data ####

# write data to final folder

write_csv(methods, "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024_methods.csv")
write_csv(cores, "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024_depthseries.csv")
write_csv(impacts, "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024_impacts.csv")

#veg tables 
      # write_csv(plot_summary, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plot_summary.csv")
      # write_csv(plant_plot_detail, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plant_plot_summary.csv")
      # write_csv(plant, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_plant.csv")
      # write_csv(allometric_eq, "data/primary_studies/CIFOR/derivative_ALT/cifor_alt_allometric_eq.csv")
      # 

## Write citations and bib file 

#associated paper 
    # Royna M, Murdiyarso D, Sasmito SD, Arriyadi D, Rahajoe JS, Zahro MG, Ardhani TSP. 2024. 
    # Carbon stocks and effluxes in mangroves converted into aquaculture: a case study from Banten Province, Indonesia. 
    # Front. Ecol. Evol. 12:1-12. https://doi.org/10.3389/fevo.2024.1340531 doi: https://doi.org/10.3389/fevo.2024.1340531

paper_citation <- dataset_citation <- data.frame(study_id = id,
                                                 bibliography_id = "Royna_et_al_2024_paper",
                                                 publication_type = "article",
                                                 bibtype = "Article", 
                                                 title = "Carbon stocks and effluxes in mangroves converted into aquaculture: a case study from Banten Province, Indonesia",
                                                 author = "Royna M, Murdiyarso D, Sasmito SD, Arriyadi D, Rahajoe JS, Zahro MG, Ardhani TSP",
                                                 doi = "https://doi.org/10.3389/fevo.2024.1340531",
                                                 url = "https://doi.org/10.3389/fevo.2024.1340531",
                                                 journal = "Frontiers in Ecology and Evolution",
                                                 year = "2024")

#soils dataset citation
dataset_citation <- data.frame(study_id = id,
                               bibliography_id = "Royna_et_al_2024_data",
                               publication_type = "primary dataset",
                               bibtype = "Misc", 
                               title = "SWAMP Dataset-Soil-Banten-2022",
                               author = "Milkah Royna; Daniel Murdiyarso; Sigit D. Sasmito; Desra Arriyadi; Joeni Setijo Rahajoe; Mufidah Ghina Zahro; Trialaksita Sari Priska Ardhani",
                               doi = " https://doi.org/10.17528/CIFOR/DATA.LKVADC",
                               url = "https://data.cifor.org/dataset.xhtml?persistentId=doi%3A10.17528%2FCIFOR%2FDATA.LKVADC",
                               year = "2024")

study_citations <- full_join(dataset_citation, paper_citation)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024.bib")
write_csv(study_citations, "data/primary_studies/CIFOR/Banten_2022/derivative/Royna_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/







