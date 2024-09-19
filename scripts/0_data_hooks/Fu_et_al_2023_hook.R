## CCRCN Data Library ########
## contact: cheneyr@si.edu

## Hook script for Fu et al 2023 article and dataset 
# article - https://www.nature.com/articles/s43247-023-01154-0
# data pub - https://figshare.com/articles/dataset/Bahamas_seagrass_sediment_data_xlsx/24418201/2 

#seagrass data from bahamas, worlds largest seagrass meadow 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
data1 <- read_xlsx("data/primary_studies/Fu_et_al_2023/original/46151427_Bahamas data.xlsx", sheet = 1)
data_pb <- read_xlsx("data/primary_studies/Fu_et_al_2023/original/46151427_Bahamas data.xlsx", sheet = 2)

## 1. Curation ####

id <- "Fu_et_al_2023"

## ... Methods ####

# curate materials and methods
methods <- tibble(
  study_id = id,
  coring_method = "push core",
  roots_flag = "roots and rhizomes separated", #visible inorganic material and shells also removed 
  compaction_flag = "compaction qualified",
  dry_bulk_density_temperature = 60,
  dry_bulk_density_flag = "to constant mass",
  carbonates_removed = TRUE,
  carbonate_removal_method = "direct acid treatment",
  fraction_carbon_method = "EA",
  fraction_carbon_type = "organic carbon",
  pb210_counting_method = "alpha"
)

methods <- reorderColumns("methods", methods)


## ... Depthseries ####


pb_depth <- data_pb %>% 
  filter(!`Bahamas Site 1` == "Internal diameter")



depthseries_curate <- data1 %>% 
  filter(!Depth == "cm") %>%  #filter out unit row
  rename(site_id = Site,
         core_id = Core,
         organic_carbon = Corg,
         dry_bulk_density = `Dry bulk density`,
         delta_c13 = `13C`) %>% 
  fill(core_id, site_id) %>% 
  mutate(study_id = id,
         method_id = "single set of methods",
         core_id = paste(site_id, core_id, sep = "_"),
         depth_min = case_when(Depth == "0-1" ~ 0, #fix encoding, error occurred when reading xlsx into r 
                               Depth == "44563"|Depth == "1-3.5" ~ 1,
                               Depth == "44595" ~ 2,
                               Depth == "44625"|Depth == "44624" ~ 3,
                               Depth == "44688"|Depth == "44657" ~ 5,
                               Depth == "44751"|Depth == "44721" ~ 7,
                               Depth == "44815" ~ 9,
                               Depth == "44878" ~ 11,
                               Depth == "13-15" ~ 13,
                               Depth == "3.5-4.5" ~ 3.5,
                               Depth == "4.5-6.5" ~ 4.5,
                               Depth == "6.5-8.5" ~ 6.5,
                               Depth == "8.5-10.5" ~ 8.5,
                               Depth == "10.5-12.5" ~ 10.5,
                               Depth == "12.5-14.5" ~ 12.5,
                               Depth == "14.5-16.5" ~ 14.5,
                               TRUE ~ NA),
         depth_max = case_when(Depth == "0-1" ~ 1, 
                               Depth == "44563"|Depth == "1-3.5" ~ 2,
                               Depth == "44595" ~ 3,
                               Depth == "44625"|Depth == "44624" ~ 5,
                               Depth == "44688"|Depth == "44657" ~ 7,
                               Depth == "44751"|Depth == "44721" ~ 9,
                               Depth == "44815" ~ 11,
                               Depth == "44878" ~ 13,
                               Depth == "13-15" ~ 15,
                               Depth == "3.5-4.5" ~ 4.5,
                               Depth == "4.5-6.5" ~ 6.5,
                               Depth == "6.5-8.5" ~ 8.5,
                               Depth == "8.5-10.5" ~ 10.5,
                               Depth == "10.5-12.5" ~ 12.5,
                               Depth == "12.5-14.5" ~ 14.5,
                               Depth == "14.5-16.5" ~ 16.5,
                               TRUE ~ NA)) %>% 
  select(-N, -P, -`N/P molar`, -Ca, -Mg, -`Ca/Mg molar`, -Cinorg) %>% #remove uncontrolled variables 
  select(-Depth)




depthseries <- reorderColumns("depthseries", depthseries_curate)


## ... Cores ####
#sampling occurred between nov 17th and nov 23rd 2021, date of each core unclear
#need to find coring/site locations, these don't seem to be listed in the paper or data pub 
#lead cores separate from other cores?? 

cores <- depthseries %>% 
  select(study_id, site_id, core_id) %>% distinct() %>% 
  mutate(habitat = "seagrass",
         year = 2021,
         month = 11,
         vegetation_class = "seagrass",
         vegetation_method = "field observation")


cores <- reorderColumns("cores", cores)


## ... Species #####

#pull dominant species from paper

species <- cores %>% select(study_id, site_id) %>% distinct() %>% 
  mutate(habitat = "seagrass",
         code_type = "Genus species",
         species_code = case_when(site_id == "S2" ~ "Thalassia testudinum & Syringodium filiforme",
                                  TRUE ~ "Thalassia testudinum")) %>% 
  separate_longer_delim(cols = species_code, delim = " & ")

species <- reorderColumns("species", species)




## 2. QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)
          #leaflet does not like special character in some site names 

#table names
table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

testUniqueCoords(cores)
testIDs(cores, depthseries, by = "core_id")
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## 3. Write datavis report ####
writeDataVizReport(id)

## 4. Study Citations ####

library(RefManageR)

databib <- as.data.frame(GetBibEntryWithDOI("10.6084/m9.figshare.24418201")) %>% 
  mutate(bibliography_id = "Fu_et_al_2023_data", 
         study_id = id, 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())


articlebib <- as.data.frame(GetBibEntryWithDOI("10.1038/s43247-023-01154-0")) %>% 
  mutate(bibliography_id = "Fu_et_al_2023_article", 
         study_id = id, 
         publication_type = "primary dataset") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())


study_citations <- full_join(databib, articlebib)

write_csv(study_citations, "data/primary_studies/Fu_et_al_2023/derivative/Fu_et_al_2023_study_citations.csv") 


## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(methods, "data/primary_studies/Fu_et_al_2023/derivative/Fu_et_al_2023_methods.csv")
write_csv(cores, "data/primary_studies/Fu_et_al_2023/derivative/Fu_et_al_2023_cores.csv") 
write_csv(depthseries, "data/primary_studies/Fu_et_al_2023/derivative/Fu_et_al_2023_depthseries.csv")
write_csv(species, "data/primary_studies/Fu_et_al_2023/derivative/Fu_et_al_2023_species.csv")



