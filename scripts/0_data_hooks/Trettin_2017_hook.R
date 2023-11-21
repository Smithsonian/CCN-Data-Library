## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Data and publication citations #########
# Data citation: 
# Trettin, Carl C.; Stringer, Christina E.; Zarnoch, Stanley J.; Tang, Wenwu; Dai, Zhaohua. 2017. 
# Mangrove carbon stocks in Zambezi River Delta, Mozambique. Fort Collins, CO: Forest Service Research Data Archive. 
# https://doi.org/10.2737/RDS-2017-0053

## 2. Prep workspace and read in data ####################
# Load RCurl, a package used to download files from a URL

library(tidyverse)
library(readxl)
library(RefManageR)

raw_depthseries <- read_csv("./data/primary_studies/Trettin_2017/original/Zambezi_Soils.csv")

## 3. Curate data #############

study <- "Trettin_et_al_2017"

## ... Depth series data ###################
# Issues: 
# 1. Depthseries in this data does not reflect the entire core interval. 
# About 4 to 6 five cm intervals were extracted from 200 cm cores and tested

depthseries <- raw_depthseries %>%
  mutate(site_id = as.character(Plot)) %>%
  mutate(study_id = study,
         method_id = "single set of methods") %>%
  # Paste plot and subplot values to create a unique core ID 
  mutate(core_id = as.factor(paste(Plot, Subplot, sep="_"))) %>%
  # One sample interval has double _ _ 
  # Rename variables
  rename(dry_bulk_density = 'Bulk Density (g cm-3)') %>%
  rename(percent_carbon = '%C') %>%
  mutate(fraction_carbon = percent_carbon / 100) %>%
  separate(col='Actual Sample Interval', into=c("depth_min", "depth_max"), sep="_") %>%
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, fraction_carbon) %>%
  mutate(depth_min = ifelse(is.na(depth_max==TRUE),100,depth_min)) %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  arrange(study_id, site_id, core_id, depth_min) %>% 
  # Jaxine Edits
  # there are two intervals where the depth_max < depth_min
  mutate(depth_max = case_when(core_id == "9008_2" & depth_min == 190 ~ 195,
                               core_id == "9018_4" & depth_min == 177 ~ 182,
                               T ~ depth_max))

## ... Core-level ###########
# We'll scale up to the core-level from the depthseries
coreLocations <- read_csv("./data/primary_studies/Trettin_2017/original/Zambezi_PlotLocations.csv")
coreLocations <- coreLocations %>%
  mutate(site_id = as.character(Plot)) %>%
  rename(species_code = 'Dominant Species')

species <- coreLocations %>% 
  mutate(species_code = strsplit(as.character(species_code), "; ")) %>% 
  unnest(species_code) %>%
  mutate(study_id = study) %>%
  select(study_id, site_id, species_code) %>%
  arrange(study_id, site_id, species_code)

print(unique(species$species_code))

species <- species %>%
  mutate(
    species_code = recode_factor(
      species_code,
      "H. Littoralis" = "Heritiera littoralis",
      "B. Gymnorrhiza" = "Bruguiera gymnorrhiza",
      "R. Mucronata" = "Rhizophora mucronata",
      "X. Granatum" = "Xylocarpus granatum",
      "A. Marina" = "Avicennia marina",
      "C. Tagal" = "Ceriops tagal",
      "S. Alba" = "Sonneratia alba"
    )
  )

cores <- depthseries %>%
  select(study_id, site_id, core_id) %>%
  distinct() %>%
  left_join(coreLocations) %>%
  mutate(core_length_flag = "not specified") %>% 
  rename(core_latitude = Lati, core_longitude = Long) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_length_flag)

library(leaflet)
leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~core_longitude, lat = ~core_latitude, radius = 2)

## ... Site-level ##########
sites <- cores %>%
  group_by(study_id, site_id) %>% 
  summarize(site_longitude_max = max(core_longitude),
            site_longitude_min = min(core_longitude),
            site_latitude_max = max(core_latitude),
            site_latitude_min = min(core_latitude)) %>% 
  ungroup()

# West_Bounding_Coordinate: 36.30681
# East_Bounding_Coordinate: 36.11881
# North_Bounding_Coordinate: -18.89591
# South_Bounding_Coordinate: -18.80846

## 4. Create study-citation table ######
# Get bibtex citation from DOI

if(!file.exists("./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")){
  doi <- "10.2737/RDS-2017-0053"
  
  biblio_raw <- GetBibEntryWithDOI(doi)
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Trettin_et_al_2017_data", 
           study_id = study,
           publication_type = "primary dataset", 
           year = 2017) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Trettin_2017/derivative/Trettin_2017.bib")
  write_csv(study_citations, "./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_study_citations.csv")
  
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
# methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## 5. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)
testIDs(cores, depthseries, "core")
fractionNotPercent(depthseries)
results <- testNumericCols(depthseries)

## 6. Write data ##############
write_csv(sites, "./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_sites.csv")
write_csv(cores, "./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_cores.csv")
write_csv(depthseries, "./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_depthseries.csv")
write_csv(species, "./data/primary_studies/Trettin_2017/derivative/Trettin_et_al_2017_species.csv")

