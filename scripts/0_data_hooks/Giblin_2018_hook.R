## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Citations for data and publication ##########

# Data citation: 
# Giblin A., I. Forbrich. 2018. PIE LTER high marsh sediment chemistry and activity measurements, 
# Nelson Island Creek marsh, Rowley, MA. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/d1d5cbf87602ccf51de30b87b8e46d01. Dataset accessed 1/29/2019.

# Publication: 
# Forbrich, I., A. E. Giblin, and C. S. Hopkinson. 2018. 
# “Constraining Marsh Carbon Budgets Using Long‐Term C Burial and Contemporary Atmospheric CO2 Fluxes.” 
# Journal of Geophysical Research: Biogeosciences 123 (3): 867–78. https://doi.org/10.1002/2017JG004336.


## 2. Prep workspace and scrape data #######################
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/427/1/9264db472a63733e8489e8db67846a31" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Date",     
                 "Core.ID",     
                 "Latitude",     
                 "Longitude",     
                 "Elevation",     
                 "Name.per.Vegetation",     
                 "section",     
                 "section.depth..paren.cm.paren.",     
                 "bulk.density..paren.g.per.cm3.paren.",     
                 "C.percent.",     
                 "N.percent.",     
                 "v_137Cs..paren.mBq.per.g.paren.",     
                 "v_210Pb..paren.Bq.per.g.paren.",     
                 "v_214Pb..paren.Bq.per.g.paren.",     
                 "v_214.Bi..paren.Bq.per.g.paren."    ), check.names=TRUE)

write_csv(dt1, "./data/Giblin_2018/original/MAR-NE-MarshSedChemActivity.csv")

# if you read in the original data from the CCRCN library: 
#dt1 <- read.csv("./data/Giblin_2018/original/MAR-NE-MarshSedChemActivity.csv")
#dt1 <- select(dt1, -X)

## 3. Process data to meet CCRCN standards ############

## ... 3A. Prep depthseries data ############
depthseries_data <- dt1 %>%
  rename(core_id = Core.ID, 
         sample_id = section,
         dry_bulk_density = bulk.density..paren.g.per.cm3.paren., 
         section_depth = section.depth..paren.cm.paren., 
         cs137_activity = v_137Cs..paren.mBq.per.g.paren., 
         total_pb210_activity = v_210Pb..paren.Bq.per.g.paren.) %>%
  mutate(fraction_carbon = C.percent. / 100,
         # cs137 is in mBq. per gram, need to be in becquerel per kilogram
         # no conversion of cs137 is necc because the conversion factors cancel out (1000/1000)         
         # pb210 is in Bq. per gram, needs to be bq. per kg
         total_pb210_activity = total_pb210_activity * 1000) %>%
  # create unique core IDs
  mutate(core_id = paste("Giblin2018", gsub(" ", "_", core_id), sep=""),
         study_id = "Giblin_and_Forbrich_2018") %>%
  select(core_id, sample_id, study_id, dry_bulk_density, fraction_carbon, section_depth,
         cs137_activity, total_pb210_activity)

## ... ... 2Ai. calculate min and max depth variables ###############
# I am sure there is a way to do this in dplyr but I can't figure it out right now: 
min <- 0
max <- 0
depth_min <- vector(length = nrow(depthseries_data))
depth_max <- vector(length = nrow(depthseries_data))

# for each core, iterate on the section_depth to calculate min and max depth for each section
for (i in 1:nrow(depthseries_data)) {
  if(depthseries_data[i,"sample_id"] == 1) {
    min <- 0
    max <- 0
  }
  max <- max + depthseries_data[i,"section_depth"]
  depth_max[i] <- max
  depth_min[i] <- min
  min <- max
}

depthseries_data <- cbind(depthseries_data, depth_max, depth_min)
depthseries_data <- select(depthseries_data, -sample_id, -section_depth)

## ... 2B. core level data ###################
cores <- dt1 %>%
  rename(core_id = Core.ID,
         core_date = Date, 
         core_latitude = Latitude,
         core_longitude = Longitude,
         core_elevation = Elevation) %>% 
  # create unique core IDs
  mutate(core_id = paste("Giblin2018", gsub(" ", "_", core_id), sep="")) %>%
  group_by(core_id) %>%
  summarize(core_date = first(core_date), core_latitude = first(core_latitude), 
            core_longitude = first(core_longitude), core_elevation = first(core_elevation)) %>%
  mutate(study_id = "Giblin_and_Forbrich_2018", 
         core_length_flag = "core depth limited by length of corer", 
         site_id = "Nelson_Island_Creek", 
         core_elevation_datum = "NAVD88") %>%
  select(study_id, site_id, core_id, everything())

## ... 2C. Vegetation data #####################
veggies <- dt1 %>%
  rename(core_id = Core.ID,
         species_code = Name.per.Vegetation) %>% 
  # create unique core IDs
  mutate(core_id = paste("Giblin2018", gsub(" ", "_", core_id), sep="")) %>%
  group_by(core_id) %>%
  summarize(study_id = "Giblin_and_Forbrich_2018", 
         site_id = "Nelson_Island_Creek",
         species_code = first(ifelse(species_code == "S. alterniflora", "Spartina alterniflora", "Spartina patens")))
  
## ... 2D. Site data ########################
site_data <- cores %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude, core_elevation)

# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)
# remove NAs before aggregation
site_data <- na.omit(site_data)

# Now aggeregate data to the site level
site_data <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  mutate(site_description = "Plum Island Sound Estuary, Massachusetts, USA", 
         vegetation_class = "seagrass")

## 3. Create study-level data ######
# import the CCRCN bibliography 
library(bib2df)
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

# link each study to primary citation and join with synthesis table
studies <- unique(cores$study_id)

study_data_primary <- CCRCN_bib %>%
  select(BIBTEXKEY, CATEGORY, DOI) %>%
  rename(bibliography_id = BIBTEXKEY,
         study_type = CATEGORY,
         doi = DOI) %>%
  filter(bibliography_id %in% studies) %>%
  mutate(study_id = bibliography_id, 
         study_type = tolower(study_type)) %>%
  select(study_id, study_type, bibliography_id, doi) 

## 4. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("cores", cores)
test_colnames("sites", site_data) 
test_colnames("depthseries", depthseries_data)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries_data)

## 5. Write data ################
write_csv(site_data, "./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_sites.csv")
write_csv(veggies, "./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_species.csv")
write_csv(cores, "./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_cores.csv")
write_csv(depthseries_data, "./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_depthseries.csv")
write_csv(study_data_primary, "./data/Giblin_2018/derivative/Giblin_and_Forbrich_2018_study_citations.csv")
