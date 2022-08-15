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
# library(rvest)
# library(stringr)
library(tidyverse)
library(lubridate)
library(RefManageR)
library(readxl)

# DATA DOWNLOAD WORKFLOW ARCHIVED

# infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/427/1/9264db472a63733e8489e8db67846a31" 
# infile1 <- sub("^https","http",infile1) 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                , col.names=c(
#                  "Date",     
#                  "Core.ID",     
#                  "Latitude",     
#                  "Longitude",     
#                  "Elevation",     
#                  "Name.per.Vegetation",     
#                  "section",     
#                  "section.depth..paren.cm.paren.",     
#                  "bulk.density..paren.g.per.cm3.paren.",     
#                  "C.percent.",     
#                  "N.percent.",     
#                  "v_137Cs..paren.mBq.per.g.paren.",     
#                  "v_210Pb..paren.Bq.per.g.paren.",     
#                  "v_214Pb..paren.Bq.per.g.paren.",     
#                  "v_214.Bi..paren.Bq.per.g.paren."    ), check.names=TRUE)
# 
# write_csv(dt1, "./data/Giblin_2018/original/MAR-NE-MarshSedChemActivity.csv")

# read in the original data from the CCRCN library: 
dt1 <- read.csv("./data/primary_studies/Giblin_2018/original/MAR-NE-MarshSedChemActivity.csv")
raw_methods <- read_xlsx("data/primary_studies/Giblin_2018/intermediate/giblin_2018_methods.xlsx", sheet = 2)
## 3. Process data to meet CCRCN standards ############

## ... Methods ####
# Jaxine edits 

# curate materials and methods
methods <- raw_methods %>%
  select_if(function(x) {!all(is.na(x))})

## ... 3A. Prep depthseries data ############
depthseries <- dt1 %>%
  rename(core_id = Core.ID, 
         dry_bulk_density = bulk.density..paren.g.per.cm3.paren., 
         section_depth = section.depth..paren.cm.paren., 
         cs137_activity = v_137Cs..paren.mBq.per.g.paren., 
         total_pb210_activity = v_210Pb..paren.Bq.per.g.paren.,
         bi214_activity = v_214.Bi..paren.Bq.per.g.paren.,
         pb214_activity = v_214Pb..paren.Bq.per.g.paren., 
         fraction_carbon = C.percent.) %>%
  mutate(fraction_carbon = as.numeric(fraction_carbon)) %>%
  mutate(fraction_carbon = ifelse(is.nan(fraction_carbon)==TRUE, NA, fraction_carbon), 
         pb214_activity = ifelse(is.nan(pb214_activity)==TRUE, NA, fraction_carbon)) %>%
  mutate(fraction_carbon = fraction_carbon / 100,
         cs137_unit = "microbecquerelPerGram", # metadata indicates microbecquerelPerGram, not millibecquerelPerGram
         pb210_unit = "becquerelPerGram", 
         pb214_unit = ifelse(is.na(pb214_activity) == FALSE, "becquerelPerGram", NA),  
         bi214_unit = "becquerelPerGram") %>%
  # create unique core IDs
  mutate(core_id = paste("Giblin2018", gsub(" ", "_", core_id), sep=""),
         study_id = "Giblin_and_Forbrich_2018",
         site_id = "Nelson_Island_Creek",
         method_id = "single set of methods") %>%
  mutate(depth_min = section - 1, 
         depth_max = section + 1) %>%
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, 
         dry_bulk_density, fraction_carbon, 
         cs137_activity, cs137_unit, total_pb210_activity, pb210_unit,
         pb214_activity, pb214_unit, bi214_activity, bi214_unit)

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
  mutate(core_length_flag = "core depth limited by length of corer", 
         core_elevation_datum = "NAVD88", 
         site_id = "Nelson_Island_Creek", 
         study_id = "Giblin_and_Forbrich_2018") %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(-core_date) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_elevation, core_elevation_datum, core_length_flag,
         everything())

## ... 2C. Vegetation data #####################
species <- dt1 %>%
  rename(core_id = Core.ID,
         species_code = Name.per.Vegetation) %>% 
  # create unique core IDs
  mutate(core_id = paste("Giblin2018", gsub(" ", "_", core_id), sep="")) %>%
  group_by(core_id) %>%
  summarize(study_id = "Giblin_and_Forbrich_2018", 
         site_id = "Nelson_Island_Creek",
         species_code = first(ifelse(species_code == "S. alterniflora", "Spartina alterniflora", "Spartina patens"))) %>%
  select(study_id, site_id, core_id, species_code)
  
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
sites <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  mutate(site_description = "Plum Island Sound Estuary, Massachusetts, USA", 
         vegetation_class = "emergent")

## 3. Create study-level data ######

if(!file.exists("data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_study_citations.csv")){
  doi <- "10.6073/pasta/d1d5cbf87602ccf51de30b87b8e46d01"
  study <- "Giblin_and_Forbrich_2018"
  
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI(doi)
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Giblin_and_Forbrich_2018_data", 
           study_id = study,
           publication_type = "primary dataset") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018.bib")
  write_csv(study_citations, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("cores", "depthseries", "sites", "species", "methods")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## 4. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 5. Write data ################
write_csv(methods, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_methods.csv")
write_csv(sites, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_sites.csv")
write_csv(species, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_species.csv")
write_csv(cores, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_cores.csv")
write_csv(depthseries, "./data/primary_studies/Giblin_2018/derivative/Giblin_and_Forbrich_2018_depthseries.csv")
