## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

# This is a hook script for the Wetland Paleoecological Study of Southwest Coastal Louisiana: Sediment Cores and Diatom Calibration Dataset

## 1. Citations ###########

# Data Citation
# Smith, K.E.L., Flocks, J.G., Steyer, G.D., and Piazza, S.C., 2015, 
# Wetland paleoecological study of southwest coastal Louisiana—Sediment cores and diatom calibration dataset: 
# U.S. Geological Survey Data Series 877, https://dx.doi.org/10.3133/ds877.

# Publication Citation (PhD Thesis)
# Smith, Kathryn E.L., 2012, 
# Paleoecological study of coastal marsh in the Chenier Plain, Louisiana: Investigating the diatom composition of hurricane-deposited 
# sediments and a diatom-based quantitative reconstruction of sea-level characteristics. 
# PhD Thesis. University of Florida


## 2. Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(tidyverse)
library(lubridate)
library(RefManageR)

## 3. Data Location ########

# I'm unable to scrape files from USGS. Until I figure it out, I'll download and place in original folder. 

URL <- "https://pubs.usgs.gov/ds/0877/html/ds877_data.html"

## 4. Import data ####################

raw_depthseries <- read.csv("./data/primary_studies/Smith_2015/original/sediment_core_properties.txt")
raw_cores <- read.csv("./data/primary_studies/Smith_2015/original/sediment_core_sites.txt")
raw_methods <- read_csv("data/primary_studies/Smith_2015/original/Smith_et_al_2015_methods.csv")

## Methods ####

methods <- raw_methods %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  mutate(study_id = "Smith_et_al_2015",
         sediment_sieved_flag = "sediment sieved",
         method_id = "single set of methods")

## ... 4A. Core-level data ###########
# Although the original file says sites, it is the core-level data. 

cores <- raw_cores %>%
  rename(core_id = Site, 
         core_latitude = Latitude, 
         core_longitude = Longitude,
         core_date = Date) %>%
  mutate(core_date = as.Date(core_date, format = "%m/%d/%Y")) %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>% 
  mutate(core_position_method = "handheld", 
         study_id = "Smith_et_al_2015", site_id = "09WCC01") %>%
  select(-c(core_date, Time, SW_depth, SW_salinity, PW10_salinity, PW30_salinity)) %>% 
  select(core_id, site_id, study_id, core_latitude, core_longitude, core_position_method, everything())
# core diameter: 10cm

## ... 4B. Depthseries data ##########

depthseries <- raw_depthseries %>%
  rename(core_id = Sample, 
         dry_bulk_density = BD,
         fraction_organic_matter = LOI,
         total_pb210_activity = Pb210, total_pb210_activity_sd = Pb_error,
         ra226_activity = Ra226, ra226_activity_sd = Ra_error,
         cs137_activity = Cs137, cs137_activity_sd = Cs_error) %>%
  # create unit code for dating methods
  mutate(pb210_unit = ifelse(is.na(total_pb210_activity) == FALSE, "disintegrationsPerMinutePerGram", NA),
         cs137_unit = ifelse(is.na(cs137_activity) == FALSE, "disintegrationsPerMinutePerGram", NA),
         ra226_unit = ifelse(is.na(ra226_activity) == FALSE, "disintegrationsPerMinutePerGram", NA)) %>%
  # convert LOI to a fraction
  mutate(fraction_organic_matter = fraction_organic_matter / 100) %>%
  separate(core_id, into=c("core_id", "depth_interval"), sep=10) %>%
  # establish min and max depths for each sample
  mutate(depth_interval = as.numeric(gsub("_", "", depth_interval))) %>%
  mutate(depth_min = depth_interval - 1, 
         depth_max = depth_interval + 1,
         study_id = "Smith_et_al_2015", 
         site_id = "09WCC01",
         method_id = "single set of methods") %>%
  select(study_id, site_id, core_id, method_id, depth_min, depth_max,
         dry_bulk_density,
         fraction_organic_matter,
         cs137_activity, cs137_activity_sd, cs137_unit,
         total_pb210_activity, total_pb210_activity_sd,
         ra226_activity, ra226_activity_sd, pb210_unit)

## ... 4C. Site-level data ##########
site_data <- cores %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude)

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
  mutate(site_description = "Rockefeller Wildlife Refuge")

## 5. Create study-citation table ######

if(!file.exists("./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_study_citations.csv")){
  Smith_thesis <- read_csv("data/primary_studies/Smith_2015/intermediate/Smith_2015_thesis_citation.csv") %>%
    mutate_all(as.character)
  
  study <- "Smith_et_al_2015"
  doi <- "10.3133/ds877"
  
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI(doi)
  
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Smith_et_al_2015_data", 
           study_id = study,
           publication_type = "primary dataset") %>%
    remove_rownames() %>%
    bind_rows(Smith_thesis) %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015.bib")
  write_csv(study_citations, "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_study_citations.csv")
  
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "methods", "cores", "depthseries")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores

## 6. QA/QC  ################
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 7. Write data ##################
write_csv(cores, "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_cores.csv")
write_csv(sites, "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_sites.csv")
write_csv(depthseries, "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_depthseries.csv")
write_csv(methods, "./data/primary_studies/Smith_2015/derivative/Smith_et_al_2015_methods.csv")

