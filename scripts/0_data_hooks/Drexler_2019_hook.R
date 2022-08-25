# Curation script for Drexler et al. 2019 
# Drexler, Judith Z., et al. 
# "Carbon accumulation and vertical accretion in a restored versus historic salt marsh 
# in southern Puget Sound, Washington, United States." Restoration Ecology.

# Contact: Michael Lonneman, lonnemanM@si.edu

library(tidyverse)
library(lubridate)
library(RefManageR)

raw_cores <- read_csv("./data/primary_studies/drexler_2019/original/coordinates.csv")
raw_depthseries <- read_csv("./data/primary_studies/drexler_2019/original/nisqually_depthseries_manual_edits.csv")
raw_species <- read_csv("./data/primary_studies/drexler_2019/original/species.csv")
raw_methods <- read_csv("./data/primary_studies/drexler_2019/original/drexler_et_al_2019_methods.csv")

## Curate data #########
study_id_value <- "Drexler_et_al_2019"

## ... methods ####
methods <- raw_methods %>% mutate(sediment_sieved_flag = "sediment sieved",
                                  method_id = "single set of methods")

## ... depthseries #####
depthseries <- raw_depthseries %>%
  rename(total_pb210_activity_sd = total_pb210_sd) %>%
  mutate(core_id = gsub("Core ", "", core_id)) %>%
  mutate(study_id = study_id_value,
         depth_min = depth_max - 2,
         site_id = ifelse(grepl("SG", core_id), "Six_Gill_Slough", "Animal_Slough"),
         method_id = "single set of methods") %>%
  # Modify carbon stock variables: 
  mutate(fraction_carbon = percent_carbon / 100,
         pb210_unit = ifelse(!is.na(total_pb210_activity), "distintegrationsPerMinutePerGram", NA),
         ra226_unit = ifelse(!is.na(ra226_activity), "distintegrationsPerMinutePerGram", NA)) %>%
  select(study_id, site_id, core_id, method_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon, 
         total_pb210_activity, total_pb210_activity_sd,  
         ra226_activity, ra226_activity_sd, ra226_unit,
         excess_pb210_activity, excess_pb210_activity_sd, pb210_unit)

## ... core data #######
coordinates <- raw_cores %>%
  filter(!is.na(Latitude)) %>%
  rename(core_latitude = Latitude,
         core_longitude = Longitude, 
         core_id = `Core abbreviations`,
         core_notes = `Full core ID`, 
         core_date = `Date of collection`) %>%
  mutate(study_id = study_id_value)

cores <- depthseries %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id)) %>%
  merge(coordinates, by="core_id", all.x=TRUE, all.y=TRUE) %>%
  mutate(core_date = mdy(core_date), 
         core_length_flag = "core depth limited by length of corer") %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select_if(function(x) {!all(is.na(x))}) %>%
  select(-c(core_date, `Wetland type`, Sample))

## ... site data #######
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- cores %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) 

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

sites <- cores %>%
  group_by(site_id) %>%
  summarize(study_id = study_id_value) %>%
  merge(site_boundaries, by="site_id") %>%
  mutate(salinity_class = "polyhaline",
         vegetation_class = "emergent") %>%
  select(study_id, site_id, everything())

## ... impact data #####
impacts <- cores %>%
  select(study_id, site_id, core_id) %>%
  filter(site_id == "Six_Gill_Slough") %>%
  mutate(impact_class = "tidally restored")
  
# ## ... species data ####
species <- cores %>%
  select(study_id, site_id, core_id) %>%
  filter(site_id == "Animal_Slough") %>%
  merge(raw_species, by="site_id", all.x=TRUE, all.y=TRUE)

## Create study-level data ######

if(!file.exists("./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_study_citations.csv")){
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI("10.1111/rec.12941")
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Drexler_et_al_2019_article", 
           study_id = study_id_value,
           publication_type = "associated source") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019.bib")
  write_csv(study_citations, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects
sites <- updated$sites
impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)
testTaxa(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## Export curated data ###########
write_csv(depthseries, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_depthseries.csv")
write_csv(cores, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_cores.csv")
write_csv(sites, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_sites.csv")
write_csv(impacts, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_impacts.csv")
write_csv(species, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_species.csv")
write_csv(methods, "./data/primary_studies/drexler_2019/derivative/drexler_et_al_2019_methods.csv")
