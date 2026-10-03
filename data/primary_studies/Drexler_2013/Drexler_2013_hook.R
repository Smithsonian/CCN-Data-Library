# Curation script for Drexler et al 2013
# Waccamaw National Wildlife refuge

# Contact: Michael Lonneman, lonnemanM@si.edu

library(tidyverse)
library(lubridate)
library(RefManageR)

raw_cores <- read_csv("./data/primary_studies/drexler_2013/original/drexler_2019_initial_sites.csv")
raw_depthseries <- read_csv("./data/primary_studies/drexler_2013/original/drexler_et_al_2013_depthseries.csv")
raw_methods <- read_csv("./data/primary_studies/drexler_2013/original/drexler_et_al_2013_methods.csv")

## Curate data #########
study_id_value <- "Drexler_et_al_2013"

## clean up general core data
core_data <- raw_cores %>%
  mutate(site_id = gsub(" ", "_", site_id),
         core_date = as_date(core_date, format="%m/%d/%Y", tz="UTC")) %>%
  mutate(study_id = study_id_value,
         core_id = paste(site_id, core_number, sep = "_"),
         core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(-core_date) %>%
  separate(core_position, into=c("core_latitude", "core_longitude"), sep=",") %>%
  mutate(core_latitude = as.numeric(core_latitude), 
         core_longitude = as.numeric(core_longitude))
  
## ... methods ####
methods <- raw_methods %>% mutate(method_id = "single set of methods")

## ... site data #######
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- core_data %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) %>%
  filter(is.na(core_latitude) == FALSE)

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

sites <- core_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id),
            site_description = first(site_description)) %>%
  merge(site_boundaries, by="site_id") %>%
  select(study_id, site_id, everything())

## ... impact data #####
impacts <- core_data %>%
  select(study_id, site_id, core_id, impact_class)

## ... species data ####
species <- core_data %>%
  mutate(species_code = recode(species_code, 
                               "ZIMI" = "Zizaniopsis miliacea",
                               "SCTA2" = "Schoenoplectus tabernaemontani")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  filter(!is.na(species_code))

## ... depthseries #####

# NEEDS RESOLUTION

# original depthseries curation
# there was radioisotope information present...where did it go?
depthseries <- raw_depthseries %>%
#   mutate(site_id = gsub(" ", "_", site_id)) %>%
#   mutate(study_id = study_id_value,
#          core_id = paste(site_id, `core_number`, sep = "_")) %>%
  mutate(#fraction_carbon = organic_carbon_percent / 100,
         pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         cs137_unit = ifelse(!is.na(cs137_unit), "picocuriesPerGram", NA)) %>%
  select(study_id, site_id, core_id, depth_min, depth_max,
         dry_bulk_density, fraction_carbon,
         cs137_activity, cs137_activity_sd, cs137_unit,
         total_pb210_activity, total_pb210_activity_sd, excess_pb210_activity, excess_pb210_activity_sd, pb210_unit,
         age, age_sd, cs137_peak_present, depth_interval_notes)

# depthseries curation with recycled derivative table
# we couldn't find the original CSV that was extracted from the supplementary material
depthseries <- depthseries %>%
  mutate(method_id = "single set of methods")

# .... core data #####

cores <- core_data %>%
  mutate(core_length_flag = "core depth limited by length of corer") %>%
  # Filter out cores that aren't in the depthseries
  filter(core_id %in% depthseries$core_id) %>%
  select(-c(impact_class, species_code, core_notes, core_number, core_length, `Lat/Long`, site_description)) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_year, core_month, core_day, everything())


## Create study-level data ######
if(!file.exists("./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_study_citations.csv")){
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI("10.1007/s13157-013-0456-3")
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Drexler_et_al_2013_combined", 
           study_id = study_id_value,
           publication_type = "combined dataset and manuscript") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013.bib")
  write_csv(study_citations, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

# once the depthseries table is resolved, add it to the list of table names
table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects

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

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Export curated data ###########
write_csv(depthseries, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_depthseries.csv")
write_csv(cores, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_cores.csv")
write_csv(species, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_species.csv")
write_csv(impacts, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_impacts.csv")
write_csv(methods, "./data/primary_studies/drexler_2013/derivative/drexler_et_al_2013_methods.csv")

