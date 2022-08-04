## CCRCN Data Library
# contact: lonnemanm@si.edu

## 1. Citations  ###############
# Data Citation
# Thorne, K. 2015. Field and model data for studying the effects of sea-level rise on eight tidal marshes in coastal Washington and Oregon. 
# US Geological Survey Data Release. 10.5066/F7SJ1HNC.
# https://www.sciencebase.gov/catalog/item/55ae7d09e4b066a24924239f

# Publication Citation
# Karen Thorne, Glen MacDonald, Glenn Guntenspergen, Richard Ambrose, Kevin Buffington, Bruce Dugger, Chase Freeman, 
# Christopher Janousek, Lauren Brown, Jordan Rosencranz, James Holmquist, John Smol, Kathryn Hargan, and John Takekawa, 2018, 
# U.S. Pacific coastal wetland resilience and vulnerability to sea-level rise: Science Advances, v. 4, iss. 2.

## 2. Prep workspace #######################
library(tidyverse)
library(lubridate)
library(readxl)
library(RefManageR)
# the following packages are needed to convert UTM to lat/long
library(sp)
library(rgdal)
library(DataCombine)

## 3. Read in data #########################
# The soil core and depthseries data is spread across multiple sheets in an excel file. 
# Each core's depth series has it's own page but there is no core ID in the table. 
# Instead, the sheet name is the associated core name. 
# Each sheet will need to be read in as part of a loop

## ... 3A Assemble vector of core names and import ##################
core_ids <- c("BM01", "BM03", "BM05", "CB00", "CB03", 'CB06', "GH01", "GH03", "GH06", 
              "NQ01", "NQ04", "NQ06", "PS02", "PS04", 'PS05', "SZ02", "SZ03", "SZ05",
              "SK02", "SK04", "SK06", "WB01", "WB04", "WB06")

num_cores <- length(core_ids)

for(i in 1:num_cores) {
  d <- read_excel("./data/primary_studies/Thorne_2015_a/original/NWCSC Sediment Core Data.xlsx", 
                  sheet=as.character(core_ids[i]), na = "NA")
  d <- d %>%
    mutate(core_id = core_ids[i]) %>%
    rename(depth_min = "Depth (cm)",
           fraction_organic_matter = "Organic Content", 
           dry_bulk_density = "Bulk Density") %>%
    mutate(fraction_organic_matter = fraction_organic_matter,
           dry_bulk_density = dry_bulk_density)
  assign(core_ids[i],d)
}

## ... 3B Import core-level data 
raw_core_data <- read_excel("./data/primary_studies/Thorne_2015_a/original/NWCSC Sediment Core Data.xlsx", sheet="CoreSurveys_CS137")

## 4 Curate Data ##################

## ... 4A Append depthseries data, add appropriate core ID, and curate ############

# depthseries_data <- data.frame(matrix(nrow=0, ncol=4))
# colnames(depthseries_data) <- colnames(BM01)

core_ids <- list(BM01, BM03, BM05, CB00, CB03, CB06, GH01, GH03, GH06, 
              NQ01, NQ04, NQ06, PS02, PS04, PS05, SZ02, SZ03, SZ05,
              SK02, SK04, SK06, WB01, WB04, WB06)

depthseries_data <- data.frame() %>%
  bind_rows(core_ids) %>%
  mutate(depth_max = depth_min + 1, 
         fraction_organic_matter = fraction_organic_matter / 100,
         study_id = "Thorne_et_al_2015") %>%
  # turn negative fraction_organic_matter values to 0
  mutate(fraction_organic_matter = ifelse(fraction_organic_matter < 0, 0, fraction_organic_matter)) %>%
  select(study_id, core_id, depth_min, depth_max, fraction_organic_matter, dry_bulk_density)

# add site IDs to depthseries 
sites <- raw_core_data %>%
  mutate(core_id = paste(SiteCode, Core, sep="0")) %>%
  rename(site_id = Site) %>%
  select(core_id, site_id) 

depthseries_data <- depthseries_data %>% 
  merge(sites, 
        by="core_id", 
        all.x=TRUE, all.y=FALSE) %>%
  select(study_id, site_id, core_id, depth_min, depth_max, fraction_organic_matter, dry_bulk_density)

## ... 4B Curate core-level data #############
core_data <- raw_core_data %>%
  rename(core_elevation = `Elevation (m, NAVD88)`,
         cs137_peak_cm = `CS137 Peak (cm)`) %>%
  
  # Whomever uploaded this dataset to its public repository forgot a digit from
  #   a coordinate....
  mutate(Northing = ifelse(SiteCode == "CB" & Core == 5, (Northing * 10), Northing)) %>%
  mutate(core_id = paste(SiteCode, Core, sep="0"),
         study_id = "Thorne_et_al_2015", 
         core_position_method = "RTK", 
         core_elevation_datum = "NAVD88",
         zone = 10) 

# convert UTM to lat long
source("./scripts/1_data_formatting/curation_functions.R") 
output <- convert_UTM_to_latlong(core_data$Easting, core_data$Northing, core_data$zone, core_data$core_id)

# merge coordinates to core table and clean up table
core_data <- core_data %>%
  merge(output, by="core_id") %>%
  rename(site_id = Site) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_position_method, core_elevation, core_elevation_datum, cs137_peak_cm)

# There are some cores that have no information other than coordinates. ML and DK
#   agreed that there isn't much value in including these, and they inappropriately
#   inflate our total number of cores stats, so we'll remove
core_data <- core_data %>%
  filter(core_id %in% depthseries_data$core_id)

## ... 4C Add cs137 TRUE/FALSE value to depthseries #####
# If a given interval contains the cs137 peak, give it a TRUE value. 

peaks <- core_data %>%
  filter(cs137_peak_cm >= 0) %>%
  select(core_id, cs137_peak_cm)

depthseries <- depthseries_data %>%
  merge(peaks, by="core_id", all.x=TRUE, all.y=TRUE) %>%
  mutate(cs137_peak_present = ifelse(is.na(cs137_peak_cm)==TRUE, FALSE, ifelse(cs137_peak_cm == depth_min, TRUE, FALSE)),
         method_id = "single set of methods") %>%
  select(-cs137_peak_cm)

cores <- core_data %>% 
  select(-cs137_peak_cm) %>%
  mutate(core_elevation_method = "RTK",
         elevation_accuracy = 0.02,
         core_length_flag = "core depth limited by length of corer")
  

## ... methods ####

methods_raw <- read_csv("data/primary_studies/Thorne_2015_a/original/Thorne_et_al_2015_methods.csv")

methods <- methods_raw %>%
  mutate(method_id = "single set of methods") %>% 
  select_if(function(x) {!all(is.na(x))})


## ... 4D Generate study-citation link ############
study <- "Thorne_et_al_2015"

if(!file.exists("./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_study_citations.csv")){
  doi <- "10.5066/F7SJ1HNC"
  pub_dio <- "10.1126/sciadv.aao3270"
  
  biblio_raw <- GetBibEntryWithDOI(c(doi, pub_dio))
  # biblio_raw <- BibEntry(bibtype = "Misc", 
  #                              key = "Thorne_et_al_2015", 
  #                              title = "Marshes to Mudflats: Climate Change Effects Along a Latitudinal Gradient in the Pacific Northwest",
  #                              author = "U.S. Geological Survey {Karen Thorne}", 
  #                              doi = "10.5066/f7sj1hnc",
  #                              publisher = "U.S. Geological Survey",
  #                              year = "2015", 
  #                              url = "https://www.sciencebase.gov/catalog/item/5006e99ee4b0abf7ce733f58"
  # )
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = c("Thorne_et_al_2015_data", "Thorne_et_al_2015_article"), 
           study_id = study,
           publication_type = c("primary dataset", "associated source")) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015.bib")
  write_csv(study_citations, "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries")

updated <- updateTables(table_names)

# save listed tables to objects

methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores

## 5. QA/QC of data ################
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

## 6. Export data
write_csv(cores, "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_cores.csv")
write_csv(depthseries, "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_depthseries.csv")
write_csv(methods, "./data/primary_studies/Thorne_2015_a/derivative/Thorne_et_al_2015_methods.csv")

