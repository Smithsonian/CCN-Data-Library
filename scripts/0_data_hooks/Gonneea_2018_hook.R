## CCRCN Data Library
# contact: klingesd@si.edu

# Data citation: 
# Gonneea, M.E., O'Keefe Suttles, J.A., and Kroeger, K.D., 2018, 
# Collection, analysis, and age-dating of sediment cores from salt marshes on the south shore of Cape Cod, Massachusetts, from 2013 through 2014: 
# U.S. Geological Survey data release, https://doi.org/10.5066/F7H41QPP.


# This script hooks in data from the Gonneea et al 2018 data release

## Assumptions made about data ###############

# that lat and long is in WGS84


## Prep workspace #######################
# Load RCurl, a package used to download files from a URL
# library(RCurl)
library(tidyverse)
library(lubridate)
library(RefManageR)
library(readxl)

source("./scripts/1_data_formatting/qa_functions.R")

## Download data ########################

## DATA DOWNLOAD WORKFLOW ARCHIVED

# The Gonneea et al (2018) data release features a diverse suite of file types:
#   a .jpg, a .xlsx, a .csv, and a .xml
# So we'll need to have a custom hook for each file

# url_list <- list("https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__6f%2F73%2F4b%2F6f734b0239c27f78c7f347dcf277c491a4a47903",
#                  "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__7d%2Fe4%2Fdc%2F7de4dc002db596e1d7fbe8254de9ccc3af05ae3b",
#                  "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__70%2F1e%2F99%2F701e99829e5860c1a0dc512056e5d71ff292dc19",
#                  "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__3e%2F2d%2Ff5%2F3e2df544c537a35007214d1fe595b45499df2f4a")
# 
# # Extract Saltmarsh_AR.jpg
# download.file(url_list[[1]], "./data/Gonneea_2018/original/Saltmarsh_AR.jpg",
#               mode = "wb")
# 
# # Extract Waquoit_Core_data_release.xlsx
# download.file(url_list[[2]], "./data/Gonneea_2018/original/Waquoit_Core_data_release.xlsx",
#               mode = "wb")
# 
# # Extract Waquoit_Core_data_release.csv
# download.file(url_list[[3]], "./data/Gonneea_2018/original/Waquoit_Core_data_release.csv",
#               mode = "wb")
# 
# # Extract Waquoit_Core_data_release_meta.xml
# download.file(url_list[[4]], "./data/Gonneea_2018/original/Waquoit_Core_data_release_meta.xml",
#               mode = "wb")


## Curate data to CCRCN Structure ########################

# Import data file into R
Gonneea_2018 <- read_csv("./data/primary_studies/Gonneea_2018/original/Waquoit_Core_data_release.csv", 
                         col_names = TRUE,
                         na = c("", "NA", "-99999"))

# Change column names to values of first row
# Why? Because the top 2 rows were both dedicated to column headers
new_colnames <- c(Gonneea_2018 %>% slice(1))
colnames(Gonneea_2018) <- new_colnames
Gonneea_2018 <- Gonneea_2018 %>% slice(2:561)

# Curate data: 
Gonneea_2018_clean <- Gonneea_2018 %>%
  # na_if(-99999) %>% # Changes all "-99999" values to "NA"
  rename(core_id = "ID",
         core_date = "Date", 
         depth = "Depth",
         core_latitude = "Lat",
         core_longitude = "Lon",
         dry_bulk_density = "DBD",
         age = "Age",
         total_pb210_activity = "210Pb",
         ra226_activity = "226Ra",
         excess_pb210_activity = "210Pbex",
         cs137_activity = "137Cs",
         be7_activity = "7Be", 
         total_pb210_activity_sd = `210Pb_e`, 
         ra226_activity_sd = `226Ra_e`,
         excess_pb210_activity_sd = `210Pbex_e`,
         cs137_activity_sd = `137Cs_e`, 
         be7_activity_sd = `7Be_e`,
         age_sd = Age_e) %>%
  mutate(study_id = "Gonneea_et_al_2018") %>%
  mutate(core_latitude = as.numeric(core_latitude)) %>%
  mutate(core_longitude = as.numeric(core_longitude)) %>%
  mutate(dry_bulk_density = as.numeric(dry_bulk_density)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(total_pb210_activity = as.numeric(total_pb210_activity)) %>%
  mutate(ra226_activity = as.numeric(ra226_activity)) %>%
  mutate(excess_pb210_activity = as.numeric(excess_pb210_activity)) %>%
  mutate(cs137_activity = as.numeric(cs137_activity)) %>%
  mutate(be7_activity = as.numeric(be7_activity)) %>%
  # Change core_date column to date objects
  mutate(core_date = as.Date(as.numeric(core_date), origin = "1899-12-30"), 
         depth = as.numeric(depth))

# according to the publication, the first 30 cm are 1 cm intervals, 
# the proceeding interals are at 2 cm 
# Convert mean interval depth to min and max interval depth
Gonneea_2018_final <- Gonneea_2018_clean %>%
  mutate(depth_min = ifelse(depth < 30, depth - .5, 
                            ifelse(depth < 100, depth - 1, depth - 5)), 
         depth_max = ifelse(depth < 30, depth + .5, 
                            ifelse(depth < 100, depth + 1, depth + 5))) %>% 

# Provide units and notes for dating techniques 
# Gonneea_2018 <- Gonneea_2018 %>%
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity)|!is.na(excess_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA),
         be7_unit = ifelse(!is.na(be7_activity), "disintegrationsPerMinutePerGram", NA),
         ra226_unit = ifelse(!is.na(ra226_activity), "disintegrationsPerMinutePerGram", NA)) %>%
  # if 0, below detection limits
  mutate(dating_interval_notes = ifelse(cs137_activity == 0 & be7_activity == 0, "cs137 and be7 activity below detection limits",
                                        ifelse(cs137_activity == 0, "cs137 activity below detection limits", 
                                               ifelse(be7_activity == 0, "be7 activity below detection limits", NA)))) %>% 

# Convert percent weights to fractions
# Gonneea_2018 <- Gonneea_2018 %>%
  mutate(fraction_carbon = as.numeric(wtC) / 100)
  
## Parcel data into separate files according to data level #################
# Core data

# Gonneea elevation is calculated for each depth interval. We only want elevation
#   at the top of the core
core_elevation <- Gonneea_2018_final %>%
  group_by(core_id) %>%
  summarize(core_elevation = max(as.numeric(Elevation)))
  
Gonneea_2018_core_Data <- Gonneea_2018_final %>%
  group_by(study_id, core_id, core_date) %>%
  summarize(core_latitude = first(core_latitude), core_longitude = first(core_longitude)) %>%
  left_join(core_elevation) %>%
  # convert elevation from cm to meters
  mutate(core_elevation = core_elevation * .01,
         core_elevation_method = "RTK",
         core_elevation_datum = "NAVD88") %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>%
  select(-core_date) %>%
  ungroup()

# Depth Series data
Gonneea_2018_depth_series_data <- Gonneea_2018_final %>%
  select(study_id, core_id, depth_min, depth_max, 
         dry_bulk_density, fraction_carbon, 
         cs137_activity, cs137_activity_sd, cs137_unit, 
         total_pb210_activity, total_pb210_activity_sd, pb210_unit, 
         ra226_activity, ra226_activity_sd, ra226_unit,
         excess_pb210_activity, excess_pb210_activity_sd,  
         be7_activity, be7_activity_sd, be7_unit, 
         age, age_sd, dating_interval_notes) %>%
  filter(depth_min >= 0)


## Add site data ################
# The data is missing site IDs but we have records of them from the Holmquist et al. 2018 data release. 

cores <- Gonneea_2018_core_Data %>%
  mutate(site_id = recode_factor(core_id, 
                                 "EPA" = "Eel_Pond", 
                                 "EPB" = "Eel_Pond",
                                 "GPA" = "Great_Pond", 
                                 "GPB" = "Great_Pond", 
                                 "GPC" = "Great_Pond", 
                                 "HBA" = "Hamblin_Pond",
                                 "HBB" = "Hamblin_Pond",
                                 "HBC" = "Hamblin_Pond", 
                                 "SLPA" = "Sage_Log_Pond", 
                                 "SLPB" = "Sage_Log_Pond",
                                 "SLPC" = "Sage_Log_Pond"
  )) %>%
  mutate(core_length_flag = ifelse(site_id != "Hamblin_Pond", "core depth represents deposit depth", NA),
         inundation_class = ifelse(core_id == "SLPB", "high", "low"),
         inundation_method = "measurement",
         core_position_method = "handheld")
# core diameter: 11cm

depthseries <- Gonneea_2018_depth_series_data %>%
  mutate(site_id = recode_factor(core_id, 
                                 "EPA" = "Eel_Pond", 
                                 "EPB" = "Eel_Pond",
                                 "GPA" = "Great_Pond", 
                                 "GPB" = "Great_Pond", 
                                 "GPC" = "Great_Pond", 
                                 "HBA" = "Hamblin_Pond",
                                 "HBB" = "Hamblin_Pond",
                                 "HBC" = "Hamblin_Pond", 
                                 "SLPA" = "Sage_Log_Pond", 
                                 "SLPB" = "Sage_Log_Pond",
                                 "SLPC" = "Sage_Log_Pond"),
         method_id = "single set of methods") %>%
  select(study_id, site_id, core_id, method_id, everything())

# cores <- reorderColumns("cores", Gonneea_2018_core_Data) %>% ungroup()
# depthseries <- reorderColumns("depthseries", Gonneea_2018_depth_series_data) %>% ungroup()

## ... Methods ####
# Jaxine edits

raw_methods <- read_xlsx("data/primary_studies/Gonneea_2018/intermediate/gonneea_2018_methods.xlsx", sheet = 2)

# curate materials and methods
methods <- raw_methods %>%
  # drop_na(study_id) %>% 
  select_if(function(x) {!all(is.na(x))})

## Create study-level data ######

if(!file.exists("data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_study_citations.csv")) {
  doi <- "10.5066/F7H41QPP"
  study <- "Gonneea_et_al_2018"
  
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI(doi)
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Gonneea_et_al_2018_data", 
           study_id = study,
           publication_type = "primary dataset") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018.bib")
  write_csv(study_citations, "./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("cores", "depthseries") # methods are already in the correct format

updated <- updateTables(table_names)

# save listed tables to objects

# methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores

## QA/QC of data ################

# Check col and varnames
testTableCols(c(table_names, "methods"))
testTableVars(c(table_names, "methods"))
testRequired(c(table_names, "methods"))
testConditional(c(table_names, "methods"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)
(results)

## Export files ##############################
  
write_csv(methods, "./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_methods.csv")
write_csv(cores, "./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_cores.csv")
write_csv(depthseries, "./data/primary_studies/Gonneea_2018/derivative/Gonneea_et_al_2018_depthseries.csv")
  
# Export master data
# write_csv(Gonneea_2018, "./data/Gonneea_2018/derivative/Gonneea_2018.csv")


