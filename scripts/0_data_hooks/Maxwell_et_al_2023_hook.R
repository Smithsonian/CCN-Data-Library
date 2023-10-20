## CCRNC Data Release Formatting ########

## Soil core data curation script for <insert dataset name>
## contact: Your Name, your email

## Notes about the dataset 
## Link to associated publication(s) for easy access

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in  original data by inserting the path to the dataset

dat_raw <- read_csv("data/primary_studies/Maxwell_et_al_2023/original/Maxwell_marshC_dataset.csv")
metadat <- read_csv("data/primary_studies/Maxwell_et_al_2023/original/Maxwell_marshC_dataset_metadata.csv")

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# attributes
# [1] "Source"             "Original_source"    "Data_type"          "Site"               "Core"              
# [6] "Plot"               "Site_name"          "Soil_type"          "Latitude"           "Longitude"         
# [11] "accuracy_flag"      "Country"            "Admin_unit"         "Year_collected"     "Year_collected_end"
# [16] "U_depth_m"          "L_depth_m"          "Method"             "Conv_factor"        "OC_perc"           
# [21] "BD_g_cm3"           "SOM_perc"           "N_perc"             "Time_replicate"     "Treatment"         
# [26] "n_cores"            "SOM_perc_mean"      "SOM_perc_sd"        "OC_perc_mean"       "OC_perc_sd"        
# [31] "OC_perc_se"         "BD_g_cm3_mean"      "BD_g_cm3_sd"        "BD_g_cm3_se"        "OC_from_SOM_our_eq"
# [36] "OC_obs_est"         "OC_perc_final"      "Notes"

# FXN for overlapping columns and coalescing
# unique(is.na(dat_raw %>% select(OC_obs_est, Notes)))

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
# id <- "Author_et_al_year"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

dat_raw %>% 
  drop_na(SOM_perc, OC_perc) %>% 
  ggplot(aes(SOM_perc, OC_perc, col = Method)) + 
  geom_point(pch = 1) +
  facet_wrap(~Method)

# dat_raw %>% mutate(id = paste(Original_source, Site, Site_name, Core, Plot))

# drop studies that we've already synthesized
to_remove <- c("Adame_et_al_2013", "Adame_et_al_2015", "Burden_et_al_2018")
# some of these original studies have radioisotope dating that wasn't synthesized for this meta analysis 

# curate core depthseries data
dat <- dat_raw %>% 
  rename(dry_bulk_density = BD_g_cm3,
         depth_min = U_depth_m, 
         depth_max = L_depth_m,
         method_id = Method,
         position_notes = accuracy_flag) %>% 
  mutate(OC_perc = ifelse(is.na(Conv_factor), OC_perc, NA),
         fraction_organic_matter = SOM_perc/100,
         fraction_carbon = OC_perc/100,
         Notes = coalesce(OC_obs_est, Notes),
         core_id = coalesce(Core, Plot),
         study_id = gsub(" ", "_", Original_source),
         dropcol = ifelse(is.na(SOM_perc) & is.na(OC_perc) & is.na(dry_bulk_density), TRUE, FALSE)) %>% 
  # add_count(Core) %>%
  select(-OC_obs_est) %>% 
  select(study_id, Site_name, core_id, depth_min, depth_max, everything()) %>% 
  arrange(study_id, Site_name, core_id, depth_min)

## Sources

# compare to our data library bib
sources <- dat_raw %>% 
  distinct(Source, Original_source) %>% 
  rename(study_id = Original_source) %>% 
  mutate(synthesis_source = ifelse(Source != study_id, gsub(" ", "_", Source), NA),
         study_id = gsub(" ", "_", study_id)) %>% 
  arrange(study_id) %>% 
  select(-Source)

# write_csv(sources, "data_releases/maxwell_et_al_2023/maxwell_marsh_synthesis_sources.csv")


## ... Methods ####

unique(dat_raw$Method)

# [1] "combined LOI EA"                      "EA"                                  
# [3] "LOI"                                  "Wilson (1973)"                       
# [5] "SOM by Suguio (1973)"                 "Tyurin (1951)"                       
# [7] NA                                     "MIR predicted"                       
# [9] "CF-IRMS"                              "Walkley Black"                       
# [11] "oxidation with potassium dichromate"  "MIR absorbance spectra"              
# [13] "Walkley-Black wet oxidation"          "Tyurin spectrophotometry"            
# [15] "Walkley–Black wet combustion"         "wet oxidation redox titration method"

unique(dat_raw$Conv_factor)
unique(dat_raw$accuracy_flag)

methods <- dat %>% 
  distinct(study_id, method_id, Conv_factor, Country) %>% 
  add_count(study_id)

# organic carbon

## ... Sites ####

# Site-level observations, we'll have to cut these out
# but maybe we can track down the original data later
site_level <- dat_raw %>% 
  filter(Data_type == "Site-level") %>% 
  select_if(~!all(is.na(.)))

# curate site-level data
# sites <- orig_sites

# reorder the columns based on the database guidance
# sites <- reorderColumns("site-level", sites)

## ... Cores ####

# curate core-level data
cores <- dat %>%
  rename(latitude = Latitude,
         longitude = Longitude) %>% 
  select(-c(OC_perc, SOM_perc, N_perc, SOM_perc_mean, depth_min, depth_max, dry_bulk_density,
            fraction_organic_matter, fraction_carbon,
            SOM_perc_sd, OC_perc_mean, OC_perc_sd, OC_perc_se, BD_g_cm3_mean, BD_g_cm3_sd,
            BD_g_cm3_se, OC_perc_final, OC_from_SOM_our_eq)) %>% 
  distinct()

# reorder the columns based on the database guidance
# cores <- reorderColumns("core-level", cores)

## ... Depthseries ####

depthseries <- dat %>% 
  filter(is.na(n_cores)) %>% 
  filter(dropcol == FALSE) %>% 
  select_if(~!all(is.na(.)))
  # select(-c(OC_perc, SOM_perc, N_perc, Latitude, Longitude, Year_collected, SOM_perc_mean, 
  #           SOM_perc_sd, OC_perc_mean, OC_perc_sd, OC_perc_se, BD_g_cm3_mean, BD_g_cm3_sd,
  #           BD_g_cm3_se, OC_perc_final, OC_from_SOM_our_eq))

## ... Impacts ####

impacts <- dat %>% 
  drop_na(Treatment) %>% 
  distinct(study_id, Site_name, core_id, Treatment)

## 2. QAQC ####

## ... Map Core Locations ####
library(leaflet)

leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 2, label = ~Source)

## ... Standard QA Tests ####
table_names <- c("methods", "cores", "depthseries", "impacts") # add other tables if present

# test alignment to database structure
# if there are uncontrolled attributes or variables
# create tables to define them
testTableCols(table_names) 
testTableVars(table_names)

# test uniqueness
test_unique_cores(cores)
test_unique_coords(cores)

# test relational databases
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numerical values and ranges
# this function requires library(skimr)
test_depth <- test_numeric_vars(depthseries, study_uncontrolled = NULL)
test_cores <- test_numeric_vars(cores, study_uncontrolled = NULL)
test_methods <- test_numeric_vars(methods, study_uncontrolled = NULL)
fraction_not_percent(depthseries)

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_methods.csv")
write_csv(sites, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_cores.csv")
write_csv(depthseries, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_depthseries.csv")
write_csv(species, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_species.csv")
write_csv(impacts, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####
library(RefManageR)

# There are three ways to approach this:
# 1) download the article citation directly to the data release folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in the metadata folder, read it in and output bib file to data release folder

# example study citation creation:
# study_citation <- data.frame(bibliography_id = "Spera_et_al_2020",
#                              title = "Spatial and temporal changes to a hydrologically-reconnected coastal wetland: Implications for restoration",
#                              author = "Alina C. Spera and John R. White and Ron Corstanje",
#                              bibtype = "Article",
#                              doi = "10.1016/j.ecss.2020.106728",
#                              url = "https://doi.org/10.1016/j.ecss.2020.106728", 
#                              journal = "Estuarine, Coastal and Shelf Science",
#                              year = "2020") %>% 
#     column_to_rownames("bibliography_id")
# 
# WriteBib(as.BibEntry(study_citation), "data_releases/path_to_data_release_folder/Author_et_al_YYYY_associated_publications.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
