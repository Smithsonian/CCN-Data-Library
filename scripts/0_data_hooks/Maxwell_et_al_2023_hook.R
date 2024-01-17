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

## 1. Curation ####

# Site-level observations, we'll have to cut these out
# but maybe we can track down the original data later
site_level <- dat_raw %>% 
  filter(Data_type == "Site-level") %>% 
  select_if(~!all(is.na(.)))

# Some cores from Miller 2022 and Smeaton 2022 are duplicates
duplicate_cores <- c("SF-18-01","SF-18-02","SF-18-03","SF-18-04","SF-18-05","SF-18-06","SF-18-07","SF-18-08","SF-18-09","SF-18-10","SF-18-11","SF-18-12","SF-18-13",
                     "SF-18-14","SF-18-15","SF-18-16","SF-18-17","SF-18-18","SF-18-19","SF-18-20","SF-18-21","SF-18-22","SF-18-23","SF-18-24","SF-18-25","SF-18-26",
                     "SF-18-27","SF-18-28","SF-18-29","SF-18-30","SF-18-31","SF-18-32","SF-18-33","Wig 1","Wig 10","Wig 2","Wig 3","Wig 4","Wig 5",
                     "Wig 6","Wig 7","Wig 8","Wig 9")

# curate table that will become the core and depthseries tables
dat <- dat_raw %>% 
  rename(dry_bulk_density = BD_g_cm3,
         depth_min = U_depth_m, 
         depth_max = L_depth_m,
         depth_interval_notes = Soil_type,
         method_id = Method,
         position_notes = accuracy_flag,
         year = Year_collected,
         latitude = Latitude,
         longitude = Longitude,
         impact_class = Treatment) %>% 
  # remove site-level soils data
  filter(Data_type != "Site-level") %>% 

  # remove observations that contain averages from multiple cores
  filter(is.na(n_cores)) %>% 
  
  mutate(site_id = coalesce(Site, Site_name),
         OC_perc = ifelse(is.na(Conv_factor), OC_perc, NA),
         fraction_organic_matter = SOM_perc/100,
         fraction_carbon = OC_perc/100,
         Notes = coalesce(OC_obs_est, Notes),
         study_id = gsub(" ", "_", Original_source),
         study_id = recode(study_id, 
                           "Kauffman_et_al_2020" = "Kauffman_et_al_2020_Brazil",
                           "Serrano_unpublished" = "Serrano_unpublished_Australia",
                           "UNPUBLISHED" = "Copertino_unpublished"),
         depth_min = ifelse(study_id != "Russell_et_al_submitted", depth_min*100, depth_min), 
         depth_max = ifelse(study_id != "Russell_et_al_submitted", depth_max*100, depth_max)) %>% 

  # fix the core ID
  mutate(core_id = coalesce(Core, Plot),
         core_id_num = as.numeric(core_id), # this will coerce some things to NA
         core_id = case_when(!is.na(core_id_num) ~ Site_name,
                             study_id %in% c("Beasy_and_Ellison_2013", "Conrad_et_al_2019") ~ Site_name,
                             grepl("Burgh", core_id) ~ paste(core_id, word(Source, 4, 4)),
                             is.na(core_id) ~ site_id,
                             site_id %in% c("Nadia's Landing", "Cape Missiessy") ~ str_sub(core_id, end = -3), 
                             grepl("FL-N", core_id) ~ "FL-N",
                             grepl("NF-N", core_id) ~ "NF-N",
                             core_id == "Salmi allpool vähe orgaanikat" ~ "Salmi managed",
                             T ~ core_id)) %>%
  # remove duplicate cores
  filter(!(study_id == "Miller_et_al_2022" & core_id %in% duplicate_cores)) %>% 
  # one of the coords is wrong
  mutate(latitude = ifelse(core_id == "RMN97_NB", 41.84336, latitude),
         longitude = ifelse(core_id == "RMN97_NB", -69.95326, longitude)) %>% 
  select(-c(OC_obs_est, N_perc, SOM_perc, OC_perc)) %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, fraction_carbon,
         everything()) %>% 
  arrange(study_id, site_id, core_id, depth_min) %>% 
  # remove duplicate studies
  filter(!(study_id %in% c("Ward_et_al_2021", "Burden_et_al_2018"))) %>%
  # drop rows where there are no original measurements of DBD, LOI, or OC
  mutate(droprow = ifelse(is.na(fraction_organic_matter) & is.na(fraction_carbon) & is.na(dry_bulk_density), T, F)) %>%
  filter(droprow == F) %>%
  select_if(~!all(is.na(.)))

## ... Cores ####

# curate core-level data
cores <- dat %>%
  distinct(study_id, site_id, core_id, latitude, longitude, year, position_notes) %>% 
  mutate(vegetation_class = "emergent",
         habitat = "marsh",
         position_method = ifelse(grepl("estimated|averaged", position_notes), "other low resolution", NA))
  # add_count(core_id) %>% filter(n > 1)
# unique(cores$position_notes)

# reorder the columns based on the database guidance
# cores <- reorderColumns("core-level", cores)

## ... Depthseries ####

depthseries <- dat %>% 
  add_count(core_id) %>% mutate(one_interval = ifelse(n == 1, T, F)) %>%
  # filter(one_interval == T)
  rename(organic_carbon_model = Conv_factor) %>% 
  mutate(quality_flag = ifelse(grepl("Outlier", Notes), Notes, NA)) %>% 
  select(contains("_id"), everything()) %>% 
  select(-c(Source, Original_source, position_notes, Country, Admin_unit, Core, Plot, Data_type,
            latitude, longitude, Site, Site_name, year, Time_replicate, impact_class, core_id_num,
            contains("_sd"), OC_perc_final, n, one_interval, droprow, OC_from_SOM_our_eq, Notes))

# View(depthseries %>% filter(is.na(fraction_organic_matter) & is.na(dry_bulk_density) & is.na(fraction_carbon)))


depthseries %>% 
  drop_na(fraction_organic_matter, fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = quality_flag)) + 
  geom_point(pch = 1)
# there's OC outliers => leave in or out?

depthseries %>% 
  drop_na(dry_bulk_density, fraction_organic_matter) %>% 
  ggplot(aes(dry_bulk_density, fraction_organic_matter, col = quality_flag)) + 
  geom_point(pch = 1) +
  facet_wrap(~quality_flag)

depthseries %>% 
  drop_na(dry_bulk_density, fraction_carbon) %>% 
  # filter(fraction_carbon < 1) %>% 
  ggplot(aes(dry_bulk_density, fraction_carbon, col = quality_flag)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~quality_flag) +
  theme(legend.position = "bottom")
# there an OC outliers => leave in or out?

# depthseries %>% 
#   drop_na(fraction_carbon, OC_from_SOM_our_eq) %>% 
#   ggplot(aes(fraction_carbon, OC_from_SOM_our_eq)) + 
#   geom_point(pch = 1)

## ... Methods ####

unique(dat$method_id)
# [1] "Wilson (1973)"                       "EA"                                  NA                                   
# [4] "LOI"                                 "Tyurin (1951)"                       "MIR predicted"                      
# [7] "SOM by Suguio (1973)"                "oxidation with potassium dichromate" "Tyurin spectrophotometry" 

methods <- dat %>% 
  distinct(study_id, method_id) %>% 
  rename(fraction_carbon_method = method_id) %>% 
  arrange(study_id) %>% add_count(study_id)

# Martins_et_al_2022 fraction carbon method is EA (there are some intervals that only have LOI)
# check out de_los_Santos_et_al_2022 more (part a and b)
# organic carbon?

## ... Impacts ####

impacts <- dat %>% 
  drop_na(impact_class) %>% 
  mutate(impact_class = tolower(impact_class)) %>% 
  distinct(study_id, site_id, core_id, impact_class)

unique(impacts$impact_class)

## ...Check Duplicates

ccn_bib <- read_csv("data/CCN_synthesis/CCN_study_citations.csv")

maxwell_studies <- dat %>% distinct(Original_source, Country, Admin_unit) %>% 
  rename(country = Country) %>% 
  mutate(country = recode(country, 
                          "UK" = "United Kingdom",
                          "SouthAfrica" = "South Africa"),
         first_name = word(Original_source))

# read in ccn cores to make sure there's no duplicates
ccn_cores <- read_csv("data/CCN_synthesis/CCN_cores.csv", guess_max = 10000) 
ccn_studies <- ccn_cores %>% 
  distinct(study_id, country, admin_division, habitat) %>% 
  filter(country %in% unique(maxwell_studies$country)) %>% 
  mutate(study_id = gsub("_", " ", study_id),
         first_name = word(study_id))

find_dups <- left_join(maxwell_studies, ccn_studies, multiple = "all") %>% drop_na(study_id)

# "Adame_et_al_2013", "Adame_et_al_2015", "Yando_et_al_2016", are all in the Atlas, but only the mangrove data
# this synthesis supplies the marsh data from these studies
# Copertino_unpublished is the same but for seagrass

## 2. QAQC ####

## ... Map Core Locations ####
library(leaflet)

dat %>%
  distinct(study_id, site_id, core_id, latitude, longitude, Country) %>% 
# cores %>%
  filter(study_id == "Roman_et_al_1997") %>% 
  # filter(Country == "China") %>% filter(study_id != "Xia_et_al_2022") %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 2, label = ~site_id)

# ccn_cores %>%
#   filter(study_id  == "Copertino_unpublished") %>%
#   leaflet() %>%
#   addTiles() %>% 
#   addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
#                    radius = 2, label = ~study_id)

## ... Standard QA Tests ####
table_names <- c("methods", "cores", "depthseries", "impacts") # add other tables if present

# test alignment to database structure
# if there are uncontrolled attributes or variables
# create tables to define them
testTableCols(table_names) 
testTableVars(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

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
# write_csv(sites, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_cores.csv")
write_csv(depthseries, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_depthseries.csv")
# write_csv(species, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_species.csv")
# write_csv(impacts, "data_releases/path_to_data_release_folder/Author_et_al_YYYY_impacts.csv")

## 4. Bibliography ####
library(RefManageR)

# read in bib file
bib <- as.data.frame(ReadBib("data/primary_studies/Maxwell_et_al_2023/original/core-level.bib")) %>% 
  rownames_to_column("bib_key")

# compare to our data library bib
sources <- dat %>% distinct(study_id, Source) 
write_csv(sources, "data/primary_studies/Maxwell_et_al_2023/intermediate/maxwell_marsh_synthesis_sources.csv")

maxwell_synth <- as.data.frame(GetBibEntryWithDOI("10.1038/s41597-023-02633-x"))




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
