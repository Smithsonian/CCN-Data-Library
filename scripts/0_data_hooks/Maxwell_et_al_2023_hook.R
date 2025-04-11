## CCRNC Data Release Formatting ########

## Soil core data curation script for Maxwell et al 2023
## contact: Jaxine Wolfe; wolfejax@si.edu

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

# read in some original data that I revised in Tania's script
# some cols were dropped that would be relevant for the CCN database

# Human et al 2022 is included in the Anesu and Machite hook
# dat_human <- read_csv("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/Human et al 2022.csv") %>% 
#   rename(BD_g_cm3 = BD_reported_g_cm3, Original_source = Source) %>%
#   mutate(Data_type = "Core-level")

dat_ford <- read_csv("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/Ford et al 2016.csv") %>% 
  mutate(Data_type = "Core-level") %>% rename(Original_source = Source) %>% 
  select(-BD_reported_g_cm3)
  
dat_miller <- read_csv("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/Miller et al 2022.csv") %>%
  rename(BD_g_cm3 = BD_reported_g_cm3, Original_source = Source) %>%
  mutate(Data_type = "Core-level")

dat_kumar <- read_csv("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/Kumar et al 2020.csv") %>%
  rename(Original_source = Source) %>%
  mutate(Data_type = "Core-level")

dat_knysna <- read_csv("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported/Raw et al 2020.csv") %>%
  rename(BD_g_cm3 = BD_reported_g_cm3, Original_source = Source) %>%
  mutate(Data_type = "Core-level")

# nrow(dat_raw %>% filter(Source == "Miller et al 2022"))

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

## 1. Curation ####

# Site-level observations, we'll have to cut these out since they're averaged
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

dat_revised <- dat_raw %>% 
  filter(!(Original_source %in% c("Human et al 2022", "Ford et al 2016", "Miller et al 2022", "Kumar et al 2020", 
                                  "Raw et al 2020"))) %>% 
  # bind_rows(dat_human) %>% # included in a different hook
  bind_rows(dat_ford) %>% 
  bind_rows(dat_miller) %>% 
  bind_rows(dat_kumar) %>% 
  bind_rows(dat_knysna) %>% 
  # remove duplicate studies
  filter(!(Source %in% c("Ward et al 2021", "Burden et al 2018", "Schile et al 2016"))) 
                         # removing studies that were recently included more comprehensively by the PNW database
                         # "Adame et al 2015", "Adame et al 2021")))

# 16432 rows currently 
dat <- dat_revised %>% 
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
                           "Serrano_unpublished" = "Serrano_AUS_unpublished",
                           "Azevedo_2015-UNPUBLISHED" = "Azevedo_2015_unpublished",
                           "UNPUBLISHED" = "Copertino_unpublished",
                           "Miller_et_al_2022" = "Miller_et_al_2022_Scotland",
                           # "Yando_et_al_2016" = "Yando_et_al_2016_marsh",
                           "Smeaton_unpublished_Essex" = "Smeaton_et_al_2023",
                           "Russell_et_al_submitted" = "Russell_et_al_2023",
                           "Mazarrasa_et_al_in_prep" = "Mazarrasa_et_al_2023",
                           "de_los_Santos_et_al_2022_(a)" = "de_los_Santos_et_al_2022",
                           "de_los_Santos_et_al_2022_(b)" = "de_los_Santos_et_al_2023",
                           "Pagès_et_al_(in_preparation)" = "Pagès_et_al_unpublished",
                           "Neto_&_Lana_1997" = "Neto_and_Lana_1997"),
         depth_min = case_when(study_id == "Russell_et_al_2023" ~ depth_min, T ~ depth_min*100), 
         depth_max = case_when(study_id == "Russell_et_al_2023" ~ depth_max, T ~ depth_max*100)) %>% 

  # create unique core IDs
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
                             !is.na(Core_type) ~ paste(Core, Core_type, sep = " "),
                             T ~ core_id),
         drop_duplicates = case_when(study_id == "Smeaton_et_al_2022a" & core_id %in% duplicate_cores ~ "drop", T ~ "keep")) %>% 
  # remove duplicate cores (going to keep all the Miller cores and drop the duplicates from Smeaton et al 2022a)
  filter(drop_duplicates == "keep") %>% 
  
  # create unique core IDs for some surface samples
  add_count(core_id, depth_min, depth_max) %>%
  group_by(core_id) %>%
  mutate(replicate_id = case_when(study_id == "de_los_Santos_et_al_2022" ~ seq_along(core_id), T ~ NA),
         core_id = case_when(study_id == "de_los_Santos_et_al_2022" & n > 1 ~ paste(core_id, replicate_id, sep = "_"),
                             T ~ core_id)) %>%
  select(-n) %>%
  ungroup() %>%
  
  # one of the coords is wrong
  mutate(latitude = ifelse(core_id == "RMN97_NB", 41.84336, latitude),
         longitude = ifelse(core_id == "RMN97_NB", -69.95326, longitude)) %>% 
  select(-c(OC_obs_est, N_perc, SOM_perc, OC_perc)) %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, fraction_carbon,
         everything()) %>% 
  arrange(study_id, site_id, core_id, depth_min) %>% 
  # drop rows where there are no original measurements of DBD, LOI, or OC
  mutate(droprow = ifelse(is.na(fraction_organic_matter) & is.na(fraction_carbon) & is.na(dry_bulk_density), T, F)) %>%
  filter(droprow == F) %>%
  select_if(~!all(is.na(.)))

## ... Cores ####

# curate core-level data
cores <- dat %>%
  distinct(study_id, site_id, core_id, latitude, longitude, year, position_notes, Core_type, Marsh_type, Marsh_zone, Habitat_type, impact_class) %>%  # month, day?
  mutate(habitat = case_when(grepl("marsh", Habitat_type) ~ "marsh",
                             grepl("Mud", site_id) | Habitat_type == "Mudflat" ~ "mudflat",
                             T ~ "marsh"),
         vegetation_class = case_when(habitat == "marsh" ~ "emergent", T ~ NA),
         inundation_class = case_when(Marsh_zone == "High" ~ "high",
                                      Marsh_zone %in% c("Low-Mid", "Low_Mid") ~ "mid",
                                      T ~ NA),
         inundation_notes = Marsh_type,
         salinity_class = case_when(Marsh_type == "Estuarine" ~ "estuarine"),
         position_method = ifelse(grepl("estimated|averaged", position_notes), "other low resolution", NA),
         core_notes = case_when(Core_type == "Narrow" ~ "30mm core diameter",
                                Core_type == "Wide" ~ "60mm core diameter", 
                                study_id == "Wollenberg_et_al_2018" ~ "managed realignment",
                                T ~ tolower(impact_class))) %>% 
    select(-c(Core_type, Marsh_type, Marsh_zone, Habitat_type, impact_class))
  # add_count(core_id) %>% filter(n > 1)
# unique(cores$position_notes)
core_count <- cores %>% count(study_id)

# reorder the columns based on the database guidance
# cores <- reorderColumns("core-level", cores)

## ... Depthseries ####

# santos_2022 <- dat %>% 
#   # filter(study_id == "de_los_Santos_et_al_2022") %>% 
#   add_count(core_id, depth_min, depth_max) %>% 
#   # filter(n > 1) %>% 
#   group_by(core_id) %>% 
#   mutate(replicate_id = seq_along(core_id))

# 31 studies with no DBD (but OM or OC is present)
# tricky for stock calculations later, but I suppose we include these for now
no_dbd <- dat %>% group_by(study_id) %>% summarize(dbd_sum = sum(dry_bulk_density, na.rm = T)) %>% 
  filter(dbd_sum == 0) %>% 
  distinct(study_id) %>% pull(study_id)

depthseries <- dat %>% 
  # add_count(core_id) %>% mutate(one_interval = ifelse(n == 1, T, F)) %>%
  # filter(one_interval == T)
  # rename(organic_carbon_model = Conv_factor) %>% 
  mutate(depth_interval_notes = case_when(grepl("Outlier", Notes) & is.na(depth_interval_notes) ~ Notes,
                                          grepl("Outlier", Notes) & !is.na(depth_interval_notes) ~ paste0(depth_interval_notes, "; ", Notes),
                                          study_id == "Ford_et_al_2016" ~ "bulk density measured but not provided in original dataset",
                                          study_id == "Kumar_et_al_2020" & Carbonate_removed == "no"  ~ "carbonates not removed",
                                          study_id == "Kumar_et_al_2020" & Carbonate_removed == "yes"  ~ "carbonates removed",
                                          study_id == "Sammul_et_al_2012" ~ paste0(depth_interval_notes, "; carbon content determined via oxidation with potassium dichromate"),
                                          impact_class == "Post-fires, input of black carbon" ~ impact_class,
                                  T ~ depth_interval_notes),
         # spot correction for Kauffman depths
         depth_min = case_when(study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 15 ~ 0,
                               study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 30 ~ 15,
                               study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 50 ~ 30,
                               study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 100 ~ 50,
                               study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 300 ~ 100,
                               T ~ depth_min),
         method_id = "single set of methods") %>% 
  select(contains("_id"), everything()) %>% 
  select(-c(Source, Original_source, position_notes, Country, Admin_unit, Core, Plot, Data_type, Conv_factor,
            latitude, longitude, Site, Site_name, year, Time_replicate, impact_class, core_id_num, replicate_id,
            contains("_sd"), OC_perc_final, droprow, OC_from_SOM_our_eq, Notes, Species, Subsite, Season, 
            Core_type, Marsh_type, Marsh_zone, Habitat_type, DOI, accuracy_code, drop_duplicates, State, Carbonate_removed))
  # filter(study_id %in% no_dbd)

dup_interval <- depthseries %>% 
  add_count(study_id, site_id, core_id, depth_min, depth_max) %>% filter(n > 1)

surface_samples <- depthseries %>% 
  add_count(core_id) %>% 
  # mutate(one_interval = ifelse(n == 1, T, F)) %>%
  filter(n == 1) %>% filter(depth_max < 50)

# studies with cores that have some cases of multiple observations per depth increment
#  "Kumar_et_al_2020" "Smeaton_et_al_2022b" "Smeaton_et_al_2023" "Van_de_Broek_et_al_2018"

dat %>% 
  drop_na(fraction_organic_matter, fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = Notes)) + 
  geom_point(pch = 1)
# there's OC outliers => leave in or out?

dat %>% 
  drop_na(dry_bulk_density, fraction_organic_matter) %>% 
  ggplot(aes(dry_bulk_density, fraction_organic_matter, col = Notes)) + 
  geom_point(pch = 1) +
  facet_wrap(~Notes)

dat %>% 
  drop_na(dry_bulk_density, fraction_carbon) %>% 
  # filter(fraction_carbon < 1) %>% 
  ggplot(aes(dry_bulk_density, fraction_carbon, col = Notes)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~Notes) +
  theme(legend.position = "bottom")
# there an OC outliers => leave in or out?

# depthseries %>% 
#   drop_na(fraction_carbon, OC_from_SOM_our_eq) %>% 
#   ggplot(aes(fraction_carbon, OC_from_SOM_our_eq)) + 
#   geom_point(pch = 1)

## ... Methods ####

unique(dat$method_id)
# "Wilson (1973)" "EA" "LOI"  "MIR predicted" "combined LOI EA"            
# "oxidation with potassium dichromate" "Tyurin spectrophotometry" 

methods <- dat %>% 
  distinct(study_id, method_id, Conv_factor) %>% 
  rename(fraction_carbon_notes = method_id,
         carbon_profile_notes = Conv_factor) %>% 
  filter(!(study_id == "Kohfeld_et_al_2022" & is.na(carbon_profile_notes))) %>% 
  mutate(method_id = "single set of methods",
         fraction_carbon_method = case_when(fraction_carbon_notes == "EA" ~ "measured",
                                            fraction_carbon_notes == "LOI" ~ "modeled", 
                                            grepl("oxidation", fraction_carbon_notes) ~ "wet oxidation",
                                            T ~ fraction_carbon_notes)) %>% 
  
  select(study_id, method_id, everything()) %>% 
  arrange(study_id) 

# Martins_et_al_2022 fraction carbon method is EA (there are some intervals that only have LOI)
# check out de_los_Santos_et_al_2022 more (part a and b)
# organic carbon?
 
## ... Impacts ####

impacts <- dat %>% 
  mutate(impact_class = case_when(grepl("impounded", core_id) ~ "impounded",
                                  grepl("managed", core_id) ~ "managed",
                                  grepl("restored", core_id) ~ "restored",
                                  impact_class %in% c("Historic-Breach", "Managed Realignment") ~ "tidally restored",
                                  study_id == "Wollenberg_et_al_2018" ~ "tidally restored",
                                  T ~ tolower(impact_class))) %>%
  filter(grepl("managed|restored|impounded", core_id) | !is.na(impact_class)) %>% 
  filter(!(study_id %in% c("Gallagher_et_al_2021", "Graversen_et_al_2022"))) %>% 
  # drop_na(impact_class) %>% 
  # mutate(impact_class = tolower(impact_class)) %>% 
  distinct(study_id, site_id, core_id, impact_class)

unique(impacts$impact_class)


## ... Species ####

species <- dat %>% 
  drop_na(Species) %>% 
  distinct(study_id, site_id, core_id, Species) %>% 
  rename(species_code = Species) %>% 
  mutate(code_type = case_when(grepl(" ", species_code) ~ "Genus species", T ~ "Genus"))


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
  # filter(study_id == "Schile_et_al_2016") %>%
  filter(study_id == "Raw_et_al_2020") %>% 
  # filter(grepl("de_los_Santos", study_id)) %>%
  # filter(Country == "China") %>% filter(study_id != "Xia_et_al_2022") %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 2, label = ~study_id)

# ccn_cores %>%
#   filter(grepl("Marisma", core_id)) %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude),
#                    radius = 2, label = ~paste(study_id, habitat, sep = "; "))

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
# test_depth <- testNumericCols(depthseries)
# test_cores <- testNumericCols(cores)
# test_methods <- testNumericCols(methods)

fractionNotPercent(depthseries)

## 3. Write Curated Data ####

# write data to final folder
# write_csv(methods, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_methods.csv")
write_excel_csv(cores, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_species.csv")
write_excel_csv(impacts, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_impacts.csv")

## 4. Bibliography ####
library(RefManageR)

# read in bib file
# bib <- as.data.frame(ReadBib("data/primary_studies/Maxwell_et_al_2023/original/core-level.bib")) %>% 
#   rownames_to_column("bib_key")
# write_excel_csv(bib, "data/primary_studies/Maxwell_et_al_2023/intermediate/maxwell_study_citations.csv")

# compare to our data library bib
# sources <- dat %>% distinct(study_id, Source)
# write_csv(sources, "data/primary_studies/Maxwell_et_al_2023/intermediate/maxwell_marsh_synthesis_sources.csv")

study_dois <- read_csv("data/primary_studies/Maxwell_et_al_2023/intermediate/maxwell_study_citations.csv") %>% 
  bind_rows(read_xlsx("data/primary_studies/Maxwell_et_al_2023/intermediate/missing_maxwell_studies.xlsx")) %>% 
  drop_na(study_id) %>% 
  filter(!(study_id %in% c("Markewich_et_al_1998", "Xia_et_al_2022", "Fu_et_al_2021", "Human_et_al_2022", "Schile_et_al_2016",
                           # remove studies that have already partially been included via the Sanderman synthesis
                           # because they share a study ID, they will share the citation once synthesized
                           "Yando_et_al_2016", "Adame_et_al_2015", "Adame_et_al_2013"))) %>% 
  select(study_id, doi)

missing_citations <- unique(dat$study_id)[!(unique(dat$study_id) %in% study_dois$study_id)]
missing_citations

# resolve the following:
# Copertino synthesis (synthesis and original pubs need citations, otherwise it'll just be the Maxwell)
# "Adaime_1978", "Azevedo_2015-UNPUBLISHED",  "Copertino_unpublished", "Lacerda_et_al_1997", "Neto_&_Lana_1997", "Newton_2017"  
# "Rios_et_al_2018", "Payne_et_al_2019", "Zanin_2003"

# solo unpublished: "Pagès_et_al_unpublished", "Serrano_unpublished_Australia" 

# Xia et al 2022 synthesis: "Gao_et_al_2016", "Liu_et_al_2017" (RESOLVED)

# Fu et al 2021 synthesis: "Wan_et_al_2017", "Wang_et_al_2017", "Loh_et_al_2018", "Lu_et_al_2019"              

synthesis_bib <- data.frame()

for (i in 1:nrow(study_dois)) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(study_dois$doi[i])) %>% 
    remove_rownames() %>% 
    mutate(study_id = study_dois$study_id[i])
  
  synthesis_bib <- bind_rows(synthesis_bib, temp_df)
}

# Markewich_et_al_1998 citation needs to be manually created
markewich_citation <- data.frame(study_id = "Markewich_et_al_1998",
                                 bibliography_id = "Markewich_et_al_1998_report",
                                 publication_type = "associated source",
                                 title = "Detailed Descriptions for Sampling, Sample Preparation and Analyses of Cores from St. Bernard Parish, Louisiana",
                                 author = "Markewich, Helaine Walsh and Britsch, Louis D. and Buell, Gary R. and Dillon, Douglas L. and Fraticelli, Carmen M. and Fries, Terry L. and McGeehin, John P. and Pracht, Jodi B. and Robbins, John A. and Wrenn, John H.",
                                 bibtype = "Misc",
                                 issn = "2331-1258",
                                 number = "98-429",
                                 doi = "10.3133/ofr98429",
                                 url = "https://pubs.er.usgs.gov/publication/ofr98429",
                                 publisher = "U.S. Geological Survey",
                                 year = "1998")
# is this a tech report or other bib type?


# create an expanded table which assigns Maxwell 2023 as a synthesis source for all the studies
# paper publication
maxwell_synth <- data.frame(study_id = unique(dat$study_id)) %>% 
  bind_cols(as.data.frame(GetBibEntryWithDOI("10.1038/s41597-023-02633-x")) %>% remove_rownames()) %>% 
  mutate(bibliography_id = "Maxwell_et_al_2023_synthesis",
         publication_type = "synthesis source")

# synthesized dataset
maxwell_data <- data.frame(study_id = unique(dat$study_id)) %>% 
  bind_cols(as.data.frame(GetBibEntryWithDOI("10.5281/zenodo.8414110")) %>% remove_rownames()) %>% 
  mutate(bibliography_id = "Maxwell_et_al_2023_data",
         publication_type = "synthesis dataset")
# https://doi.org/10.5281/zenodo.8414110

# Fu et al 2021 synthesis
fu_synth <- data.frame(study_id = c("Wan_et_al_2017", "Wang_et_al_2017", "Loh_et_al_2018", "Lu_et_al_2019")) %>% 
  bind_cols(as.data.frame(GetBibEntryWithDOI("10.1111/gcb.15348")) %>% remove_rownames()) %>% 
  mutate(bibliography_id = "Fu_et_al_2021_synthesis",
         publication_type = "associated source")

xia_synth <- data.frame(study_id = c("Gao_et_al_2016", "Liu_et_al_2017", "Xia_et_al_2022")) %>% 
  bind_cols(as.data.frame(GetBibEntryWithDOI("10.1111/gcb.16325")) %>% remove_rownames()) %>% 
  mutate(bibliography_id = "Xia_et_al_2022_synthesis",
         publication_type = "associated source")

# combine all citations 
synthesis_citations <- synthesis_bib %>% 
  mutate(bibliography_id = ifelse(bibtype == "Misc", paste0(study_id, "_data"),
                                  paste0(study_id, "_article")),
         publication_type = ifelse(bibtype == "Article", "associated source", "primary dataset")) %>% 
  bind_rows(markewich_citation) %>% 
  bind_rows(maxwell_synth) %>% 
  bind_rows(maxwell_data) %>% 
  bind_rows(fu_synth) %>% 
  bind_rows(xia_synth) %>% 
  arrange(study_id) %>% 
  select(-c(editor, language, keywords, copyright)) %>% 
  select(study_id, bibliography_id, publication_type, everything())

# unique(dat$study_id)[!(unique(dat$study_id) %in% unique(synthesis_citations$study_id))]

write_excel_csv(synthesis_citations, "data/primary_studies/Maxwell_et_al_2023/derivative/Maxwell_et_al_2023_study_citations.csv")

# Create .bib file
# this is more of a test that bib IDs are unique
bib_file <- synthesis_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# as.BibEntry(bib_file)

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/

## 

# studies <- unique(cores$study_id)
# 
# for (i in studies) {
#   cores <- cores_master %>% filter(study_id == i)
#   depthseries <- depths_master %>% filter(study_id == i)
#   
#   writeDataVizReport(i)
# }


