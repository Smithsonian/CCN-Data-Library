## CCN Data Library ####

# Curation script for Everhart et al 2020 data curation
# Contact: Henry Betts, BettsH@si.edu
# URL: https://coastal.er.usgs.gov/data-release/doi-P926MS6T/

library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


## Create datasheets ####
gamma_raw <- data.frame()
for (i in 1:6) {
  temp_df <- read_excel("./data/primary_studies/Everhart_et_al_2020/original/GammaSpectrometry.xlsx", sheet = i, skip = 1, na = c("--", "ND"))
  gamma_raw <- rbind(gamma_raw, temp_df) %>% 
    drop_na(`Core ID`)
}

alpha_raw <- data.frame()
for (i in 1:8) {
  temp_df <- read_excel("./data/primary_studies/Everhart_et_al_2020/original/AlphaSpectrometry.xlsx", sheet = i, skip = 1, na = "a = replicate analysis")
  alpha_raw <- rbind(alpha_raw, temp_df) 
}

alpha_push <- data.frame()
for (i in 9:10) {
  temp_df <- read_excel("./data/primary_studies/Everhart_et_al_2020/original/AlphaSpectrometry.xlsx", sheet = i, skip = 1, na = "a = replicate analysis")
  alpha_push <- rbind(alpha_push, temp_df) 
}

loi_raw <- read_excel("./data/primary_studies/Everhart_et_al_2020/original/LossOnIgnition.xlsx", skip = 1)
site_raw <- read_excel("./data/primary_studies/Everhart_et_al_2020/original/SiteInformation.xlsx", skip = 2)


## Curate ####
methods <- data.frame(study_id = "Everhart_et_al_2020",
                      method_id = c("push core, gamma spectroscopy", "push core, alpha spectroscopy", "surface sample, alpha spectroscopy"),
                      coring_method = c("push core", "push core", "surface sample"),
                      dry_bulk_density_temperature = 110,
                      dry_bulk_density_time = 6, 
                      dry_bulk_density_sample_mass = "1-5",
                      dry_bulk_density_flag = "time approximate",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 6,
                      cs137_counting_method = c("gamma", NA, NA),
                      pb210_counting_method = c("gamma", "alpha", "alpha"),
                      ra226_assumption = c("each sample", NA, NA))
                         

alpha_joiner <- alpha_raw %>% 
  drop_na() %>% 
  separate(`Depth\r\n (cm)`, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(core_id = gsub("[.]", "_", `Core ID`),
         # core_id = case_when(grepl("a", depth_max) ~ paste(core_id, 2, sep = "_"),
         #                     T ~ core_id),
         depth_max = gsub("a", "", depth_max)) %>% 
  rename(total_pb210_activity = `Total Pb-210 \r\n(dpm/g)`,
         total_pb210_activity_se = `Total Pb-210 Error \r\n(+/- dpm/g)`)

alpha_push_joiner <- alpha_push %>% 
  drop_na() %>% 
  mutate(core_id = gsub(" ", "_", `Sample ID`),
         core_id = gsub("a", "", core_id)) %>% 
  rename(total_pb210_activity = `Total Pb-210 \r\n(dpm/g)`,
         total_pb210_activity_se = `Total Pb-210 Error \r\n(+/- dpm/g)`)

gamma_joiner <- gamma_raw %>% 
  mutate(core_id = gsub("2019-302-CNT |2019-306-CNT ", "", `Core ID`),
         core_id = gsub("[.]", "_", core_id),
         core_id = case_when(grepl("Lc", core_id) ~ NA_character_,
                             T ~ core_id),
         spectroscopy = "gamma spectroscopy") %>% 
  drop_na(core_id) %>% 
  separate("Depth\r\n (cm)", into = c("depth_min", "depth_max"), sep = "-") %>% 
  rename(total_pb210_activity = `Pb-210 \r\n(dpm/g)`,
         total_pb210_activity_se = `Pb-210 Error \r\n(+/- dpm/g)`)

site_joiner <- site_raw %>% 
  mutate(core_id = gsub("[.]| ", "_", `Site ID`),
         date = as.Date(`Date\r\nCollected`),
         day = day(date),
         month = month(date),
         year = year(date)) %>% 
  rename(latitude = Latitude,
         longitude = Longitude)

loi_joiner <- loi_raw %>% 
  separate(`Depth\r\n (cm)`, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(core_id = gsub("-", "_", `Core ID`)) %>% 
  rename(fraction_organic_matter = `Loss On \r\nIgnition \r\n(gOM/gdry)`)

depth_raw <- full_join(alpha_joiner, alpha_push_joiner) %>% 
  mutate(spectroscopy = "alpha spectroscopy") %>% 
  full_join(gamma_joiner) %>% 
  full_join(site_joiner) %>% 
  full_join(loi_joiner, by = join_by(depth_min, depth_max, core_id)) %>% 
  rename(cs137_activity_se = "Cs-137 Error \r\n(+/- dpm/g)",
         cs137_activity = "Cs-137 \r\n(dpm/g)",
         ra226_activity = "Ra-226 \r\n(dpm/g)",
         ra226_activity_se = "Ra-226 Error \r\n(+/- dpm/g)",
         th234_activity = `Th-234 \r\n(dpm/g)`, 
         th234_activity_se = `Th-234 Error \r\n(+/- dpm/g)`, 
         k40_activity = `K-40 \r\n(dpm/g)`, 
         k40_activity_se = `K-40 Error \r\n(+/- dpm/g)`) %>% 
  mutate(method_id = paste(`Type of Samples`, spectroscopy, sep = ", "),
         study_id = "Everhart_et_al_2020",
         site_id = case_when(grepl("ES", core_id) ~ "Mockhorn_Island",
                             grepl("GW|GSS", core_id) ~ "Goodwin_Island",
                             grepl("PI", core_id) ~ "Plum_Island_Estuary",
                             grepl("SA", core_id) ~ "South_Altamaha",
                             T ~ NA_character_),
         depth_min = case_when(grepl("Surface Sample", method_id) ~ 0,
                               T ~ as.numeric(depth_min)),
         depth_max = case_when(grepl("Surface Sample", method_id) ~ 10,
                               T ~ as.numeric(depth_max)))

depthseries <- depth_raw %>% 
  select(study_id, site_id, core_id, method_id, depth_min, depth_max, fraction_organic_matter, cs137_activity,
         cs137_activity_se, total_pb210_activity, total_pb210_activity_se, ra226_activity, ra226_activity_se,
         th234_activity, th234_activity_se, k40_activity, k40_activity_se)

cores <- depth_raw %>% 
  mutate(habitat = "marsh") %>% 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, habitat) %>% 
  distinct()

uncontrolled_attributes <- data.frame(attribute_name = "th234_activity",
                                      attribute_definition = "Radioactivity counts per unit dry weight for 234 Th.",
                                      data_type = "numeric",
                                      units = "dpm/g",
                                      study_id = "Everhart_et_al_2020") %>% 
  add_row(attribute_name = "th234_activity_se",
          attribute_definition = "Estimated uncertainty in th234_activity",
          data_type = "numeric",
          units = "dpm/g",
          study_id = "Everhart_et_al_2020") %>% 
  add_row(attribute_name = "k40_activity",
          attribute_definition = "Radioactivity counts per unit dry weight for 40 K.",
          data_type = "numeric",
          units = "dpm/g",
          study_id = "Everhart_et_al_2020") %>% 
  add_row(attribute_name = "k40_activity_se",
          attribute_definition = "Estimated uncertainty in k40_activity",
          data_type = "numeric",
          units = "dpm/g",
          study_id = "Everhart_et_al_2020")

study_citations <- data.frame(study_id = "Everhart_et_al_2020", 
                              bibliography_id = "Everhart_et_al_2020_data", 
                              publication_type = "primary dataset", 
                              bibtype = "Misc", 
                              title = "Sediment Radiochemical Data from Georgia, Massachusetts, and Virginia Coastal Marshes",
                              author = "Everhart,Cheyenne S., Smith, Christopher G., Ellis, Alisha M., Marot, Marci E., Coleman, Daniel J., Guntenspergen, Glenn R., Kirwan, Matthew L.",
                              doi = "10.5066/P926MS6T.", 
                              url = "https://doi.org/10.5066/P926MS6T.", 
                              year = 2020,
                              publisher = "U.S. Geological Survey data release") 

bib_file <- study_citations %>%
  remove_rownames() %>%
  select(-c(study_id)) %>%
  column_to_rownames("bibliography_id")


## QAQC ####
leaflet(cores) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## Write files ####
write_csv(cores, "./data/primary_studies/Everhart_et_al_2020/derivative/Everhart_et_al_2020_cores.csv")
write_csv(depthseries, "./data/primary_studies/Everhart_et_al_2020/derivative/Everhart_et_al_2020_depthseries.csv")
write_csv(methods, "./data/primary_studies/Everhart_et_al_2020/derivative/Everhart_et_al_2020_methods.csv")
write_csv(study_citations, "./data/primary_studies/Everhart_et_al_2020/derivative/Everhart_et_al_2020_study_citations.csv")


## Write Data Visualization Report ####
rmarkdown::render(input = "data/primary_studies/Everhart_et_al_2020/Everhart_et_al_2020_datavis.Rmd",
                  output_file = "Everhart_et_al_2020_datavis.html",
                  output_dir = "data/primary_studies/Everhart_et_al_2020")

