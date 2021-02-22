## CCRCN Data Library Hook Script ####

# Data Release: Long-term soil carbon data and accretion from four marsh types in Mississippi River Delta in 2015
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5b3299d4e4b040769c159bb0
# Contact: Melissa Baustian

# Data Citation:
# Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., Carruthers, T.J.B., Allison, M.A., and Hall, C.T., 2021, 
# Long-term soil carbon data and accretion from four marsh types in Mississippi River Delta in 2015: U.S. Geological Survey data release,
# https://doi.org/10.5066/P93U3B3E.


## Prep Workspace ####

# load libraries
library(tidyverse)
library(lubridate)
library(RefManageR)
library(readxl)
library(leaflet)
# library(anytime)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data
raw_depthseries <- read_csv("./data/primary_studies/Baustian_et_al_2021/intermediate/Baustian_Long_Term_Carbon_Soil.csv", na = ".")
raw_radio <- read_csv("./data/primary_studies/Baustian_et_al_2021/original/Baustian_Long_Term_Radionuclide.csv", na = ".")
crms_sites <- read_csv("./data/primary_studies/Baustian_et_al_2021/intermediate/CRMS_Long_Lat.csv")
raw_siteinfo <- read_xlsx("data/primary_studies/Baustian_et_al_2021/original/Baustian_WI_LongTerm_Soil_Core_Site Info.xlsx")
# raw_coreinfo <- read_csv("./data/primary_studies/Baustian_et_al_2021/intermediate/Baustian_Short Term Carbon_Soil Core Data.csv")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Baustian_et_al_2021"

# Reference Tables ----

cores_ref <- data.frame(salinity_class = c("fresh", "intermediate", "brackish", "saline"), 
                        
                        # Marsh_Type = c(1:4),
                        core_elevation = c(0.34, 0.13, 0.14, 0.14))
# map referenced for marsh type assignment: https://pubs.usgs.gov/sim/3290/pdf/sim3290.pdf

# depth_ref <- data.frame(Core_Increment = c(1,2,3,5),
#                         depth_min = c(0,2,4,8),
#                         depth_max = c(2,4,6,10))

# locations <- crms_sites %>%
#   mutate(core_id = gsub("CRMS0", "", `CRMS Site`)) %>%
#   mutate(core_id = gsub("CRMS", "", core_id))


coreinfo <- raw_siteinfo %>%
  rename(species = `Target Species`,
         core_latitude = Lat, 
         core_longitude = Long,
         core_id = `CRMS Site ID`) %>%
  mutate(study_id = id,
         site_id = recode(Basin,
                          "BA" = "Barataria basin",
                          "TE" = "Terrebonne basin"),
         salinity_class = tolower(`2014 Habitat Type`),
         core_position_method = "RTK",
         core_elevation_datum = "NAVD88",
         vegetation_class = "emergent",
         vegetation_method = "measurement",
         salinity_method = "field observation") %>%
  full_join(cores_ref) %>%
  # Marsh type is defined as intermediate salinity (based on vegetation) but I'm reclassifying it to brackish
  mutate(salinity_class = recode(salinity_class, "intermediate" = "brackish")) %>%
  select(-c(Basin, `2014 Habitat Type`))
# core dates will have to be merged from depthseries


# coreinfo <- raw_coreinfo %>%
#   select(Site, Basin, Marsh_Type) %>%
#   distinct() %>%
#   rename(core_id = Site) %>%
#   mutate(study_id = id,
#          site_id = recode(Basin,
#                           "BA" = "Barataria basin",
#                           "TE" = "Terrebonne basin"),
#          core_position_method = "RTK",
#          core_elevation_datum = "NAVD88",
#          vegetation_class = "emergent",
#          vegetation_method = "measurement",
#          salinity_method = "field observation") %>%
#   left_join(locations, by = "core_id") %>%
#   full_join(cores_ref) %>%
#   rename(core_latitude = Latitude,
#          core_longitude = Longitude) %>%
#   mutate(vegetation_notes = case_when(salinity_class == "fresh" ~ "dominated by Panicum hemitomon, Sagittaria lancifolia, Eleocharis baldwinii, or Cladium jamaicense",
#                                       salinity_class == "intermediate" ~ "dominated by Leptochloa fusca, Panicum virgatum, Paspalum vaginatum, Phragmites australis, or Schoenoplectus americanus",
#                                       salinity_class == "brackish" ~ "dominated by Spartina patens but occasionally by Spartina cynosuroides, Spartina spartinae, or Bolboschoenus robustus",
#                                       salinity_class == "saline" ~ " dominated by Spartina alterniflora, Distichlis spicata, or Avicennia germinans.")) %>%
#   select(-c(Basin, Marsh_Type, "CRMS Site"))

# create core site lookup
site_core <- coreinfo %>% select(site_id, core_id)


# Depthseries ----

stock <- raw_depthseries %>%
  rename(core_id = Site,
         dry_bulk_density = `Bulk Density (g/cm^3)`) %>%
  mutate(study_id = id,
         fraction_organic_matter = `Organic matter (percent)`/100,
         increments = recode(`Core Increment (cm)`, 
                             "14-Dec" = "12-14",
                             "12-Oct" = "10-12")) %>%
  separate(col = increments, into = c("depth_min", "depth_max"), sep = "-") %>% 
  select(-c(`Core Increment (cm)`, `Moisture (percent)`, `Organic matter (percent)`))

# curate radionuclide data
radionuclides <- raw_radio %>%
  drop_na(`CRMS Site`) %>%
  rename("core_id" = "CRMS Site",
         "core_date" = "Field Collection Date",
         "cs137_activity" = "Cs-137 (dpm/g)",
         "cs137_activity_se" = "Cs-137 - error (dpm/g)",
         "total_pb210_activity" = "Total Pb-210 (dpm/g)",
         "total_pb210_activity_se" = "Total Pb-210 - error (dpm/g)",
         "excess_pb210_activity" = "Excess Pb-210 (dpm/g)",
         "excess_pb210_activity_se" = "Excess Pb-210 - error (dpm/g)") %>%
  separate(col = `Core Increment (cm)`, into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(study_id = id, 
         core_id = as.character(core_id),
         # depth_max = gsub(" cm", "", depth_max),
         # depth_max = as.numeric(depth_max),
         # depth_min = as.numeric(depth_min),
         pb210_unit = "disintegrationsPerMinutePerGram",
         cs137_unit = "disintegrationsPerMinutePerGram") %>%
  left_join(core_batch) %>%
  select(-`Mid-Depth (cm)`, -`Radionuclide Counted Date`, -core_date)

# there are 7 sites missing from the radionuclide table
unique(stock$core_id)[which(!(unique(stock$core_id) %in% unique(radionuclides$core_id)))]

# join depthseries info
depthseries <- full_join(stock, radionuclides) %>%
  full_join(site_core) # merge site info

# create date ref for core table
# date_ref <- depthseries %>% select(site_id, core_id, core_date) %>% distinct() %>%
#   drop_na(core_date) # drop NA dates

final_depthseries <- reorderColumns("depthseries", depthseries) %>%
  select(-c(Batch, core_date, "2014_Habitat Type", Most_Freq_Occ_Habitat_1949to1988))

# Cores ----

# use this to supply core dates to the core table
date_ref <- stock %>% select(Batch, core_id) %>% distinct() %>%
  mutate(core_date = case_when(Batch == "1" ~ "2/1/2015",
                               Batch == "2" ~ "7/1/2015"))

cores <- left_join(coreinfo, date_ref) %>%
    mutate(core_year = year(as.Date(core_date, format = "%m/%d/%Y")),
           core_month = month(as.Date(core_date, format = "%m/%d/%Y")),
           core_day = day(as.Date(core_date, format = "%m/%d/%Y"))) %>%
  mutate(core_length_flag = "core depth limited by length of corer") %>% 
  select(-core_date, -species, -Batch)

final_cores <- reorderColumns("cores", cores)

# Species
species <- coreinfo %>%
  select(study_id, site_id, core_id, species) %>%
  mutate(species = str_split(species, "/")) %>% 
  unnest(species) %>% mutate(species = trimws(species)) %>%
  separate(species, into = c("genus", "species"), sep = " ")
  

# Methods ----

methods <- data.frame(study_id = id,
                      coring_method = "mcauley corer",
                      dry_bulk_density_flag = "freeze dried",
                      loss_on_ignition_temperature = 550, 
                      loss_on_ignition_time = 14, 
                      # fraction_carbon_type = "total carbon", # no fraction carbon in the tables but metadata says it was calculated
                      cs137_counting_method = "gamma",
                      pb210_counting_method = "gamma",
                      dry_bulk_density_sample_volume = pi*((5.1/2)^2)*2)

final_methods <- reorderColumns("methods", methods)

#### Study Citation ####

data_doi <- "10.5066/P93U3B3E"
pub_doi <- "10.1029/2020JG005832" # doi not active yet (paper hasnt been officially published)

data_bib <- GetBibEntryWithDOI(data_doi)
# pub_bib <- GetBibEntryWithDOI(pub_doi)

# Convert citations to dataframe
data_citation <- as.data.frame(data_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

# pub_citation

# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- data_citation %>%
  # bind_rows(pub_citation) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  # distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021.bib")


## QA/QC ###############

leaflet(cores) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~as.numeric(core_longitude), lat = ~as.numeric(core_latitude), 
                   radius = 5, label = ~core_id)

# Make sure column names are formatted correctly: 

test_colnames("cores", final_cores)
test_colnames("depthseries", final_depthseries)
test_colnames("methods", methods)

test_unique_cores(final_cores)
test_core_relationships(final_cores, final_depthseries)


## Write derivative data ####
# write_csv(sites, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_sites.csv")
write_csv(final_cores, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_cores.csv")
# write_csv(species, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_species.csv")
write_csv(final_methods, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_methods.csv")
write_csv(final_depthseries, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Baustian_et_al_2021/derivative/Baustian_et_al_2021_study_citations.csv")


