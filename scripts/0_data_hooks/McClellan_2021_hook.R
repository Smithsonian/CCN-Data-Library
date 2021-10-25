## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data release

# Data for: Root-zone carbon and nitrogen pools across two chronosequences of coastal marshes formed using different restoration techniques: 
# Dredge sediment versus river sediment diversion
# Authors: S. Alex McClellan <alex.mcclellan@gmail.com>, Tracy Elsey-Quirk, Edward Laws, Ronald DeLaune

# Mendeley Data Release: https://doi.org/10.17632/5zbv2mb5zp.1.  
# Associated Article: https://doi.org/10.1016/j.ecoleng.2021.106326

library(tidyverse)
library(RefManageR)
library(lubridate)
library(readxl)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in data
soil_data_raw <- read_xlsx("./data/primary_studies/McClellan_et_al_2021/original/Soil_data_ECOENG_106326.xlsx")
methods_raw <- read_xlsx("./data/primary_studies/McClellan_et_al_2021/intermediate/McClellan_2021_material_and_methods.xlsx")
site_info <- read_delim("data/primary_studies/McClellan_et_al_2021/intermediate/site_locations.txt", 
                        delim = ", ", trim_ws = T) %>% select(-lat_coord, -long_coord)

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "McClellan_et_al_2021"

# Align tables to database guidance

# All quantities except moisture content and bulk density are expressed on a dry-soil basis.  
# † The coded ID represents each soil core section in the form of Ayy-p-xx, 
# where A is S for Sabine, A is W for WLD, yy is the marsh age in years, p is the plot number, 
# and xx is the upper soil depth of the 5-cm section; sections from reference marshes are nominally assigned as yy=50; 
# Sabine reference marshes A and B are denoted as plot numbers 1–3 and 4–6, respectively.
soil_data <- soil_data_raw %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  filter(`Coded ID†` != "Column sum") %>% 
  rename(sample_id = `Coded ID†`,
         fraction_organic_matter = `Soil loss-on-ignition (mg·g−1)`, # need conversion?
         fraction_carbon = `Total soil organic carbon (mg·g−1)`, # need conversion?
         dry_bulk_density = `Soil bulk density (g·cm−3)`) %>% 
  separate(sample_id, into = c("site_id", "plot", "depth_min"), sep = "-", remove = F) %>% 
  mutate(study_id = id,
         method_id = "single set of methods",
         plot = case_when(site_id == "S50" & plot <= 3 ~ paste0(plot,"A"),
                          site_id == "S50" & plot >= 4 ~ paste0(plot, "B"),
                          TRUE ~ plot),
         marsh = ifelse(grepl("S", site_id), "Sabine","WLD"),
         core_id = str_c(site_id, plot, sep = "-"),
         fraction_organic_matter = fraction_organic_matter/1000,
         fraction_carbon = fraction_carbon/1000,
         depth_min = as.numeric(depth_min),
         depth_max = depth_min + 5) %>% 
  select(-c(`Total soil nitrogen (mg·g−1)`, `Refractory soil nitrogen (mg·g−1)`,
            `Soil moisture content (%)`, `Refractory soil organic carbon (mg·g−1)`))

ggplot(soil_data) +
  # geom_point(aes(fraction_organic_matter, fraction_carbon))
  geom_point(aes(dry_bulk_density, fraction_carbon))

# need conversion:     
# "Soil loss-on-ignition (mg·g−1)" => fraction_organic_matter     
# "Total soil organic carbon (mg·g−1)" => fraction_carbon

depthseries <- soil_data %>% 
  reorderColumns("depthseries", .) %>% 
  select(-plot)

## ... Cores ####

core_data <- soil_data %>% distinct(study_id, site_id, core_id, marsh, plot) %>% 
  # vegetation is measured
  mutate(year = "2016",
         marsh_age = case_when(site_id == "S01" ~ "1 year",
                               site_id == "S06" ~ "6 years",
                               site_id == "S14" ~ "14 years",
                               site_id == "S33" ~ "33 years",
                               grepl("A", plot) ~ "Reference A",
                               grepl("B", plot) ~ "Reference B",
                               site_id == "W16" ~ "16 years",
                               site_id == "W29" ~ "29 years",
                               site_id == "W41" ~ "41 years",
                               site_id == "W60" ~ "Reference",
                               TRUE ~ NA_character_)) %>% 
  full_join(site_info) %>% 
  # separate(inundation_time, into = c("floodtime_min", "floodtime_max"), sep = "–", convert = T) %>% 
  mutate(elevation = elevation/100,
         elevation_se = elevation_se/100,
         # the 1 year old sabine marsh doesn't have much soil
         core_length_flag = ifelse(site_id == "S01", "core depth represents deposit depth",
                                   "core depth limited by length of corer"),
         vegetation_method = "measurement",
         position_notes = "triplicate samples were collected haphazardly within 10 m of these coordinates",
         elevation_datum = "NAVD88",
         # floodtime_avg = (floodtime_min + floodtime_max)/2,
         salinity_class = ifelse(marsh == "Sabine", "saline", "fresh"),
         elevation_method = ifelse(marsh == "Sabine", "RTK", "LiDAR"),
         vegetation_class = case_when(marsh == "WLD" ~ "forested to emergent", 
                                      marsh == "Sabine" & marsh_age == "1 year" ~ "unvegetated",
                                      TRUE ~ "emergent"),
         habitat = case_when(marsh == "WLD" ~ "swamp", # need to check this habitat assignment
                             vegetation_class == "unvegetated" ~ "unvegetated", 
                             TRUE ~ "marsh")) %>% 
  rename(elevation_accuracy = elevation_se) %>% 
  reorderColumns("cores", .) %>% 
  select(-c(plot, aboveground_biomass))

## ... Site ####

sites <- core_data %>% distinct(study_id, site_id, marsh, marsh_age, elevation, elevation_accuracy, inundation_time)
# include elevation or not? No site-level elevation, perhaps as a note in the core table?

## ... Methods ####

methods <- methods_raw %>% 
  drop_na(study_id) %>% 
  select_if(function(x) {!all(is.na(x))})

## ... Impacts ####

# WLD: sediment diversion => sediment added
# Sabine: marsh created through sediment addition => wetlands built

impacts <- soil_data %>% distinct(study_id, site_id, core_id) %>% 
  mutate(impact_class = ifelse(site_id == "WLD", "sediment added", "wetlands built"))

## ... Impacts ####

# sp_lookup <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv") %>% 
#   select(-notes, -recode_as)

species <- soil_data %>% distinct(study_id, site_id) %>% 
  mutate(species_code = ifelse(site_id == "WLD", "Nelumbo lutea, Colocasia esculenta, Polygonum puntatum, Salix nigra",
                               # sabine species
                               "Spartina alterniflora, Distichlis spicata, Spartina patens"),
         species_code = strsplit(species_code, split = ", "),
         code_type = "Genus species") %>% 
  unnest(species_code) %>% 
  resolveTaxa(., db = c(150, 9, 4, 3)) %>% # cooool new function!
  select(study_id, site_id, resolved_taxa, code_type) %>% 
  rename(species_code = resolved_taxa)

## 2. Study Citations ####

# the GitBibEntry function doesnt pull the article:
# A bibentry of bibtype ‘Article’ has to specify the field: c("journaltitle", "journal")
# may have to enter the citation into a spreadsheet

if(!file.exists("data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_study_citations.csv")){

  data_bib <- as.data.frame(GetBibEntryWithDOI("10.17632/5zbv2mb5zp.1"))
  # pub_doi <- "10.1016/j.ecoleng.2021.106326" # doesn't work, downloaded bib file instead
  pub_bib <- as.data.frame(ReadBib("data/primary_studies/McClellan_et_al_2021/original/McClellan_pub_citation_2021.bib"))

  # create citation df
  study_citations <- bind_rows(data_bib, pub_bib) %>%
    mutate(bibliography_id = c("McClellan_et_al_2021_data", "McClellan_et_al_2021_article"),
           study_id = id,
           publication_type = c("primary dataset", "associated source")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()

  ## Format bibliography
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")

  WriteBib(as.BibEntry(bib_file), "data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021.bib")
  write_csv(study_citations, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_study_citations.csv")
}

## 3. QA/QC ####

# map sites
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~core_id, radius = 2)

# test tables
table_names <- c("methods", "cores", "depthseries", "species", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries) 
fraction_not_percent(depthseries)
skim_results <- test_numeric_vars(depthseries)

# write files
write_csv(cores, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_cores.csv")
write_csv(species, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_species.csv")
write_csv(methods, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_methods.csv")
write_csv(depthseries, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_depthseries.csv")
write_csv(impacts, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_impacts.csv")

