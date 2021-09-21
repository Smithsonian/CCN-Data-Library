## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data release

# Data for: Root-zone carbon and nitrogen pools across two chronosequences of coastal marshes formed using different restoration techniques: 
# Dredge sediment versus river sediment diversion
# Authors: S. Alex McClellan <alex.mcclellan@gmail.com>, Tracy Elsey-Quirk, Edward Laws, Ronald DeLaune

# Mendeley Data Release: https://doi.org/10.17632/5zbv2mb5zp.1.  
# Associated Article: https://doi.org/10.1016/j.ecoleng.2021.106326.

library(tidyverse)
library(RefManageR)
library(lubridate)
library(readxl)
# library(anytime)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in data
soil_data_raw <- read_xlsx("./data/primary_studies/McClellan_et_al_2021/original/Soil_data_ECOENG_106326.xlsx")
# methods_raw <- read_csv("./data/primary_studies/McClellan_et_al_2021/intermediate/mcclellan_et_al_2021_materials_and_methods.csv")

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
         fraction_organic_matter = `Soil loss-on-ignition (mg·g−1)`, # need conversion
         fraction_carbon = `Total soil organic carbon (mg·g−1)`, # need conversion
         dry_bulk_density = `Soil bulk density (g·cm−3)`) %>% 
  separate(sample_id, into = c("site", "plot", "depth_min"), sep = "-") %>% 
  mutate(study_id = id,
         site_id = ifelse(grepl("S", site), "Sabine","WLD"),
         core_id = str_c(site, plot, sep = "-"),
         fraction_organic_matter = fraction_organic_matter/1000,
         fraction_carbon = fraction_carbon/1000,
         depth_min = as.numeric(depth_min),
         depth_max = depth_min + 5) %>% 
  select(-c(site, plot, `Total soil nitrogen (mg·g−1)`, `Refractory soil nitrogen (mg·g−1)`,
            `Soil moisture content (%)`, `Refractory soil organic carbon (mg·g−1)`)) %>% 
  reorderColumns("depthseries", .)
# there are no locations for the cores

ggplot(soil_data) +
  # geom_point(aes(fraction_organic_matter, fraction_carbon))
  geom_point(aes(fraction_organic_matter, dry_bulk_density))

# need conversion:     
# "Soil loss-on-ignition (mg·g−1)" => fraction_organic_matter     
# "Total soil organic carbon (mg·g−1)" => fraction_carbon

## ... Cores ####

cores <- soil_data %>% distinct(study_id, site_id, core_id) %>% 
  # vegetation is measured
  mutate(vegetation_method = "measurement")

## ... Methods ####

methods <- methods_raw

## ... Impacts ####

impacts <- impacts_raw
# WLD: sediment diversion
# Sabine: marsh created through sediment addition

## ... Impacts ####

species <- soil_data %>% distinct(study_id, site_id)
# WLD: Nelumbo lutea, Colocasia esculenta, Polygonum puntatum, Salix nigra
# Sabine: Spartina alterniflora (Sporobolus alterniflorus), Distichlis spicata, Spartina patens (Sporobolus pumilus)

## 2. Study Citations ####

if(!file.exists("data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_study_citations.csv")){
  
  data_doi <- "10.17632/5zbv2mb5zp.1"
  pub_doi <- "10.1016/j.ecoleng.2021.106326"
  
  citation_raw <- as.data.frame(GetBibEntryWithDOI(c(data_doi, pub_doi)))
  
  study_citations <- citation_raw %>%
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
  
  # WriteBib(as.BibEntry(bib_file), "data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021.bib")
  # write_csv(study_citations, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_study_citations.csv")
}

## 3. QA/QC ####

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries) # a few cores will not be present in the depthseries
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

# write files
# write_csv(cores, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_cores.csv")
# write_csv(species, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_species.csv")
# write_csv(methods, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_methods.csv")
# write_csv(depthseries, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_depthseries.csv")
# write_csv(impacts, "./data/primary_studies/McClellan_et_al_2021/derivative/mcclellan_et_al_2021_impacts.csv")

