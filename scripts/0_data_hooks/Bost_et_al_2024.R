# CCN Data Library Hook Script for Bost et al. 2024
# Written by JH
# 2024-03-22

# import libraries
library(tidyverse)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# import datasets
core_int <- read_csv("data/primary_studies/Bost_et_al_2024/intermediate/core_info.csv")
depth_int <- read_csv("data/primary_studies/Bost_et_al_2024/intermediate/depth_info.csv")
loi_int <- read_csv("data/primary_studies/Bost_et_al_2024/intermediate/loi_digitized.csv")
tab1 <- read_csv("data/primary_studies/Bost_et_al_2024/intermediate/table_1.csv")

id <- "Bost_et_al_2024"

# methods 
methods <- data.frame(study_id=id,
                      method_id = "single set of methods",
                      coring_method = "push core",
                      dry_bulk_density_sample_volume= 10,
                      loss_on_ignition_sample_volume = 10,
                      pb210_counting_method = "alpha",
                      excess_pb210_rate = "mass accumulation",
                      dating_notes = "CF or CRS")

methods <- reorderColumns("methods", methods)

# core-level
cores <- core_int %>% 
  left_join(tab1) %>% 
  mutate(study_id = id) %>% 
  rename(core_id = site_id,
         site_id = creek_type) %>% 
  mutate(position_method = "RTK",
         elevation_datum = "NAVD88",
         elevation_method = "RTK",
         salinity_class = "saline",
         salinity_method = "field observation",
         vegetation_class = "emergent",
         habitat = "marsh",
         inundation_class = "low",
         core_length_flag = "not specified")

cores <- reorderColumns("cores", cores)
names(cores)

depth_int <- depth_int %>% 
  mutate(depth_med = as.numeric(depth_med))

site_names <- cores %>% 
  select(site_n, site_id, core_id)

# depth series
depthseries <- loi_int %>% 
  mutate(depth_med = round(depth_mid +0.5)+0.5) %>% 
  full_join(depth_int) %>% 
  mutate(study_id=id) %>% 
  left_join(site_names) %>% 
  mutate(method_id = "single set of methods",
         depth_min = depth_med - 0.5,
         depth_max = depth_med + 0.5,
         fraction_organic_matter = loi_pct / 100,
         depth_interval_notes = "fraction_organic_matter digitized from plot in figure",
         pb210_unit = case_when(!is.na(total_pb210_activity) ~ "disintegrationsPerMinutePerGram"))

depthseries <- reorderColumns("depthseries", depthseries)
names(depthseries)
depthseries <- depthseries %>% select(-c(site_n:depth_med))
depthseries <- depthseries %>% 
  arrange(site_id, core_id, depth_min, depth_max)

# impacts?

# species
species <- site_names %>% 
  mutate(study_id = id,
         species_code = "Spartina alterniflora",
         code_type = "Genus species",
         habitat = "marsh") %>% 
  select(-site_n)

species <- reorderColumns("species", species)

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3)
#leaflet does not like special character in some site names 

cores <- select(cores, -c(site_n:core_date))

#table names
table_names <- c("methods", "cores", "depthseries", "species")

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
(results)


# Vis ouptut
writeDataVizReport(id)

# bibliography
library(RefManageR)

# Write Files
write_csv(depthseries, "./data/primary_studies/Bost_et_al_2024/derivative/Bost_et_al_2024_depthseries.csv")
write_csv(cores, "./data/primary_studies/Bost_et_al_2024/derivative/Bost_et_al_2024_cores.csv")
write_csv(methods, "./data/primary_studies/Bost_et_al_2024/derivative/Bost_et_al_2024_methods.csv")
write_csv(species, "./data/primary_studies/Bost_et_al_2024/derivative/Bost_et_al_2024_species.csv")

bost_bib <- as.data.frame(GetBibEntryWithDOI("10.1016/j.ecss.2024.108693")) %>% 
  mutate(bibliography_id = "Bost_et_al_2024_paper", 
         study_id = id, 
         publication_type = "combined dataset and manuscript") %>% 
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, everything())

write_csv(bost_bib, "data/primary_studies/Bost_et_al_2024/derivative/Bost_et_al_2024_study_citations.csv") 




