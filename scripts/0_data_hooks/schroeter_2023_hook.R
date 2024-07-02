spc_ussft

## CCN Data Library Hook Script ####
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(RefManageR)
library(sf)
library(leaflet)
# library(lubridate)
# library(anytime)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in datasets
schroeter_dat <- read_csv("./data/primary_studies/Schroeter_et_al_2023/original/wetland_process_soil_properties-2023-06-02_17-44-20.csv")

# guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Curation ####

## Transform CRS
## test conversion: https://www.ngs.noaa.gov/NCAT/

# provide this fnx a dataframe, specify the lon and lat columns
# along with the old CRS and new CRS to apply

transformCRS <- function(df, lon_col, lat_col, old_crs, new_crs){
  
  coords_old <- st_as_sfc(df, coords = c(lon_col, lat_col), crs = old_crs)
  coords_new <- st_transform(coords_old, new_crs)
  
  coords_final <- coords_new %>% 
    mutate(longitude = sf::st_coordinates(.)[,1],
           latitude = sf::st_coordinates(.)[,2]) %>% 
    st_drop_geometry()
  
  return(coords_final)
}

# cores
cores <- schroeter_dat %>% distinct(date, spc_zone, elevation_mllw,
                                    waypoint_northing_spc_ussft, waypoint_easting_spc_ussft) %>% 
  mutate(easting = waypoint_easting_spc_ussft/3.281,
         northing = waypoint_northing_spc_ussft/3.281)

coords_transformed <- transformCRS(cores, 
                                   lon_col = "waypoint_easting_spc_ussft", lat_col = "waypoint_northing_spc_ussft",
                                   old_crs = 4269, new_crs = 4326)


leaflet(coords_transformed) %>% addTiles() %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 2, label = ~spc_zone)

# depthseries
ds_wide <- schroeter_dat %>%
  pivot_wider(names_from = soil_property_code, values_from = soil_property_value) %>% 
  mutate(depth_min = 0, depth_max = 10) %>% 
  arrange(waypoint_northing_spc_ussft) %>% 
  select(date, spc_zone, elevation_mllw,
         waypoint_northing_spc_ussft, waypoint_easting_spc_ussft, depth_min, depth_max, "Bulk-Density", "OM-LOI", "C-Org-LOI", "TOC-CM")
# not sure if these lists are replicates? BDB is not recorded for these instances


# visualize fraction carbon
# ggplot(ds_wide) +
  # geom_point(aes(as.numeric(`Bulk-Density`), as.numeric(`TOC-CM`)), pch = 1)
  # facet_wrap(~core_id) + theme_bw()

# methods
# methods <- raw_methods %>% 
#   mutate(method_id = "single set of methods", 
#          age_depth_model_notes = "custom box model used", 
#          excess_pb210_model = "CRS") %>% 
#   select(-ra226_counting_method)

# impacts (no change)
# impacts <- raw_impacts 

# species


################

## Create citation info 

id <- "Schroeter_et_al_2023"

paper_bib_raw <- as.data.frame(GetBibEntryWithDOI("10.1111/rec.13936"))
data_bib_raw <- as.data.frame(GetBibEntryWithDOI("10.6073/pasta/f45c73366967c532c57ea40c21a53dba"))

# Convert this to a dataframe
# paper_biblio <- as.data.frame(paper_bib_raw) %>%
#   mutate(study_id = id,
#          bibliography_id = "Arias-Ortiz_et_al_2015_article",
#          publication_type = "associated source") %>%
#   remove_rownames()
# 
# data_biblio <- as.data.frame(data_bib_raw) %>%
#   mutate(study_id = id,
#          bibliography_id = "Schroeter_et_al_2023_data",
#          publication_type = "primary dataset") %>%
#   remove_rownames()
# 
# Curate biblio so ready to read out as a BibTex-style .bib file
# study_citations <- bind_rows(data_bib_raw, paper_bib_raw) %>%
#   mutate(study_id = id,
#          bibliography_id = c("Schroeter_et_al_2023_data", "Schroeter_et_al_2023_article"),
#          publication_type = c("primary dataset", "associated source")) %>%
#   remove_rownames() %>% 
#   # mutate(month = ifelse(is.na(month), "jan", month)) %>%
#   select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  column_to_rownames("bibliography_id")

# write files
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023.bib")
write_csv(study_citations, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_study_citations.csv")

## QA/QC ###############

table_names <- c("methods", "cores", "depthseries", "species", "impacts")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write derivative data ####
write_csv(cores, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_cores.csv")
write_csv(methods, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_methods.csv")
write_csv(depthseries, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_depthseries.csv")
write_csv(impacts, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_impacts.csv")
write_csv(species, "./data/primary_studies/Schroeter_et_al_2023/derivative/Schroeter_et_al_2023_species.csv")


