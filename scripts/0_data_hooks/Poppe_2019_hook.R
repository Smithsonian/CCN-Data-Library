# Dataset: Sediment carbon stocks and sequestration rates in the Pacific Northwest region of Washington, US
# 
# Authors: Katrina  Poppe <katrina.poppe@wwu.edu>, John Rybczyk
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.10005248
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in six separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# poppe_and_rybczyk_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# poppe_and_rybczyk_2019_sites.csv - Contains positional and descriptive information on site locations.
# poppe_and_rybczyk_2019_cores.csv - Contains positional and descriptive information on core locations.
# poppe_and_rybczyk_2019_depthseries.csv - Contains raw depth-series information for all cores.
# poppe_and_rybczyk_2019_species.csv - Contains information on the dominant plant species at coring locations.
# poppe_and_rybczyk_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# poppe_and_rybczyk_2019_associated_publication.csv - Is a CSV file containing citation information for the associated publication accompanying this data release.

library(tidyverse)
library(RefManageR)

cores_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_species.csv")

# Rename core year variable since date requires a full date string
cores <- cores_raw %>%
  rename(core_year = core_date) 

# Provide unit columns
depthseries <- depthseries_raw %>%
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity), "becquerelsPerKilogram", NA), 
         pb214_unit = ifelse(!is.na(pb214_activity), "becquerelsPerKilogram", NA))

# Format species correctly
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  mutate(species_code = gsub("sp.", "spp", species_code))

write_csv(cores, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_cores.csv")
write_csv(depthseries, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_depthseries.csv")
write_csv(species, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_species.csv")
