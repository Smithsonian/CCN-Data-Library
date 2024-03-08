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
library(lubridate)
library(RefManageR)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data
cores_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_cores.csv")
depthseries_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_species.csv")
impacts_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_impacts.csv")
methods_raw <- read_csv("./data/primary_studies/Poppe_2019/original/poppe_and_rybczyk_2019_material_and_methods.csv")

## Curate Data ####

# create ID lookup to retain the estuary association
id_lookup <- distinct(cores_raw, estuary_id, site_id) %>% 
  separate(estuary_id, into = "estuary", sep = " ", remove = F) %>% 
  mutate(new_site = paste0(estuary, "_", site_id)) %>% 
  select(site_id, new_site)

# Rename core year variable since date requires a full date string
cores <- cores_raw %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>% 
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK"),
         core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id, -core_date) %>% 
  left_join(id_lookup) %>% select(-site_id) %>% rename(site_id = new_site) %>% select(study_id, site_id, everything())

# leaflet(cores) %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~site_id)

# Provide unit columns
depthseries <- depthseries_raw %>%
  mutate(method_id = "single set of methods",
         pb210_unit = ifelse(!is.na(total_pb210_activity), "becquerelsPerKilogram", NA), 
         pb214_unit = ifelse(!is.na(pb214_activity), "becquerelsPerKilogram", NA),
         cs137_unit = ifelse(!is.na(cs137_activity), "becquerelsPerKilogram", NA)) %>%
  mutate(core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id, -fraction_carbon_modeled) %>%
  rename(fraction_carbon = fraction_carbon_measured) %>% 
  left_join(id_lookup) %>% select(-site_id) %>% rename(site_id = new_site) %>% select(study_id, site_id, everything())

depthseries %>% 
  # filter(core_id == "LM3_16" | core_id == "LM4_24") %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = study_id)) + 
  geom_point(alpha = 0.6) +
  # ggtitle("Poppe_and_Rybczyk_2019 cores with zero fraction C")
  facet_wrap(~study_id, scales = "free")
# cores LM3_16 and LM4_24 have 0s in the fraction carbon, is there a reason for this?
# ggsave("database_query/figures/poppe_and_rybczyk_2019_zero_carbon.jpg")

# Format species correctly
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  mutate(species_code = gsub("sp.", "spp", species_code))%>% 
  mutate(core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, 
                          paste(site_id, core_id, sep="_"), core_id)) %>% 
  left_join(id_lookup) %>% select(-site_id) %>% rename(site_id = new_site) %>% select(study_id, site_id, everything())

impacts <- impacts_raw %>%
  mutate(core_id = ifelse(nchar(as.character(core_id)) == 1 | nchar(as.character(core_id)) == 2, paste(site_id, core_id, sep="_"), core_id)) %>%
  select(-estuary_id) %>% 
  left_join(id_lookup) %>% select(-site_id) %>% rename(site_id = new_site) %>% select(study_id, site_id, everything()) %>% 
  distinct() #removing duplicates

methods <- methods_raw %>%
  mutate(method_id = "single set of methods") %>%
  # mutate(study_id = id,
  #        method_id = recode(method_id,
  #                           "Poppe_Rybczyk_2018" = "Poppe_and_Rybczyk_2018",
  #                           "Poppe_Rybczyk_2019" = "Poppe_and_Rybczyk_2019")) %>% 
  mutate(sediment_sieve_size = as.numeric(gsub(" mm", "", sediment_sieve_size)),
         carbon_profile_notes = "Modeled fraction carbon values available in https://doi.org/10.25573/data.10005248") %>%
  reorderColumns("methods", .)

## Citation ####

study_citations <- read_csv("data/primary_studies/Poppe_2019/intermediate/poppe_and_rybczyk_2019_associated_publications.csv")

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019.bib")
write_csv(study_citations, "data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_study_citations.csv")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects
impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
species <- updated$species
cores <- updated$cores %>% 
  # add core NE_B position as a site level replicate of core NE_A 
  mutate(latitude = case_when(core_id == "NE_B" ~ 48.0352,
                              core_id == "SP_B" ~ 47.9902,
                              T ~ latitude),
         longitude = case_when(core_id == "NE_B" ~ -122.1626,
                               core_id == "SP_B" ~ -122.1611,
                               T ~ longitude),
         position_notes = case_when(core_id == "NE_B" ~ "site level replicate of core NE_A's position",
                                    core_id == "NE_B" ~ "site level replicate of core SP_A's position",
                                    T ~ position_notes))
  
  
## QA/QC ###############

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)
testTaxa(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## Write files #########
write_csv(cores, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_cores.csv")
write_csv(depthseries, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_depthseries.csv")
write_csv(species, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_species.csv")
write_csv(impacts, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_impacts.csv")
write_csv(methods, "./data/primary_studies/Poppe_2019/derivative/poppe_and_rybczyk_2019_methods.csv")

