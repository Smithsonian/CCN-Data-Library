## CCRCN Data Library Hook Script ####

# Data Release: Collection, analysis, and age-dating of sediment cores from a salt marsh platform and ponds, Rowley, Massachusetts, 2014-15
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5fb41153d34eb413d5e0af37
# Contact: Sheron Luk

# Data Citation:
# Luk, S.Y., Spivak, A.C., Eagle, M.J., and O'Keefe-Suttles, J.A., 2020, Collection, analysis, and age-dating of sediment cores from a salt marsh platform and ponds, Rowley, Massachusetts, 2014-15: 
# U.S. Geological Survey data release, https://doi.org/10.5066/P9HIOWKT.

# Associated Pub Citation:
# Luk, S.Y., Todd-Brown, K. Gonneea, M.E., McNichol, A.P., Sanderman, J., Gosselin, K., Spivak, A.C., 2020, Soil organic carbon development and turnover in natural and disturbed salt marsh environments: 
# Geophysical Research Letters, https://doi.org/10.1029/2020GL090287.

## Prep Workspace ####

# load libraries
library(tidyverse)
library(lubridate)
library(RefManageR)
# library(anytime)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data
raw_data <- read_csv("./data/primary_studies/Luk_2020/original/data_PIE_marsh_radioisotope.csv", na = "NaN")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Luk_et_al_2020"

names(raw_data) <- tolower(names(raw_data))

data <- raw_data %>%
  rename(habitat = status, 
         core_latitude = lat, 
         core_longitude = lon,
         core_elevation = elevation, # is this elevation for the depth increment?
         dry_bulk_density = dbd,
         total_pb210_activity = `210pb`,
         total_pb210_activity_se = `210pb_e`,
         excess_pb210_activity = `210pbex`,
         excess_pb210_activity_se = `210pbex_e`,
         ra226_activity = `226ra`,
         ra226_activity_se = `226ra_e`,
         cs137_activity = `137cs`,
         cs137_activity_se = `137cs_e`) %>%
  mutate(study_id = id,
         core_id = ifelse(habitat == "POND", str_c(habitat, site_id, sep = "_"),
                          str_c(habitat, site_id, core_id, sep = "_")),
         core_elevation_notes = "Calculated by subtracting the minimum depth of soil horizon from the NAVD88 elevation of the core location (given at min_depth = 0)",
         pb210_unit = "decaysPerMinutePerGram",
         ra226_unit = "decaysPerMinutePerGram",
         sample_volume = pi*(depth_max-depth_min)*(5.5^2),
         core_year = year(as.Date(date, format = "%m/%d/%Y")),
         core_month = month(as.Date(date, format = "%m/%d/%Y")),
         core_day = day(as.Date(date, format = "%m/%d/%Y"))) %>%
  mutate(site_id = str_c("SITE", site_id, sep = "_")) %>%
  select(-date)

# cores
cores <- data %>%
  filter(depth_min == 0) %>%
  select(study_id, site_id, contains("core"), habitat, core_elevation) %>%
  # distinct() %>%
  mutate(core_position_method = "RTK",
         core_elevation_method = "RTK",
         core_elevation_datum = "NAVD88",
         # core_elevation = 1.46,
         vegetation_class = "emergent",
         vegetation_method = "field observation",
         vegetation_notes = "marsh sites dominated by Spartina patens, Spartina alterniflora, and Distichlis spicata",
         inundation_class = ifelse(habitat == "MARSH", "high", "low"),
         inundation_method = "field observation")

final_cores <- reorderColumns("cores", cores)

# depthseries
depthseries <- data %>%
  select(-c(core_longitude, core_latitude, habitat, contains("core_elevation"),
            core_year, core_month, core_day))

final_depthseries <- reorderColumns("depthseries", depthseries)

# methods
methods <- data.frame(study_id = id,
                      compaction_flag = "No obvious compaction", 
                      # core_position_accuracy = 0.001,
                      dry_bulk_density_flag = "freeze dried",
                      coring_method = "piston corer",
                      pb210_counting_method = "gamma",
                      cs137_counting_method = "gamma")
final_methods <- reorderColumns("methods", methods)


#### Study Citation ####

data_release_doi <- "10.5066/P9HIOWKT"
pub_doi <- "10.1029/2020GL090287"

data_bib <- GetBibEntryWithDOI(data_release_doi)
pub_bib <- GetBibEntryWithDOI(pub_doi)

# Convert citations to dataframe
pub_citation <- as.data.frame(pub_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = id) %>%
  mutate(doi = tolower(doi),
         bibliography_id = id,
         key = id)

# double check this citation
data_citation <- as.data.frame(data_bib) %>%
  rownames_to_column("key") %>%
  mutate(study_id = "Luk_et_al_2020_data") %>%
  mutate(doi = tolower(doi),
         bibliography_id = "Luk_et_al_2020_data",
         key = "Luk_et_al_2020_data")

# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- pub_citation %>%
  bind_rows(data_citation) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  # slice(1) %>%
  # select(-study_id, -bibliography_id, -publication_type) %>%
  # distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Luk_2020/derivative/Luk_et_al_2020.bib")


## QA/QC ###############

leaflet::leaflet(cores) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~as.numeric(core_longitude), lat = ~as.numeric(core_latitude), radius = 5, label = ~site_id)

# Make sure column names are formatted correctly: 
test_colnames("cores", cores) # habitat
test_colnames("depthseries", depthseries) # elevation 7be 7be_e
test_colnames("methods", methods)

## Write derivative data ####
# write_csv(sites, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_sites.csv")
write_csv(final_cores, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_cores.csv")
# write_csv(species, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_species.csv")
write_csv(final_methods, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_methods.csv")
write_csv(final_depthseries, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_study_citations.csv")


