## CCRCN Data Library Hook Script ####

# Data Release: Collection, analysis, and age-dating of sediment cores from a salt marsh platform and ponds, Rowley, Massachusetts, 2014-15
# Data published by Science Base: https://www.sciencebase.gov/catalog/item/5fb41153d34eb413d5e0af37
# Contact: Sheron Luk

# Data:
# Radioisotope data: https://doi.org/10.5066/P9HIOWKT
# metadata: https://www.sciencebase.gov/catalog/file/get/5fb41153d34eb413d5e0af37?f=__disk__45%2Feb%2F89%2F45eb89acb7adc9ab9a9a3076ff1e2c8db42204fc&transform=1&allowOpen=true

# Bulk Soil Properties: https://www.bco-dmo.org/dataset/827298
# metadata: http://dmoserv3.bco-dmo.org/jg/info/BCO-DMO/Benthic_PP_at_TIDE/Bulk_soil_properties%7Bdir=dmoserv3.whoi.edu/jg/dir/BCO-DMO/Benthic_PP_at_TIDE/,data=dmoserv3.bco-dmo.org:80/jg/serv/BCO-DMO/Benthic_PP_at_TIDE/bulk_soil_properties.html0%7D?

# Associated Pub Citation:
# Luk, S.Y., Todd-Brown, K. Gonneea, M.E., McNichol, A.P., Sanderman, J., Gosselin, K., Spivak, A.C., 2020, Soil organic carbon development and turnover in natural and disturbed salt marsh environments: 
# Geophysical Research Letters, https://doi.org/10.1029/2020GL090287.

## Prep Workspace ####

# load libraries
library(tidyverse)
library(lubridate)
library(RefManageR)
library(readxl)
# library(anytime)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data
# raw_depthseries <- read_delim("http://dmoserv3.bco-dmo.org/jg/serv/BCO-DMO/Benthic_PP_at_TIDE/bulk_soil_properties.flat0?", 
#                               delim = " ")
raw_soil <- read_csv("data/primary_studies/Luk_2020/original/Spivak_Luk_bulk_soil_properties.csv", na = "nd")
raw_iso <- read_csv("./data/primary_studies/Luk_2020/original/data_PIE_marsh_radioisotope.csv", na = "NaN")
raw_methods <- read_xlsx("data/primary_studies/Luk_2020/intermediate/Luk_et_al_2020_material_and_methods.xlsx")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

# id <- "Luk_et_al_2020"

names(raw_iso) <- tolower(names(raw_iso))
names(raw_soil) <- tolower(names(raw_soil))

soil <- raw_soil %>% 
  separate(date, c("core_year", "core_month"), sep = "-") %>%
  mutate(study_id = "Luk_et_al_2020_a",
         fraction_carbon = toc/100, # total organic
         core_month = as.numeric(core_month),
         core_year = as.numeric(core_year)) %>%
  mutate(core_day = case_when(core_month == 12 ~ 8,
                              core_month == 7 ~ 16)) %>%
  select(-c(x18, tn, toc, c_n, porosity, water_content, d13c, carbon_density))

isotopes <- raw_iso %>% rename(dry_bulk_density = dbd,
                               location = status) %>%
  mutate(study_id = "Luk_et_al_2020_b",
         core_year = year(as.Date(date, format = "%m/%d/%Y")),
         core_month = month(as.Date(date, format = "%m/%d/%Y")),
         core_day = day(as.Date(date, format = "%m/%d/%Y"))) %>%
  select(-c(date, 
            dry_bulk_density,
            elevation,
            # lat, lon,
            `7be`, `7be_e`))

# elevation_soil <- soil %>% filter(depth_min == 0) 
# elevation_isotope <- isotopes %>% filter(depth_min == 0) # use these elevations because the increment is smaller

# these data arent merging well
depthseries <- full_join(soil, isotopes) %>%
  rename(habitat = location, 
         core_latitude = lat, 
         core_longitude = lon,
         core_elevation = elevation,
         # dry_bulk_density = dbd,
         total_pb210_activity = `210pb`,
         total_pb210_activity_se = `210pb_e`,
         excess_pb210_activity = `210pbex`,
         excess_pb210_activity_se = `210pbex_e`,
         ra226_activity = `226ra`,
         ra226_activity_se = `226ra_e`,
         cs137_activity = `137cs`,
         cs137_activity_se = `137cs_e`) %>%
  mutate(core_id = ifelse(habitat == "POND", str_c(habitat, site_id, sep = "_"),
                          str_c(habitat, site_id, core_id, sep = "_")),
         # sample_volume = pi*(depth_max-depth_min)*(5.5^2),
         core_elevation_notes = "Calculated by subtracting the minimum depth of soil horizon from the NAVD88 elevation of the core location (given at min_depth = 0)",
         pb210_unit = "decaysPerMinutePerGram",
         ra226_unit = "decaysPerMinutePerGram") %>%
  mutate(site_id = str_c("SITE", site_id, sep = "_"))
# Pond surface elevations were calculated as the difference between elevation of the marsh surface on the edge of the pond and pond depth
# pond depths: POND 1 was 0.030 m; POND 2 was 0.026 m; POND 3 was 0.026 m.

# finalize depthseries
final_depthseries <- reorderColumns("depthseries", depthseries) %>%
  select(-c(core_longitude, core_latitude, habitat, contains("core_elevation"),
            core_year, core_month, core_day))

# FIX THIS
# data <- raw_iso %>%
#   rename(habitat = status, 
#          core_latitude = lat, 
#          core_longitude = lon,
#          core_elevation = elevation, # is this elevation for the depth increment?
#          dry_bulk_density = dbd,
#          total_pb210_activity = `210pb`,
#          total_pb210_activity_se = `210pb_e`,
#          excess_pb210_activity = `210pbex`,
#          excess_pb210_activity_se = `210pbex_e`,
#          ra226_activity = `226ra`,
#          ra226_activity_se = `226ra_e`,
#          cs137_activity = `137cs`,
#          cs137_activity_se = `137cs_e`) %>%
#   mutate(study_id = id,
#          core_id = ifelse(habitat == "POND", str_c(habitat, site_id, sep = "_"),
#                           str_c(habitat, site_id, core_id, sep = "_")),
#          core_elevation_notes = "Calculated by subtracting the minimum depth of soil horizon from the NAVD88 elevation of the core location (given at min_depth = 0)",
#          pb210_unit = "decaysPerMinutePerGram",
#          ra226_unit = "decaysPerMinutePerGram",
#          sample_volume = pi*(depth_max-depth_min)*(5.5^2),
#          core_year = year(as.Date(date, format = "%m/%d/%Y")),
#          core_month = month(as.Date(date, format = "%m/%d/%Y")),
#          core_day = day(as.Date(date, format = "%m/%d/%Y"))) %>%
#   mutate(site_id = str_c("SITE", site_id, sep = "_")) %>%
#   select(-date)

# cores
cores <- depthseries %>%
  filter(depth_min == 0) %>%
  select(study_id, site_id, contains("core"), habitat, core_elevation) %>%
  # drop_na(core_elevation) %>%
  mutate(core_position_method = "RTK",
         core_position_accuracy = 0.001,
         core_elevation_method = "RTK",
         core_elevation_datum = "NAVD88",
         vegetation_class = "emergent",
         vegetation_method = "field observation",
         vegetation_notes = "marsh sites dominated by Spartina patens, Spartina alterniflora, and Distichlis spicata",
         inundation_class = ifelse(habitat == "MARSH", "high", "low"),
         inundation_method = "field observation",
         core_notes = "Nine cores were split in half and increments were processed independently") %>%
  select(-habitat)

final_cores <- reorderColumns("cores", cores)


# methods
methods <- raw_methods %>%
  slice(-c(1:2)) %>%
  select_if(function(x) {!all(is.na(x))})

methods <- reorderColumns("methods", methods)

#### Study Citation ####

luk_doi <- "10.5066/P9HIOWKT"
spivak_doi <- "10.26008/1912/bco-dmo.827298.1"
pub_doi <- "10.1029/2020GL090287"

luk_bib <- GetBibEntryWithDOI(luk_doi)
spivak_bib <- GetBibEntryWithDOI(spivak_doi)
pub_bib <- GetBibEntryWithDOI(pub_doi)

# Convert citations to dataframe
pub_citation <- bind_rows(as.data.frame(pub_bib), as.data.frame(pub_bib)) %>%
  mutate(doi = tolower(doi),
         bibliography_id = str_c("Luk_et_al", year, sep = "_"),
         key = str_c("Luk_et_al", year, sep = "_")) %>%
  mutate(study_id = c("Luk_et_al_2020_a", "Luk_et_al_2020_b"))

luk_data_citation <- as.data.frame(luk_bib) %>%
  mutate(study_id = "Luk_et_al_2020_b") %>%
  mutate(doi = tolower(doi),
         bibliography_id = str_c("Luk_et_al", year, sep = "_"),
         key = str_c("Luk_et_al", year, sep = "_"))

spivak_data_citation <- as.data.frame(spivak_bib) %>%
  mutate(study_id = "Luk_et_al_2020_a") %>%
  mutate(doi = tolower(doi),
         bibliography_id = str_c("Spivak", year, sep = "_"),
         key = str_c("Spivak", year, sep = "_"))

# # Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- pub_citation %>%
  bind_rows(spivak_data_citation, luk_data_citation) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Luk_2020/derivative/Luk_et_al_2020.bib")


## QA/QC ###############
library(leaflet)
leaflet(cores) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~as.numeric(core_longitude), lat = ~as.numeric(core_latitude), radius = 5, label = ~site_id)

# Make sure column names are formatted correctly: 
test_colnames("cores", final_cores) 
test_colnames("depthseries", final_depthseries)
test_colnames("methods", methods)

test_varnames(methods)

test_core_relationships(final_cores, final_depthseries)
test_unique_cores(final_depthseries)

## Write derivative data ####
# write_csv(sites, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_sites.csv")
write_csv(final_cores, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_cores.csv")
# write_csv(species, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_methods.csv")
write_csv(final_depthseries, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Luk_2020/derivative/Luk_et_al_2020_study_citations.csv")


