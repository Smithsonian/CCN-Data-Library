## CCRCN Data Library Hook Script ####

# Project: Fate of Coastal Wetland Carbon Under Increasing Sea Level Rise: Using the Subsiding Louisiana Coast as a Proxy for Future World-Wide Sea Level Projections
# Data published by BCO-DMO: https://www.bco-dmo.org/project/670520

# Datasets:

# Carbon mineralization. Contains 9, 1-meter long cores from Barataria Bay, LA coastal wetlands. 
# https://www.bco-dmo.org/dataset/775547
# Chambers, L., Steinmuller, H., Dittmer, K., White, J., Cook, R., Xue, Z. (2019) Barataria Bay carbon mineralization and biogeochemical properties from nine soil cores. 
# Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2019-09-05 [if applicable, indicate subset used]. doi:10.1575/1912/bco-dmo.775547.1 [access date]
# metadata: http://dmoserv3.bco-dmo.org/jg/info/BCO-DMO/Submerged_Wetland_Carbon/carbon_min%7Bdir=dmoserv3.whoi.edu/jg/dir/BCO-DMO/Submerged_Wetland_Carbon/,data=dmoserv3.bco-dmo.org:80/jg/serv/BCO-DMO/Submerged_Wetland_Carbon/carbon_min.html0%7D?

# Core biogeochemical properties, 2018-2019. Contains 11, 2-m long cores in the Barataria Bay, LA coastal wetlands and 3 submerged, 0.5 m cores taken in the adjacent estuary.
# https://www.bco-dmo.org/dataset/833824
# White, J. R., Sapkota, Y., Chambers, L. G., Cook, R. L., Xue, Z. (2020) Biogeochemical properties of sediment cores from Barataria Basin, Louisiana, 2018 and 2019. 
# Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2020-12-16 [if applicable, indicate subset used]. doi:10.26008/1912/bco-dmo.833824.1 [access date]
# metadata: http://dmoserv3.bco-dmo.org/jg/info/BCO-DMO/Submerged_Wetland_Carbon/cores_2018_19%7Bdir=dmoserv3.whoi.edu/jg/dir/BCO-DMO/Submerged_Wetland_Carbon/,data=dmoserv3.bco-dmo.org:80/jg/serv/BCO-DMO/Submerged_Wetland_Carbon/cores_2018_19.html0%7D?


## Prep Workspace ####

# load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
# library(anytime)

source("./scripts/1_data_formatting/qa_functions.R")

# read in data

# couldn't find a csv for the core biogeochemical properties dataset for some reason
# raw_biogeochem <- read_delim("http://dmoserv3.bco-dmo.org/jg/serv/BCO-DMO/Submerged_Wetland_Carbon/cores_2018_19.flat0?", delim = " ")
# biogeochem <- raw_biogeochem %>% mutate_all(trimws) %>% select(-X25)
# names(biogeochem) <- trimws(names(biogeochem))
# write_csv(biogeochem, "./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_833824.csv")

raw_Chambers <- read_csv("./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_775547_712b_5843_9069.csv")
raw_White <- read_csv("./data/primary_studies/Chambers_et_al_2019/original/bcodmo_dataset_833824.csv", na = "nd")
raw_methods <- read_xlsx("./data/primary_studies/Chambers_et_al_2019/intermediate/Chambers_White_2020_material_and_methods.xlsx")

guidance <- read_csv("docs/ccrcn_database_structure.csv")

## Trim Data to Library ####

id <- "Chambers_and_White_2020" # or should it be White_et_al_2020?

# methods
methods <- raw_methods %>%
  slice(-c(1:2)) %>%
  select_if(function(x) {!all(is.na(x))}) %>%
  rename(method_id = study_id) %>%
  mutate(study_id = id)

methods <- reorderColumns("methods", methods)

# depthseries
# sample_vol <- pi*(3.8^2)*10 # for the 2m cores (White_et_al_2020_a)
# sample_vol <- 384.85 # cubic cm

names(raw_Chambers) <- gsub("_field", "", names(raw_Chambers))

chambers_data <- raw_Chambers %>%
  slice(-1) %>%
  rename(core_longitude = longitude,
         core_latitude = latitude,
         depth = depth2) %>%
  mutate_at(vars(-depth, -site_id), as.numeric) %>%
  mutate(method_id = "Chambers_et_al_2019")

white_data <- raw_White %>%
  rename(core_longitude = lon,
         core_latitude = lat,
         site_id = Site_id) %>%
  mutate(method_id = ifelse(site_id == "Estuary", "White_et_al_2020_b", "White_et_al_2020_a"))

# join tables together and curate depthseries
join_depthseries <- bind_rows(chambers_data, white_data) %>%
  rename(dry_bulk_density = bulk_density_g_cm_3) %>%
  mutate(core_id = str_c(site_id, replicate, sep = "_"),
         study_id = id,
         # calculate fraction total carbon (make sure all the values are included in the computation)
         # make sure these values arent modeled
         fraction_carbon = carbon_density_g_cm_3/dry_bulk_density, 
         fraction_organic_matter = as.numeric(pcnt_organic_matter)/100) %>%
  separate(col = depth, into = c("depth_min", "depth_max"), sep = "-") %>%
  select(-c(contains("aerob"), replicate, ph, pcnt_organic_matter, moisture_content_pcnt,
            ap, bg, xy, nag, cb, contains("extractable"), contains("mineralizable"), labile_c_g_kg,
            refractory_c_g_kg, total_ip_mg_kg, total_p_mg_kg, total_n_g_kg, microbial_biomass_c,
            total_n_g_cm_3, total_op_mg_kg))

# keep extractable dissolved organic carbon?

depthseries <- reorderColumns("depthseries", join_depthseries) %>%
  select(-c(core_latitude, core_longitude, sampling_date, 
            total_c_g_kg, total_c_g_cm_3, carbon_density_g_cm_3))

# cores
cores <- join_depthseries %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, sampling_date) %>%
  distinct() %>%
  mutate(core_year = year(as.Date(sampling_date)),
         core_month = month(as.Date(sampling_date)),
         core_day = day(as.Date(sampling_date)),
         core_position_method = "handheld",
         core_elevation_notes = ifelse(site_id == "Estuary", "During sampling, the depth of the estuary was 1 m relative to the marsh surface", NA),
         salinity_class = "brackish",
         salinity_notes = "salinity ranged from 14 to 20 ppt",
         vegetation_class = "emergent",
         vegetation_notes = "Spartina alterniflora was the dominant vegetation",
         # inundation_notes = "no inundation of marsh while sampling",
         core_length_flag = "core depth limited by length of corer") %>%
  mutate(core_year = ifelse(is.na(core_year), 2017, core_year)) %>%
  select(-sampling_date)

cores <- reorderColumns("cores", cores)

#### Study Citation ####

if(!file.exists("./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_study_citations.csv")){
  # associated_bib_doi <- ""
  data_release_doi_2019 <- "10.1575/1912/bco-dmo.775547.1"
  data_release_doi_2020 <- "10.26008/1912/bco-dmo.833824.1"
  
  # paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
  data_bib_raw_2019 <- GetBibEntryWithDOI(data_release_doi_2019)
  data_bib_raw_2020 <- GetBibEntryWithDOI(data_release_doi_2020)
  
  # Convert citations to dataframe
  study_citations_2019 <- as.data.frame(data_bib_raw_2019) %>%
    mutate(study_id = "Chambers_et_al_2019") %>%
    mutate(bibliography_id = str_c("Chambers_et_al", year, "data", sep = "_"))
  
  study_citations_2020 <- bind_rows(as.data.frame(data_bib_raw_2020), as.data.frame(data_bib_raw_2020)) %>%
    mutate(study_id = c("White_et_al_2020_a", "White_et_al_2020_b"),
           bibliography_id = str_c("White_et_al", year, "data", sep = "_"))
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <-  bind_rows(study_citations_2019, study_citations_2020) %>%
    mutate(publication_type = "primary dataset") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # make corrections for new methods IDs
  # raw_citations <- read_csv("data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_study_citations.csv")
  # study_citations <- raw_citations %>% mutate(study_id = id) %>% distinct()
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>% 
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019.bib")
  write_csv(study_citations, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries")

updated <- updateTables(table_names)

# save listed tables to objects
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores

## QA/QC ###############

leaflet(cores) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~as.numeric(core_longitude), lat = ~as.numeric(core_latitude), 
                   radius = 5, label = ~core_id)


# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)


## Write derivative data ####
write_csv(cores, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_cores.csv")
write_csv(methods, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_methods.csv")
write_csv(depthseries, "./data/primary_studies/Chambers_et_al_2019/derivative/Chambers_et_al_2019_depthseries.csv")


