## CCN Data Library ########

## Soil core data curation script for PNW BCWG Database
## contact: Jaxine Wolfe, wolfejax@si.edu

## supplementary info (may be useful)
# https://agupubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1029%2F2024GB008239&file=2024GB008239-sup-0001-Supporting+Information+SI-S01.pdf

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(sf)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCRCN-Community-Resources/soil_carbon_guidance.html

# Data tables
pnw_cores <- read_csv("data/primary_studies/Janousek_et_al_2025/original/Janousek_et_al_2025_cores.csv")
pnw_ds <- read_csv("data/primary_studies/Janousek_et_al_2025/original/Janousek_et_al_2025_depthseries.csv")
pnw_sites <- read_csv("data/primary_studies/Janousek_et_al_2025/original/Janousek_et_al_2025_estuaries.csv")
pnw_sources <- read_csv("data/primary_studies/Janousek_et_al_2025/original/Janousek_et_al_2025_sources.csv")

# Metadata
metadata <- read_csv("data/primary_studies/Janousek_et_al_2025/original/Janousek_et_al_2025_metadata.csv")

factors <- metadata %>% 
  filter(data_type == "factor") %>% 
  mutate(unit_or_code = str_split(unit_or_code, "; ")) %>% 
  unnest(unit_or_code) %>% 
  separate(unit_or_code, into = c("code", "code_definition"), sep = " = ")

## Identify Duplicates ####

ccn_cores <- read_csv("data/CCN_synthesis/CCN_cores.csv") %>% 
  filter(country %in% c("United States", "Canada", "Mexico")) %>% 
  filter(longitude < -88) %>% 
  select(-c(position_notes:pb210_cic_r2))

ccn_bib <- read_csv("data/CCN_synthesis/CCN_study_citations.csv", guess_max = 1200) %>% 
  filter(study_id %in% unique(ccn_cores$study_id))

# try a spatial join to isolate duplicate samples
ccn_cores_sf <- ccn_cores %>% 
  # filter(admin_division == "California") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

pnw_cores_sf <- pnw_cores %>% 
  rename(latitude = Lat, longitude = Long) %>% 
  # filter(State == "CA") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

nearest <- st_nearest_feature(pnw_cores_sf, ccn_cores_sf)

distances <- pnw_cores_sf %>% # need to add source information and plot the resulting pairs to see if they actually align
  select(StudyID:Estuary) %>% 
  mutate(sample_dist = as.vector(st_distance(pnw_cores_sf, ccn_cores_sf[nearest,], by_element = T))) %>% 
  bind_cols(ccn_cores_sf[nearest,])

# filter coords pairs that are less than X meter off and pull IDs
duplicates <- distances %>% 
  filter(sample_dist < 2) %>%
  distinct(StudyID) %>% 
  pull(StudyID)

## 1. Curate Data ####

## ... Studies/Sources ####

# curate materials and studies table
studies <- pnw_sources %>% 
  # filter(!(StudyID %in% duplicates)) %>% 
  mutate(study_id = word(`Citation (or dataset description)`, 1), 
         year = gsub("([0-9]+).*$", "\\1", `Citation (or dataset description)`),
         year = as.numeric(word(year, -1)),
         year = ifelse(is.na(year), 2025, year),
         study_id = trimws(paste0(study_id, "_et_al_", year)),
         study_id = ifelse(grepl(",", study_id), gsub(",", "", study_id), study_id),
         # study ID corrections for better alignment
         study_id = recode(study_id,
                           "Sanborn_et_al_2020" = "Sanborn_and_Coxson_2020",
                           "Darienzo_et_al_1990" = "Darienzo_and_Peterson_1990",
                           "Schile-Beers_et_al_2023" = "Beers_et_al_2023",
                           "Nahlik_et_al_2016" = "Nahlik_and_Fennessy_2016",
                           "Lutz_et_al_2018" = "Lutz_2018",
                           "Hodgson_et_al_2016" = "Hodgson_and_Spooner_2016",
                           "Thom_et_al_1992" = "Thom_1992",
                           "Poppe_et_al_2018" = "Poppe_and_Rybczyk_2018",
                           "Poppe_et_al_2021" = "Poppe_and_Rybczyk_2021",
                           "Kairis_et_al_2010" = "Kairis_and_Rybczyk_2010",
                           "Watson_et_al_2013" = "Watson_and_Byrne_2013",
                           "Stephens_et_al_2018" = "Stephens_and_Eckert_2018",
                           "Costa_et_al_2019" = "Costa_2019",
                           "Adame_et_al_2025" = "Adame_and_Najera_2025",
                           "Diefenderfer_et_al_2025" = "Diefenderfer_and_Borde_2025",
                           "CEC_et_al_2015" = "CEC_2015",
                           "Blount_et_al_2017" = "Blount_2017",
                           "Watson_et_al_2018" = "Watson_and_Corona_2018",
                           "Patrick_et_al_1990" = "Patrick_and_DeLaune_1990",
                           "Juang_et_al_2017" = "Juang_2017",
                           "Siegert_et_al_2025" = "Siegert_and_Apple_2025"),
         spatial_match = ifelse(StudyID %in% duplicates, T, F),
         # determine overlap with the CCN bib
         id_match = ifelse(study_id %in% unique(ccn_bib$study_id), T, F)) %>% 
  select(study_id, spatial_match, id_match, everything()) %>% 
  arrange(spatial_match, id_match)

# send this ranking to Chris
for_chris <- studies %>% select(study_id:`Citation (or dataset description)`, `Link to study`, `Link to data`)
# write_excel_csv(for_chris, "data/primary_studies/Janousek_et_al_2025/core_duplication_review.csv")

# studies to keep 
keep <- studies %>% 
  filter(spatial_match == F & id_match == F | study_id %in% c("Adame_et_al_2015", "Adame_et_al_2021")) %>% 
  pull(StudyID)

dont_keep <- studies %>% 
  filter(!(spatial_match == F & id_match == F)) %>% 
  pull(StudyID)

## ... Methods ####

# This needs more work
methods <- studies %>% 
  filter(StudyID %in% keep) %>% 
  rename(coring_method = `Coring method(s)`,
         compaction_flag = `Core compaction`,
         roots_flag = `Core root treatment`,
         loss_on_ignition_time = `LOI duration`,
         loss_on_ignition_temperature = `LOI temperature`,
         carbonate_removal_method = `Inorganic carbon method`
         ) %>% 
  mutate(fraction_carbon_method = case_when(`CHN analyzer` == "Not specified" ~ NA,
                                            !is.na(`CHN analyzer`) ~ "EA", T ~ NA),
         compaction_flag = case_when(grepl("Measured|%|>", compaction_flag) ~ "compaction quantified",
                                     grepl("Minimal|<", compaction_flag) ~ "compaction qualified",
                                     grepl("None|Negligible|seen", compaction_flag) ~ "no obvious compaction",
                                     compaction_flag == "Not detailed" ~ "not specified",
                                     T ~ compaction_flag),
         sediment_sieve_size = ifelse(roots_flag == "Roots and wood removed (2 mm seive)", 2, NA),
         sediment_sieved_flag = ifelse(is.na(sediment_sieve_size), "sediment not sieved", "sediment sieved"),
         roots_flag = case_when(roots_flag %in% c("Rhizomes and roots removed", "Roots and wood removed (2 mm seive)",
                                                  "Roots removed. Also any visible living biomass removed") ~ "roots and rhizomes separated",
                                roots_flag %in% c("Not removed", "Roots not removed") ~ "roots and rhizome included", 
                                T ~ NA),
         # corer_diameter = # we can include these another time
         coring_method = case_when(coring_method %in% c("PCV corer", "PVC tube", "Polycarbonate tube", 
                                                        "10 cm PVC tube", "PVC tube, 10 cm diam") ~ "pvc and hammer", 
                                   coring_method %in% c("Russian peat borer") ~ "russian corer",
                                   coring_method %in% c("Gouge auger, 6.4 cm radius") ~ "gouge auger",
                                   is.na(coring_method) ~ "none specified",
                                    T ~ coring_method),
         # carbonates_removed,
         carbonate_removal_method = case_when(grepl("HCl", carbonate_removal_method)~ "direct acid treatment", 
                                              carbonate_removal_method %in% c("Not measured", "Unknown") ~ NA,
                                              T ~ carbonate_removal_method)) %>% 
  select(study_id, coring_method, everything()) %>% 
  select(-c("Link to study","Link to data" , year, spatial_match, id_match,
            `Citation (or dataset description)`))
  
unique(methods$roots_flag)
unique(methods$coring_method)
unique(methods$carbonate_removal_method)

## ... Cores ####

# create lookup tables from metadata
veg_grp <- factors %>% 
  filter(attribute_name == "VegGrp") %>% 
  select(code, code_definition) %>% 
  rename("VegGrp" = code,
         vegetation_notes = code_definition) 

ecosystem <- factors %>% 
  filter(attribute_name == "Ecosystem") %>% 
  select(code, code_definition) %>% 
  rename("Ecosystem" = code,
         habitat = code_definition) %>% 
  mutate(habitat = tolower(recode(habitat, 
                                  "Emergent marsh" = "marsh",
                                  "Tidal swamp" = "swamp",
                                  "Tideflat" = "mudflat")))

estuary_class <- factors %>% 
  filter(attribute_name == "EstType") %>% 
  select(code, code_definition) %>% 
  rename("EstType" = code,
         inundation_notes = code_definition) 

# curate core-level data table
clean_cores <- pnw_cores %>% 
  left_join(ecosystem) %>% left_join(veg_grp) %>% left_join(estuary_class) %>% 
  # left_join(sites %>% select(Estuary, site_id)) %>% 
  mutate(StudyID = recode(StudyID, "36/37" = "37")) %>% 
  filter(StudyID %in% keep) %>% 
  mutate(StudyID = as.numeric(StudyID)) %>% 
  left_join(studies %>% distinct(StudyID, study_id)) %>%
  rename(latitude = Lat, longitude = Long, 
         site_id = Estuary) %>% 
  mutate(core_id = paste(site_id, SampID, sep = "-"),
         position_method = case_when(LatLongType == "SPEC" ~ "other high resolution", # or moderate? we don't know what "specific" means 
                                    LatLongType == "GEN" ~ "other low resolution"),
         position_notes = case_when(LatLongType == "SPEC" ~ "specific coordinates", 
                                    LatLongType == "GEN" ~ "general coordiantes"),
         salinity_class = case_when(grepl("Brackish", vegetation_notes) ~ "brackish",
                                    grepl("Freshwater", vegetation_notes) ~ "fresh",
                                    grepl("Saline", vegetation_notes) ~ "saline",
                                    T ~ NA),
         year = case_when(study_id == "Adame_et_al_2015" ~ "2012",
                          study_id == "Adame_et_al_2021" ~ "2018", T ~ NA),
         month = case_when(study_id == "Adame_et_al_2015" ~ "3", T ~ NA),
         day = case_when(study_id == "Adame_et_al_2015" ~ "7", T ~ NA),
         core_length_flag = "core depth limited by length of corer") %>% 
  select(-c(contains("Stk"), PercFines, GrainType, VegGrp, Ecosystem, EstType, LatLongType,
            KGzone, Cdepth, Lvl1EcoReg, State, StudyID)) %>% 
  select(study_id, site_id, core_id, everything())
# what to do about wetland elevation? Its in terms of z-star?
# we need to track down years of sampling

cores <- clean_cores %>% select(-c(SampID, contains("Elev")))

# Map the cores with the CCN ones
leaflet() %>%
  addTiles() %>% 
  # addCircleMarkers(data = cores %>% filter(study_id =="Prentice_et_al_2019"),
  #                  lng = ~longitude, lat = ~latitude, radius = 1, color = "red",
  #                  label = ~paste(study_id, habitat, sep = "; "))

  addCircleMarkers(data = ccn_cores %>% filter(study_id == "Adame_et_al_2013"), 
                   lng = ~longitude, lat = ~latitude, radius = 1,
                   label = ~paste(study_id, habitat, sep = "; ")) 
  addCircleMarkers(data = pnw_cores %>% filter(StudyID == "49"), 
                   color = "red", radius = 1
                   # label = studies$study_id[which(studies$StudyID == dont_keep[5])]
  )

# Thorne 2015: CCN has 24 cores and Chris had 16
# Let's keep the Adame et al 2015

# View(pnw_cores %>% filter(StudyID == dont_keep[10]))
# nrow(ccn_cores %>% filter(study_id == "Adame_et_al_2021"))

## ... Depthseries ####

core_ids <- clean_cores %>% distinct(study_id, site_id, core_id, SampID)

# curate core depthseries data table
depthseries <- pnw_ds %>% 
  mutate(StudyID = as.numeric(recode(StudyID, "36/37" = "37", "49/71" = "49"))) %>% 
  filter(StudyID %in% keep) %>% 
  rename(dry_bulk_density = BD, 
         sample_id = SubSampID,
         depth_interval_notes = Notes) %>% 
  # NA is a SampInterval ">60" which is 60-100 in the ExtrapInterval
  separate(SampInterval, into = c("depth_min", "depth_max"), sep = "-") %>%
  separate(ExtrapInterval, into = c("representative_depth_min", "representative_depth_max"), sep = "-") %>%
  # drop extrapolated and interpolated data
  mutate(method_id = "no documented methods",
         dry_bulk_density = case_when(BD_type %in% c("I", "E") ~ NA, T ~ dry_bulk_density),
         depth_min = gsub("~", "", depth_min),
         PercC = case_when(C_type %in% c("I", "E") ~ NA, 
                           # is.na(C_type) ~ NA, # Krause_et_al_2022 values look modeled, but also maybe not
                           T ~ PercC), 
         PercOM = case_when(OM_type %in% c("I", "E") ~ NA, T ~ PercOM), 
         fraction_organic_matter = PercOM/100,
         fraction_carbon = PercC/100) %>% 
  # remove rows where all of the specified columns are NA
  # filter(if_any(c(fraction_carbon, fraction_organic_matter, dry_bulk_density), ~ !is.na(.))) %>% 
  left_join(studies %>% distinct(StudyID, study_id)) %>%
  left_join(core_ids) %>%
  select(-c(Ecosystem, PercOM, PercC, Calc_CD_C, Calc_CD_OM,  StudySampID,
            SampID, StudyID,
            SampLength, contains("_type"), "C method")) %>% 
  select(study_id, site_id, core_id, everything()) %>% select(-depth_interval_notes, depth_interval_notes)

ggplot(depthseries %>% left_join(cores %>% distinct(study_id, core_id, habitat))) +
  geom_point(aes(dry_bulk_density, fraction_carbon, col = habitat), pch = 1, alpha = 0.2) +
  # geom_point(aes(fraction_organic_matter, fraction_carbon, col = habitat)) + 
  facet_wrap(~habitat)

## ... Species ####

# if provided at the site or core-level, curate taxa table
# Note: this information may need to be isolated from another table
species <- clean_cores %>%
  distinct(study_id, core_id, vegetation_notes) %>% 
  mutate(species_code = case_when(grepl("graminoid|marsh|mangrove|species", vegetation_notes) ~ NA,
                             T ~ vegetation_notes), 
         code_type = "Genus species")  %>% 
    drop_na(species_code) %>% select(-vegetation_notes)

## ... Impacts ####

# if provided, curation table of anthropogenic impacts


## 2. QAQC ####

## Mapping

## Table testing
table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

# test numeric attribute ranges
fractionNotPercent(depthseries)
testNumericCols(depthseries)

## 3. Write Curated Data ####

# write data to final folder
# write_excel_csv(methods, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_studies.csv")
write_excel_csv(cores, "data/primary_studies/Janousek_et_al_2025/derivative/Janousek_et_al_2025_cores.csv")
write_excel_csv(depthseries, "data/primary_studies/Janousek_et_al_2025/derivative/Janousek_et_al_2025_depthseries.csv")
write_excel_csv(species, "data/primary_studies/Janousek_et_al_2025/derivative/Janousek_et_al_2025_species.csv")

## 4. Bibliography ####

# There are three ways to approach this:
# 1) download the article citation directly to the study's folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

rough_citations <- studies %>% 
  filter(StudyID %in% keep) %>% 
  select(study_id,`Citation (or dataset description)`, `Link to study`, `Link to data`, year) %>% 
  rename(author = `Citation (or dataset description)`) %>% 
  pivot_longer(cols = -c(study_id, author, year), names_to = "bibtype", values_to = "url", values_drop_na = F) %>% 
  mutate(bibtype = recode(bibtype, "Link to study" = "Article", "Link to data" = "Misc"),
         year = as.character(year)) %>% 
  arrange(study_id)
# write_excel_csv(rough_citations, "data/primary_studies/Janousek_et_al_2025/new_study_citations.csv")
# rough citations needs cleaning

data_bib <- rough_citations %>% select(study_id) %>% 
  bind_cols(as.data.frame(GetBibEntryWithDOI("10.25573/serc.28127486")) %>% remove_rownames()) %>% 
  mutate(bibliography_id = "Janousek_et_al_2025_synthesis",
         publication_type = "synthesis source")

# xia_synth <- data.frame(study_id = c("Gao_et_al_2016", "Liu_et_al_2017", "Xia_et_al_2022")) %>% 
#   bind_cols(as.data.frame(GetBibEntryWithDOI("10.1111/gcb.16325")) %>% remove_rownames()) %>% 
#   mutate(bibliography_id = "Xia_et_al_2022_synthesis",
#          publication_type = "associated source")

# combine all citations 
synthesis_citations <- bind_rows(rough_citations, data_bib) %>% 
  mutate(bibliography_id = case_when(!is.na(bibliography_id) ~ bibliography_id,
                                     bibtype == "Misc" ~ paste0(study_id, "_data"),
                                     bibtype == "Article" ~ paste0(study_id, "_article")),
         publication_type = case_when(!is.na(publication_type) ~ publication_type,
                                      bibtype == "Article" ~ "associated source", 
                                      bibtype == "Misc" ~ "primary dataset")) %>% 
  # bind_rows(xia_synth) %>% 
  arrange(study_id) %>% 
  select(-c(keywords, copyright)) %>%  # editor, language
  select(study_id, bibliography_id, publication_type, everything())

# unique(dat$study_id)[!(unique(dat$study_id) %in% unique(synthesis_citations$study_id))]

write_excel_csv(synthesis_citations, "data/primary_studies/Janousek_et_al_2025/derivative/Janousek_et_al_2025_study_citations.csv")

# Create .bib file
# this is more of a test that bib IDs are unique
bib_file <- synthesis_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")
# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
