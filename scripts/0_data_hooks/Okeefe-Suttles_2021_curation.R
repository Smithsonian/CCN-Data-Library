## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for data releases
# FL: https://www.sciencebase.gov/catalog/item/60bfb8a4d34e86b938916d6f
# RI: https://www.sciencebase.gov/catalog/item/60bfb7c2d34e86b938916d1e
# MA: https://www.sciencebase.gov/catalog/item/60bfb987d34e86b938916dc9
# MA: https://www.sciencebase.gov/catalog/item/60bfb916d34e86b938916da1

# load necessary libraries
library(tidyverse)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_FL <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/original/FL/Data_TampaBay_Cores.csv")
fl_coreinfo <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/intermediate/FL_coreinfo.csv")
raw_RI <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/original/RI/Data_RhodeIsland_Cores.csv")
raw_MA1 <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/original/MA_Cape/Data_RestoredMarshes_Cores.csv")
raw_MA2 <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/original/MA_Wellfleet/Data_HerringRiver_Cores.csv")
raw_methods <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/intermediate/OkeefeSuttle_2021_material_and_methods.csv")
raw_citations <- read_csv("data/primary_studies/Okeefe-Suttles_et_al_2021/intermediate/Okeefe-Suttle_2021_release_dois.csv")

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

## ... Methods ####

# curate materials and methods
methods <- raw_methods %>%
  drop_na(study_id) %>% 
  select_if(function(x) {!all(is.na(x))})
# still in progress

## ... Core Depthseries ####

FL <- raw_FL %>% mutate(study_id = "Okeefe-Suttles_et_al_2021_FL",
                        core_id = paste0("FL_", ID))
MA1 <- raw_MA1 %>% mutate(study_id = "Okeefe-Suttles_et_al_2021_Cape",
                          core_id = paste0("MA_", ID))
MA2 <- raw_MA2 %>% mutate(study_id = "Okeefe-Suttles_et_al_2021_Wellfleet",
                          core_id = paste0("MA_", ID))
RI <- raw_RI %>% mutate(study_id = "Okeefe-Suttles_et_al_2021_RI",
                        core_id = paste0("RI_", ID))

# bind all the depthseries together
suttle_ds <- bind_rows(FL, MA1, MA2, RI) %>%
  mutate(year = year(as.Date(Date, format = "%m/%d/%Y")),
         month = month(as.Date(Date, format = "%m/%d/%Y")),
         day = day(as.Date(Date, format = "%m/%d/%Y")),
         Depth_mid = coalesce(Depth_mid, Depth)) %>% 
  rename(longitude = Lon, latitude = Lat,
         site_id = Site, 
         dry_bulk_density = DBD,
         delta_c13 = `13C`,
         total_pb210_activity = `210Pb`, 
         total_pb210_activity_se = `210Pb_e`,
         excess_pb210_activity = `210Pbex`, 
         excess_pb210_activity_se = `210Pbex_e`,
         cs137_activity = `137Cs`,
         cs137_activity_se = `137Cs_e`,
         ra226_activity = `226Ra`,
         ra226_activity_se = `226Ra_e`,
         be7_activity = `7Be`,
         be7_activity_se = `7Be_e`) %>% 
  mutate(fraction_carbon = wtC/100,
         fraction_organic_matter = LOI/100,
         depth_min = ifelse(Depth_mid %% 1 == 0.5, Depth_mid - 0.5, Depth_mid - 1),
         depth_max = ifelse(Depth_mid %% 1 == 0.5, Depth_mid + 0.5, Depth_mid + 1)) %>% 
  select(-Age)

# Should we leave these out? since they're derivative calculations?
# VAR = vertical accretion rate
# MAR = mass accumulation rate (DBD * VAR)
# CAR = carbon accumulation rate

depthseries <- suttle_ds %>% 
  filter(Depth_mid != 0) %>% 
  rename(age = Year, age_se = Age_e) %>% 
  # assign method IDs
  # mutate(method_id = case_when(study_id == "Okeefe-Suttles_et_al_2021_Cape" & is.na(delta_c13) ~ "organic_carbon",
  #                              study_id == "Okeefe-Suttles_et_al_2021_Cape" & !is.na(delta_c13) ~ "total_carbon",
  #                              # conflict, there are cases where fraction carbon was not measured...do we need a third methods ID for Cape cores?
  #                              TRUE ~ "single set of methods")) %>% 
  # filter(study_id == "Okeefe-Suttles_et_al_2021_Cape") %>% 
  reorderColumns("depthseries", .) %>% 
  select(-c(Depth, Depth_mid, wtC, wtN, Date, ID, `15N`, Status, latitude, longitude, 
            LOI, year, month, day, Elevation, Year_restored))
# create method_id lookup to merge to depthseries

ggplot(depthseries) +
  geom_point(aes(dry_bulk_density, fraction_carbon, col = study_id)) +
  # geom_point(aes(depth_min, fraction_carbon)) +
  facet_wrap(~study_id) +
  theme_bw() +
  theme(legend.position = "none")


## ... Core-Level ####

# curate core-level data
suttle_cores <- suttle_ds %>%
  group_by(study_id, site_id, core_id, year, month, day, latitude, longitude) %>% 
  mutate(elevation = max(Elevation)/100) %>% # no elevation for gansett 
  select(study_id, site_id, core_id, year, month, day, latitude, longitude, 
         Status, Year_restored, elevation) %>% 
  distinct() %>% 
  # finish classifying the following
  mutate(habitat = case_when(Status == "Mangrove" ~ "mangrove",
                             Status == "Young Mangrove" ~ "mangrove",
                             Status == "Salt Barren" ~ "unvegetated",
                             Status == "Phragmites" ~ "marsh",
                             Status == "Juncus Marsh" ~ "marsh",
                             Status == "Typha" ~ "marsh",
                             Status == "Salt marsh" ~ "marsh",
                             Status == "Wet Shrub" ~ "scrub shrub",
                             Status == "Forest" ~ "upland",
                             study_id == "Okeefe-Suttles_et_al_2021_Cape" ~ "marsh",
                             TRUE ~ NA_character_),
         vegetation_class = case_when(habitat == "marsh" ~ "emergent",
                                      habitat == "unvegetated" ~ "unvegetated",
                                      Status == "Forest" | habitat == "mangrove" ~ "forested",
                                      Status == "Wet Shrub" ~ "scrub shrub",
                                      TRUE ~ NA_character_)) %>% 
  ungroup()


# lot of study-specific core-level info
# cores need to be separately curated before re-synthesizing

fl_cores <- suttle_cores %>% 
  filter(study_id == "Okeefe-Suttles_et_al_2021_FL") %>% 
  select(-Year_restored, -Status) %>% 
  left_join(fl_coreinfo) %>% # merge positional and elevation accuracy
  mutate(position_method = ifelse(position_accuracy < 1, 
                                  "other high resolution", "other moderate resolution"),
         elevation_method = ifelse(position_accuracy < 0.5, 
                                  "other high resolution", "other low resolution"),
         elevation_datum = "NAVD88",
         position_notes = "Differential Global Positioning System (DGPS)", 
         elevation_notes = "Differential Global Positioning System (DGPS)", 
         core_notes = ifelse(habitat == "unvegetated", 
                             "not possible to take a sediment core so a section of sediment was removed with a shovel", NA),
         core_length_flag = ifelse(site_id == "E.G. Simmons" | site_id == "Fort de Soto",
                                   "core depth represents deposit depth",
                                   "core depth limited by length of corer")) 


ri_cores <- suttle_cores %>% 
  filter(study_id == "Okeefe-Suttles_et_al_2021_RI") %>% 
  select(-Year_restored) %>% 
  mutate(core_length_flag = "core depth limited by length of corer",
         position_method = "handheld",
         position_accuracy = 3)

ma1_cores <- suttle_cores %>% 
  filter(study_id == "Okeefe-Suttles_et_al_2021_Cape") %>% 
  mutate(core_length_flag = "core depth limited by length of corer",
         position_method = "handheld",
         position_accuracy = 3,
         position_notes = "Garmin GPSMAP 76Cx unit",
         elevation_method = "RTK",
         elevation_accuracy = 0.05,
         elevation_datum = "NAVD88",
         core_notes = ifelse(!is.na(Year_restored), paste0("sampling site restored in ", Year_restored), NA)) %>% 
  select(-Year_restored) 
# assigning methodsID for ma1 cores: Sediments in this sample set that were analyzed for both carbon content and 
# isotopic carbon signature were not fumed prior to analysis and reported wtC is total carbon. 
# Sediment sections without a reported isotopic signature were run in a separate lab and were fumed to remove inorganic carbon prior to analysis.

ma2_cores <- suttle_cores %>% 
  filter(study_id == "Okeefe-Suttles_et_al_2021_Wellfleet") %>% 
  mutate(core_length_flag = "core depth limited by length of corer",
         position_method = "handheld",
         position_notes = "Garmin GPSMAP 76Cx unit",
         position_accuracy = 3,
         elevation_method = "RTK",
         elevation_accuracy = 0.05,
         elevation_datum = "NAVD88") %>% 
  select(-Year_restored) 

# bind cores back together
cores <- bind_rows(fl_cores, ri_cores, ma1_cores, ma2_cores) %>% 
  reorderColumns('cores', .) %>% 
  select(-Status)

## ... Impacts ####

sort(unique(suttle_cores$Status)) 
# mix of species and impacts, checking xml metadata for more info

impacts <- suttle_ds %>%
  select(study_id, site_id, core_id, Status) %>% 
  distinct() %>%
  mutate(impact_class = case_when(Status == "Natural" ~ "natural",
                                  Status == "Restored" ~ "tidally restored",
                                  site_id == "Herring River" ~ "tidally restricted",
                                  site_id == "E.G. Simmons" | site_id == "Fort de Soto" ~ "restored",
                                  TRUE ~ NA_character_))

# mayo creek might be natural or tidally restricted
# no impacts indicated for RI cores
# no impacts indicated for most of the FL cores

## ... Species ####

species <- suttle_ds %>%
  select(study_id, site_id, core_id, Status) %>% 
  distinct() %>%
  mutate(species = case_when(Status == "Mangrove" ~ "Rhizophora mangle; Laguncularia racemosa; Avicennia germinans",
                             # Status == "Young Mangrove" ~ "mangrove",
                             Status == "Phragmites" ~ "Phragmites spp.",
                             Status == "Juncus Marsh" ~ "Juncus spp.; Spartina alterniflora",
                             Status == "Typha" ~ "Typha spp.",
                             TRUE ~ NA_character_),
         species = strsplit(species, split = "; ")) %>% 
  unnest(species)


## QAQC ####

library(leaflet)

leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

# Check col and varnames
table_names <- c("methods", "cores", "depthseries", "impacts", "species")

testTableCols(table_names)
testTableVars(table_names) # methods and cores
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI
library(RefManageR)

# consider this a synthesis? 

# if(!file.exists("data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_study_citations.csv")){
# Create bibtex file
dois <- c(raw_citations$doi)

data_bibs <- GetBibEntryWithDOI(dois)

study_citations <- as.data.frame(data_bibs) %>%
  mutate(study_id = id,
         bibliography_id = "Okeefe-Suttles_et_al_2021_data",
         publication_type = "primary dataset") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021.bib")
# write_csv(study_citations, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_study_citations.csv")
# }

## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
# write_csv(cores, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_cores.csv") 
# write_csv(depthseries, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_depthseries.csv")
# write_csv(methods, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_methods.csv")
# write_csv(sites, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_sites.csv")
# write_csv(species, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_species.csv")
# write_csv(impacts, "data/primary_studies/Okeefe-Suttles_et_al_2021/derivative/Okeefe-Suttles_et_al_2021_impacts.csv")


