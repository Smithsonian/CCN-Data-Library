# Coastal Carbon Research Coordination Network
# This script prepares Sanderman et al 2018 data for disply in map and for
#   download by user
# Contact: klingesd@si.edu

## Workspace prep ##########

library(tidyverse)
# library(RCurl)
library(readxl)
library(lubridate)
library(RefManageR)
library(parzer)

## Curate Sanderman international data ############

# This excel document was handed off in a personal communication. It corresponds
#   to some of the data held here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OCYUIT

# This sheet ("horizon data") mirrrs much of the content that can be downloaded here:
#"https://raw.githubusercontent.com/whrc/Mangrove-Soil-Carbon/master/depth_model_comparison/soil_profiles.csv"

internatl_depthseries_data_raw <- read_excel("data/primary_studies/Sanderman_2018/original/WHRC-TNC mangrove soc database 100718.xlsx", 
                                             sheet = 4)

# Another sheet on the same excel document
internatl_core_data_raw <- read_excel("data/primary_studies/Sanderman_2018/original/WHRC-TNC mangrove soc database 100718.xlsx",
                                      sheet = 3)

#m Other core level data found here (but not necessary):
# "https://raw.githubusercontent.com/whrc/Mangrove-Soil-Carbon/master/R_code/41558_2018_162_MOESM2_ESM.csv"

# Jon sent me some updated data as a few sites were inaccurate.
# We'll need to insert in the new data for these sites
internatl_depthseries_data_new <- read_excel("data/primary_studies/Sanderman_2018/original/RC_sites.xlsx")

# Delete sites with incorrect data
internatl_depthseries_data_raw <- internatl_depthseries_data_raw %>%
  filter(`Site #` != "M0018" & `Site #` != "M0019" & `Site #` != "M0020" & 
           `Site #` != "M0021") %>%
  bind_rows(internatl_depthseries_data_new)


# The below code demonstrates evidence for the issue raised in 
# https://github.com/Smithsonian/CCRCN-Data-Library/issues/4
# Can be ignored for now
soil_profiles <- read.csv("data/primary_studies/Sanderman_2018/original/soil_profiles.csv")
  
soil_profiles <- soil_profiles %>%
  rename(site_name = Site.name)

int_depth_data_test <- internatl_depthseries_data_raw %>%
  rename(site_name = `Site name`) 

absent_cores <- soil_profiles %>%
  anti_join(int_depth_data_test, by = "site_name")

## * International study level metadata ################
internatl_study_metadata <- internatl_core_data_raw %>%
  rename(email = "Data_owner") %>%
  rename(study_id = Source) %>%
  mutate(study_id = gsub(" ", "_",  study_id)) %>%
  select(study_id, email)

## * International core and species data ###############

# Rename attributes
internatl_core_data <- internatl_core_data_raw %>%
  rename(study_id = Source) %>%
  mutate(study_id = gsub(" ", "_",  study_id)) %>%
  rename(site_id = Location) %>%
  rename(core_id = `Site #`) %>%
  rename(country = Country) %>%
  rename(core_latitude = Latitude) %>%
  rename(core_longitude = Longitude) %>%
  rename(core_position_notes = "accuracy_flag") %>%
  rename(core_position_accuracy = "approx_acc") %>%
  mutate(vegetation_class = "forested") %>%
  mutate(vegetation_notes = "mangrove") %>%
  rename(landscape_position = "Landscape position") %>%
  rename(core_date = Years_collected) %>%
  mutate(core_date = as.Date(core_date, format = "%Y")) %>%
  rename(core_length = "total_depth") %>%
  rename(core_length_flag = "full_profile")

# Recode N/A to NA
internatl_core_data <- internatl_core_data %>%
  mutate(landscape_position = ifelse(landscape_position == "N/A", NA, landscape_position))

## * International species data ##############
internatl_species_data <- internatl_core_data %>%
  select(study_id, site_id, core_id, `Dominant species`) %>%
  rename(species_code = "Dominant species") %>%
  # there are multiple species listed for some cores
  # they need to be separated into separate rows
  separate_rows(species_code, sep=",") %>%
  separate_rows(species_code, sep="/") %>%
  # Avicennia marina mix should just be Avicennia marina
  mutate(species_code = str_replace(species_code, " mix", "")) %>%
  # Mixed to NA - no way to determine the species present
  mutate(species_code = str_replace(species_code, "Mixed", replacement = NA_character_)) %>%
  # Several species do not have complete names (for instance, only the genus is listed)
  # Those will be changed to NA
  mutate(species_code = ifelse(species_code == " Avicennia", NA, 
                               ifelse(species_code == "Bruguiera", NA, 
                                      ifelse(species_code == "Rhizophora", NA, species_code)))) %>%
  mutate(species_code = str_replace(species_code, "sp.", "spp")) %>%
  mutate(species_code = str_replace(species_code, "spp.", "spp"))

# finish core data 
internatl_core_data <- internatl_core_data %>%
  select(-`Landscape Unsure`, -`HGM Unsure`, -`Mangrove type`) %>%
  mutate(core_length = as.numeric(core_length)) %>%
  select(- `Site name`) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_date, 
         core_position_notes, core_length_flag, vegetation_class, vegetation_notes) %>%
  mutate(core_length_flag = recode(core_length_flag, 
                                   "Y" = "core depth represents deposit depth",
                                   "N" = "core depth limited by length of corer",
                                   "unsure" = "not specified",
                                   "unknown" = "not specified",
                                   "Not sure" = "not specified"))
  
## * International methods data ############

# Commenting out methods table as there is currently no variables present related to the CCRCN guidance
# internatl_methods_data <- internatl_core_data %>%
#   group_by(study_id) %>%
#   summarize(country = first(country), n=n())

## * international depthseries data ####################
internatl_depthseries_data <- internatl_depthseries_data_raw %>%
  rename(site_id = "Site name") %>%
  rename(core_id = "Site #") %>%
  rename(depth_min = "U_depth") %>%
  mutate(depth_min = 100 * as.numeric(depth_min)) %>%
  rename(depth_max = "L_depth") %>%
  mutate(depth_max = 100 * as.numeric(depth_max)) %>%
  rename(BD_est = "BD_estimated")

# Add in study IDs from international core data
internatl_depthseries_data <- internatl_core_data %>%
  select(study_id, core_id) %>%
  right_join(internatl_depthseries_data)

# Sanderman reports the measured BD (if measured), the modeled BD, and then the 
#   "final" BD-- the value chosen. We'll just use the final, and create a flag
#   describing whether it was measured or modeled
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(BD_final != BD_est) %>%
  mutate(DBD_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(BD_final == BD_est) %>%
  mutate(DBD_measured_or_modeled = "modeled")

depthseries_data_na <- internatl_depthseries_data %>%
  filter(is.na(BD_final)) %>%
  mutate(DBD_measured_or_modeled = NA)

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled) %>%
  bind_rows(depthseries_data_na)

# Back to renaming and recalculating attributes
internatl_depthseries_data <- internatl_depthseries_data %>%
  # assumption: BD stands for dry bulk density
  rename(dry_bulk_density = "BD_final") %>%
  mutate(dry_bulk_density = ifelse(is.na(dry_bulk_density), BD_est, dry_bulk_density)) %>%
  # convert from [Mg m-3] to [g cm-3]...which ends up being just x 1
  mutate(dry_bulk_density = as.numeric(dry_bulk_density) * 10^6 / 10^6) %>%
  select(-BD_reported, -BD_est) %>%
  rename(fraction_organic_matter = "SOM (%LOI)") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100)

# Similar as bulk density...organic carbon this time
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(`TOC (%)` == OC_final) %>%
  mutate(OC_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(`TOC (%)` != OC_final | is.na(`TOC (%)`)) %>%
  mutate(OC_measured_or_modeled = "modeled")

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Similar as bulk density and organic carbon...carbon density this time
depthseries_data_measured <- internatl_depthseries_data %>%
  filter(CD_calc == CD_reported) %>%
  mutate(CD_measured_or_modeled = "measured")

depthseries_data_modeled <- internatl_depthseries_data %>%
  filter(CD_calc != CD_reported | is.na(CD_reported)) %>%
  mutate(CD_measured_or_modeled = "modeled")

internatl_depthseries_data <- depthseries_data_measured %>%
  bind_rows(depthseries_data_modeled)

# Back to renaming and recalculating attributes
internatl_depthseries_data <- internatl_depthseries_data %>%
  rename(carbon_density = "CD_calc") %>%
  rename(fraction_carbon = OC_final) %>%
  mutate(fraction_carbon = as.numeric(fraction_carbon) / 100) %>%
  rename(fraction_carbon_type = "TC instead of TOC?") %>%
  mutate(fraction_carbon_type = gsub("N/A", "NA", fraction_carbon_type)) %>%
  mutate(carbon_profile_notes = "uses OC data if reported, otherwise used 0.5*LOI data")


# Recode fraction carbon type from yes/no to more descriptive
internatl_depthseries_data$fraction_carbon_type <- recode(internatl_depthseries_data$fraction_carbon_type,
                                                          Y = "total carbon",
                                                          N = "organic carbon")

# NOTE ASSUMPTION: "total carbon" rows include both those studies that designate that 
#   total carbon was measured, AND those studies that did not designate whether
#   total carbon or organic carbon was measured (i.e. did not mention whether
#   carbonates were removed)

internatl_depthseries_data %>% 
  drop_na(fraction_organic_matter, dry_bulk_density) %>% 
  ggplot(aes(fraction_organic_matter, dry_bulk_density, col = DBD_measured_or_modeled)) + 
  geom_point(alpha = 0.5) +
  ggtitle("Sanderman synthesis DBD ~ LOI")
# ggsave("sanderman_2018_loi_dbd.jpg")

## Final Formatting ####

source("./scripts/1_data_formatting/qa_functions.R")

# remove data that has since been published independently
studies_to_remove <- c("Breithaupt_et_al_2014", "Adame_et_al_2015")

depthseries <- internatl_depthseries_data %>%
  filter(!(study_id %in% studies_to_remove)) %>%
  
  # remove Cifuentes_Unpublished cores from Panama (since published)
  filter(!core_id %in% c("M0778", "M0779", "M0780", "M0781", "M0782", "M0783", "M0784", "M0785", "M0786", "M0787", "M0788", "M0789")) %>% 
  
  mutate(method_id = "single set of methods",
         # discard modeled values
         fraction_carbon = ifelse(OC_measured_or_modeled == "modeled", NA, fraction_carbon),
         dry_bulk_density = ifelse(DBD_measured_or_modeled == "modeled", NA, dry_bulk_density)) %>% 
  reorderColumns("depthseries", .) %>% 
  select(-OC_measured_or_modeled, -mid_point, -`TOC (%)`, -CD_reported, -Ocstock_reported, -carbon_density,
         -DBD_measured_or_modeled, -carbon_profile_notes, -fraction_carbon_type,
         -CD_measured_or_modeled)

# plot carbon, should only have measured values now
depthseries %>% 
  drop_na(fraction_organic_matter, fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = study_id)) + geom_point()

ids <- depthseries %>% distinct(study_id, site_id, core_id)

## ... Cores ####

cores <- internatl_core_data %>%
  filter(!(study_id %in% studies_to_remove)) %>% 
  
  # remove Cifuentes_Unpublished cores from Panama (since published)
  filter(!core_id %in% c("M0778", "M0779", "M0780", "M0781", "M0782", "M0783", "M0784", "M0785", "M0786", "M0787", "M0788", "M0789")) %>% 
  
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>% select(-core_date) %>%
  # some site ids were NA so I'm joining them from the depthseries table
  select(-site_id) %>% left_join(ids) %>%
  rename(habitat = vegetation_notes) %>% 
  
  # try to fill in some NA coords at the site-level so these cores show up in the Atlas
  mutate(core_position_notes = case_when(study_id == "Nsombo_et_al_2016" & is.na(core_latitude) ~ "approximate",
                                         study_id %in% c("Eid_and_Shaltout_2016_Egypt", "Eid_et_al_2016_Saudi_Arabia") ~ "estimated from GE",
                                         T ~ core_position_notes),
         core_position_method = case_when(core_position_notes %in% c("exact", "Exact") ~ "other high resolution",
                                          core_position_notes %in% c("estimated from GE", "approximate", "fuzzy (by request)") ~ "other low resolution",
                                          T ~ NA),
         core_latitude = case_when(study_id == "Nsombo_et_al_2016" & is.na(core_latitude) ~ 4.457111,
                                   site_id == "EME16 Egypt Station 1" ~ parzer::parse_lat("27°37'54N"),
                                   site_id == "EME16 Egypt Station 2" ~ parzer::parse_lat("27°29'24N"),
                                   site_id == "EME16 Egypt Station 3" ~ parzer::parse_lat("27°21'27N"),
                                   grepl("EME16 Saudi Arabia Station 1", site_id) ~ parzer::parse_lat("17°48'07N"),
                                   grepl("EME16 Saudi Arabia Station 2", site_id) ~ parzer::parse_lat("17°48'28N"),
                                   grepl("EME16 Saudi Arabia Station 3", site_id) ~ parzer::parse_lat("17°59'53N"),
                                   # coordinates fuzzy by request, but got shifted outside of correct country assignment. corrected with listed coordinates in paper
                                   study_id == "Bukoski_et_al_2017" & grepl("KB", site_id) ~ 8.04,
                                   study_id == "Bukoski_et_al_2017" & grepl("PP", site_id) ~ 8.5,
                                   study_id == "Bukoski_et_al_2017" & grepl("HP", site_id) ~ 20.5,
                                   study_id == "Bukoski_et_al_2017" & grepl("ND", site_id) ~ 20,
                                   study_id == "Bukoski_et_al_2017" & grepl("QN", site_id) ~ 21.2,
                                   grepl("KL3", site_id) ~ 10.2,
                                   T ~ core_latitude),
         core_longitude = case_when(study_id == "Nsombo_et_al_2016" & is.na(core_longitude) ~ 8.902583,
                                    site_id == "EME16 Egypt Station 1" ~ parse_lon("33°31'01E"),
                                    site_id == "EME16 Egypt Station 2" ~ parse_lon("33°37'39E"),
                                    site_id == "EME16 Egypt Station 3" ~ parse_lon("33°41'05E"),
                                    grepl("EME16 Saudi Arabia Station 1", site_id) ~ parse_lon("41°53'29E"),
                                    grepl("EME16 Saudi Arabia Station 2", site_id) ~ parse_lon("41°51'56E"),
                                    grepl("EME16 Saudi Arabia Station 3", site_id) ~ parse_lon("41°40'14E"),
                                    study_id == "Bukoski_et_al_2017" & grepl("KB", site_id) ~ 98.9,
                                    study_id == "Bukoski_et_al_2017" & grepl("PP", site_id) ~ 100.2,
                                    study_id == "Bukoski_et_al_2017" & grepl("HP", site_id) ~ 106.6,
                                    study_id == "Bukoski_et_al_2017" & grepl("ND", site_id) ~ 106,
                                    study_id == "Bukoski_et_al_2017" & grepl("QN", site_id) ~ 107.4,
                                    grepl("KL3", site_id) ~ 104.6,
                                    T ~ core_longitude)) %>%
  reorderColumns("cores", .)
 
# visual map check
cores %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lat = ~core_latitude, lng = ~core_longitude, 
                   radius = 2, label = ~study_id)

## ... Species ####

species <- internatl_species_data %>%
  filter(!(study_id %in% studies_to_remove)) %>%

  # remove Cifuentes_Unpublished cores from Panama (since published)
  filter(!core_id %in% c("M0778", "M0779", "M0780", "M0781", "M0782", "M0783", "M0784", "M0785", "M0786", "M0787", "M0788", "M0789")) %>% 

  drop_na(species_code) %>% 
  mutate(species_code = trimws(species_code),
         species_code = strsplit(species_code, split = "; ")) %>% 
  unnest(species_code) %>% 
  reorderColumns("species", .)

## Materials and methods data ################
# prelim_methods <- internatl_depthseries_data %>% 
#   filter(!(study_id %in% studies_to_remove)) %>%
#   filter(!(study_id == "Blue_Ventures_Unpublished" & fraction_carbon_type == "NA")) %>% 
#   filter(!(study_id == "Bulmer_et_al_2016" & is.na(fraction_carbon_type))) %>% 
#   distinct(study_id, fraction_carbon_type, carbon_profile_notes) %>% 
#   arrange(study_id) 
#   # mutate(method_id = "single set of methods")
# write_csv(prelim_methods, "data/primary_studies/Sanderman_2018/intermediate/Sanderman_2018_carbon_methods.csv")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects

# methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC Functions ##########################

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

testUniqueCores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
result <- test_numeric_vars(depthseries)

## Generate study citation table ######

# Kristensen et al 2000 is also cited in Fourqurean but the doi is not present here
# update the citation to match the Fourqurean so a synthesis bib can be created without duplicates
Kristensen_bib <- as.data.frame(ReadBib("data/primary_studies/Sanderman_2018/intermediate/Kristensen_citation.bib"))

Kristensen_citation <- Kristensen_bib %>% 
  mutate(bibliography_id = "Kristensen_et_al_2000_article",
         study_id = "Kristensen_et_al_2000",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

Eid_egypt_bib <- as.data.frame(GetBibEntryWithDOI("10.1016/j.rsma.2015.05.006")) %>% 
  mutate(bibliography_id = "Eid_and_Shaltout_2016_article",
         study_id = "Eid_and_Shaltout_2016_Egypt",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

Eid_saudi_bib <- as.data.frame(GetBibEntryWithDOI("10.1007/s12210-016-0542-6")) %>% 
  mutate(bibliography_id = "Eid_et_al_2016_article",
         study_id = "Eid_et_al_2016_Saudi_Arabia",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# add missing original paper citation for Phang et al 2016 (RC 9/6/24)
phang_bib <- as.data.frame(GetBibEntryWithDOI("10.1002/esp.3745")) %>% 
  mutate(bibliography_id = "Phang_et_al_2015_article",
         study_id = "Phang_et_al_2015_Saudi_Arabia",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()
  


# bring in all primary associated articles
primary_sources <- read_csv("data/primary_studies/Sanderman_2018/intermediate/Sanderman_2018_study_citations.csv") %>% 
  # filter(key != "Sanderman_2017") %>% 
  select(-key) %>%
  mutate(publication_type = "synthesis source") %>% 
  mutate(bibliography_id = case_when(bibliography_id == "Alongi_et_al_2000" & month == "oct" ~ paste0(bibliography_id, "_article_", month),
                                     bibliography_id == "Alongi_et_al_2008" ~ paste0(bibliography_id, "_article_", month),
                                     TRUE ~ paste0(bibliography_id, "_article"))) %>% 
  filter(bibliography_id != "Kristensen_et_al_2000_article") %>% 
  mutate_all(as.character) %>% 
  bind_rows(Kristensen_citation, Eid_egypt_bib, Eid_saudi_bib, phang_bib)

# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Sanderman 2018)

# create one synthesis citation and expand it to include all the studies
synthesis_citation <- data.frame(study_id = unique(cores$study_id),
                                 bibliography_id = "Sanderman_2018_data",
                                 publication_type = "synthesis dataset",
                                 bibtype = "Misc",
                                 doi = "10.7910/dvn/ocyuit",
                                 title = "Global mangrove soil carbon: dataset and spatial maps",
                                 author = "Jonathan Sanderman",
                                 publisher = "Harvard Dataverse",
                                 year = "2017",
                                 url = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OCYUIT")

study_citations <- bind_rows(primary_sources, synthesis_citation) %>%
  arrange(study_id) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

# WriteBib(as.BibEntry(bib_file), "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018.bib")
write_excel_csv(study_citations, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")

## Write data ###############

# write_csv(internatl_study_metadata, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_corresponding_authors.csv")
write_excel_csv(cores, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_cores.csv")
write_excel_csv(species, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_species.csv")
write_excel_csv(depthseries, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_depthseries.csv")

#-----

# # Re-order columns
# depthseries <- select_and_reorder_columns("depthseries", internatl_depthseries_data, "/data/primary_studies/Sanderman_2018/derivative/")
# core_data <- select_and_reorder_columns("core_level", internatl_core_data, "/data/primary_studies/Sanderman_2018/derivative/")
# methods <- select_and_reorder_columns("methods_and_materials", methods, "/data/primary_studies/Sanderman_2018/derivative/")
# # No guidance for biomass yet
# species <- select_and_reorder_columns("species", internatl_species_data, "/data/primary_studies/Sanderman_2018/derivative/")
# study_citations <- select_and_reorder_columns("associated_publications", study_citations, "/data/primary_studies/Sanderman_2018/derivative/")
# 
# 
# # test variable names
# test_varnames(cores)
# test_varnames(depthseries)
# test_varnames(site_data)
# test_varnames(species)
# test_varnames(study_citations)


## DOCUMENT AND FILTER UNPUBLISHED OR UN-CITED STUDIES ###################
# # read back in hooked and curated Sanderman data
# cores <- read.csv("data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_cores.csv")
# depthseries <- read.csv("data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_depthseries.csv")
# species <- read.csv("data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_species.csv")
#
# ## ... Remove Breithaupt et al. 2014 cores ###########
# # Cores are present in a figshare data release organized by the CCN
# # It appears Sanderman cores may have some data issues
# 
# cores <- filter(cores, study_id != "Breithaupt_et_al_2014")
# depthseries <- filter(depthseries, study_id != "Breithaupt_et_al_2014")
# species <- filter(species, study_id != "Breithaupt_et_al_2014")
# 
# write_csv(cores, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_cores.csv")
# write_csv(species, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_species.csv")
# write_csv(depthseries, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_depthseries.csv")


## ARCHIVED CITATION WORKFLOW ##
# studies <- cores %>% select(study_id) %>% distinct() %>% mutate(bibliography_id = "Sanderman_et_al_2018_data")
# 
# # create data citation
# data_bib <- GetBibEntryWithDOI("10.7910/dvn/ocyuit")
# data_citation <- as.data.frame(data_bib) %>%
#   mutate(bibliography_id = "Sanderman_et_al_2018_data",
#          publication_type = "primary") %>%
#   left_join(studies)
# 
# # The follow file represents the initial study citations file that has since been deprecated
# # The citations listed as "unpublished" in the map repo are still uncited below
# # citations <- read_csv("data/primary_studies/Sanderman_2018/intermediate/initial_citations.csv")
# 
# no_doi <- citations %>% filter(is.na(doi)) %>% filter(study_id != "Breithaupt_et_al_2014")
# 
# # Build citations for primary studies that have DOIs
# pub_dois <- citations %>% 
#   filter(!is.na(doi)) %>%
#   # filter(study_id %in% studies$study_id) %>%
#   filter(study_type != "synthesis") %>% 
#   select(study_id, bibliography_id, doi)
#   
# primary <- GetBibEntryWithDOI(primary_dois$doi)
# primary_df <- as.data.frame(primary)
# 
# study_citations_primary <- primary_df %>%
#   rownames_to_column("key") %>%
#   merge(primary_dois, by="doi", all.x=TRUE, all.y=FALSE) %>%
#   mutate(publication_type = bibtype) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, doi, everything())
# 
# # Import citations for primary studies that do not have DOIs
# primary_no_dois <- citations %>%
#   filter(is.na(year)==FALSE) %>%
#   mutate(year = as.character(year),
#          volume = as.character(volume), 
#          number = as.character(number)) %>%
#   mutate(publication_type = "Article",
#          bibtype = "Article",
#          key = study_id) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, year, volume, number, author, title, journal)
# 
# # Build the synthesis citation table and join all 
# biblio_synthesis <- BibEntry(bibtype = "Misc", 
#                              key = "Sanderman_2018", 
#                              title = "Global mangrove soil carbon: dataset and spatial maps",
#                              author = "Jonathan Sanderman", 
#                              doi = "10.7910/dvn/ocyuit",
#                              publisher = "Harvard Dataverse",
#                              year = "2017", 
#                              url = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OCYUIT")
# 
# biblio_synthesis <- as.data.frame(biblio_synthesis)
# 
# study_citations_synthesis <- citations %>%
#   filter(study_type == "synthesis" & study_id != "Breithaupt_et_al_2014") %>%
#   select(study_id, bibliography_id, doi) %>%
#   merge(biblio_synthesis, by="doi", all.x=TRUE, all.y=TRUE) %>%
#   mutate(key = "Sanderman_2017",
#          publication_type = "synthesis") %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, doi, everything()) %>%
#   bind_rows(study_citations_primary, primary_no_dois) %>%
#   mutate(year = as.numeric(year),
#          volume = as.numeric(volume), 
#          number = as.numeric(number)) 

