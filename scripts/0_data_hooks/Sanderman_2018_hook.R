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

## Materials and methods data ################
methods <- internatl_depthseries_data %>% 
  select(study_id, core_id, fraction_carbon_type, DBD_measured_or_modeled, 
         carbon_profile_notes)

# Now remove unwanted attributes from depthseries
internatl_depthseries_data <- internatl_depthseries_data %>%
  select(-mid_point, -`TOC (%)`, -CD_reported, -Ocstock_reported, -carbon_density,
         -DBD_measured_or_modeled, -carbon_profile_notes, -fraction_carbon_type,
         -OC_measured_or_modeled, - CD_measured_or_modeled)


## Final Formatting ####

source("./scripts/1_data_formatting/qa_functions.R")

# remove data that has since been published independently
studies_to_remove <- "Breithaupt_et_al_2014"

depthseries <- internatl_depthseries_data %>%
  filter(!(study_id %in% studies_to_remove)) %>%
  mutate(method_id = "single set of methods") %>% 
  reorderColumns("depthseries", .)

ids <- depthseries %>% distinct(study_id, site_id, core_id)

cores <- internatl_core_data %>%
  filter(!(study_id %in% studies_to_remove)) %>% 
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>% select(-core_date) %>%
  # some site ids were NA so I'm joining them from the depthseries table
  select(-site_id) %>% left_join(ids) %>% 
  rename(habitat = vegetation_notes) %>% 
  reorderColumns("cores", .)

species <- internatl_species_data %>%
  filter(!(study_id %in% studies_to_remove)) %>%
  drop_na(species_code) %>% 
  mutate(species_code = trimws(species_code),
         species_code = strsplit(species_code, split = "; ")) %>% 
  unnest(species_code) %>% 
  reorderColumns("species", .)

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

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
result <- test_numeric_vars(depthseries)

## Generate study citation table ######

# Kristensen et al 2000 is also cited in Fourqurean but the doi is not present here
# update the citation to match the Fourqurean so a synthesis bib can be created without duplicates
Kristensen_bib <- as.data.frame(GetBibEntryWithDOI("10.3354/ame022199"))

Kristensen_citation <- Kristensen_bib %>% 
  mutate(bibliography_id = "Kristensen_et_al_2000_article",
         study_id = "Kristensen_et_al_2000",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Sanderman 2018)

citations_raw <- read_csv("data/primary_studies/Sanderman_2018/intermediate/Sanderman_2018_study_citations.csv")

study_citations <- citations_raw %>%
  select(-key) %>%
  mutate(publication_type = ifelse(bibliography_id == "Sanderman_2018", "synthesis dataset", "synthesis source")) %>%
  mutate(bibliography_id = case_when(publication_type == "synthesis dataset" ~ paste0(bibliography_id, "_data"),
                                     bibliography_id == "Alongi_et_al_2000" & month == "oct" ~ paste0(bibliography_id, "_article_", month),
                                     bibliography_id == "Alongi_et_al_2008" ~ paste0(bibliography_id, "_article_", month),
                                     TRUE ~ paste0(bibliography_id, "_article"))) %>%
  filter(bibliography_id != "Kristensen_et_al_2000_article") %>% 
  mutate_all(as.character) %>% 
  bind_rows(Kristensen_citation) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018.bib")
write_csv(study_citations, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")

## Write data ###############

# write_csv(internatl_study_metadata, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_methods.csv")
write_csv(cores, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_cores.csv")
write_csv(species, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_species.csv")
write_csv(depthseries, "data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_depthseries.csv")

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

