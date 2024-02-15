## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Holmquist et al 2018 Scientific Reports
#   data release. 

# Holmquist, James R., Windham-Myers, Lisamarie, Bliss, Norman, Crooks, Stephen, Morris, James T., Megonigal, 
# J. Patrick, Troxler, Tiffany, Weller, Donald, Callaway, John, Drexler, Judith, Ferner, Matthew C., Gonneea, 
# Meagan E., Kroeger, Kevin D., Schile-Beers, Lisa, Woo, Isa, Buffington, Kevin, Boyd, Brandon M., Breithaupt, 
# Joshua, Brown, Lauren N., Dix, Nicole, Hice, Lyndie, Horton, Benjamin P., MacDonald, Glen M., Moyer, 
# Ryan P., Reay, William et al. 2018. 
# [Dataset] "Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States: 
# Public Soil Carbon Data Release." Distributed by Smithsonian Research Online.


## 1. Download data ################
# Load RCurl, a package used to download files from a URL
# library(RCurl)
library(tidyverse)
library(RefManageR)

## NOTE: this section commented out, but kept, because the data for the Holmquist
# 2018 Sci Reports data release does not include site IDs. If and when this data
# release is fixed we'll re-incorporate these lines, until then using internal
# data files

# # Create a list of the URLs for each data file
# url_list <- list("https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_core_data.csv?sequence=7&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_depth_series_data.csv?sequence=8&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_impact_data.csv?sequence=9&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_methods_data.csv?sequence=10&isAllowed=y",
#                  "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_species_data.csv?sequence=11&isAllowed=y"
# )
# 
# # Apply a function, which downloads each of the data files, over url_list
# lapply(url_list, function(x) {
#   # Extract the file name from each URL
#   filename <- as.character(x)
#   filename <- substring(filename, 56)
#   filename <- gsub("\\..*","", filename)
#   # Now download the file into the "data" folder
#   download.file(x, paste0(getwd(), "./data/Holmquist_2018/", filename, ".csv"))
# })

## 2. Import data to convert codes to common plain language ####

raw_cores <- read_csv("./data/primary_studies/Holmquist_2018/original/V1_Holmquist_2018_core_data.csv")
raw_depthseries <- read_csv("./data/primary_studies/Holmquist_2018/original/V1_Holmquist_2018_depth_series_data.csv")
raw_impacts <-read_csv("./data/primary_studies/Holmquist_2018/original/V1_Holmquist_2018_impact_data.csv")
raw_species <- read_csv("./data/primary_studies/Holmquist_2018/original/V1_Holmquist_2018_species_data.csv")
raw_methods <- read_csv("./data/primary_studies/Holmquist_2018/original/V1_Holmquist_2018_methods_data.csv")

# Jim extracted some cs137 data from the Elsey Quirk pub to include
Elsey_Quirk_dated_cores <- read_csv("data/primary_studies/Elsey-Quirk_et_al_2011/intermediate/Elsey-Quirk_et_al_2011_core_level_additional.csv")
Elsey_Quirk_cs137 <- read_csv("data/primary_studies/Elsey-Quirk_et_al_2011/intermediate/Elsey-Quirk_et_al_2011_depthseries_additional.csv") %>% 
  rename(cs137_unit = cs137_units) %>% 
  mutate(depth_interval_notes = "Cs137 activity data digitized from figure 4 using https://apps.automeris.io/wpd/ by J. Holmquist 2019-10-31.") %>% 
  select(-cs137_peak, -cs137_notes)

# remove the following studies that are now in their own separate data hooks: 
removed_studies <- c("Gonneea_et_al_2018", "Drexler_et_al_2009", "Weis_et_al_2001", "Noe_et_al_2016", "Johnson_et_al_2007", "Watson_and_Byrne_2013",
                     "Boyd_and_Sommerfield_2016", "Unger_et_al_2016", "Boyd_2012", "Boyd_et_al_2017", "Callaway_et_al_2012", "Piazza_et_al_2011",
                     "Gerlach_et_al_2017", "Craft_2007", "Crooks_et_al_2014", "Crooks_et_al_2013", "Breithaupt_et_al_2014", "Kemp_et_al_2012", 
                     "Ensign_et_al_2015", "Radabaugh_et_al_2018", "Drake_et_al_2015")

## 3. Data Curation #################

# Pull from curation functions script
source("./scripts/1_data_formatting/curation_functions.R")

## ...Cores ####
cores <- raw_cores %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  rename(vegetation_class = "vegetation_code",
         salinity_class = "salinity_code",
         core_position_notes= "position_code") %>%
  # recode core position notes 
  mutate(core_position_method = recode(core_position_notes, 
                                       "a" = "other high resolution",
                                       "a1" = "other moderate resolution", 
                                       "a2" = "other low resolution",
                                       "b" = "other low resolution", 
                                       "c" = "other low resolution", 
                                       "c1" = "other moderate resolution", 
                                       "c2" = "other low resolution"), 
         core_position_notes = recode(core_position_notes, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure"), 
         inundation_class = tolower(inundation_class)) %>%
  filter(!(study_id %in% removed_studies)) %>%
  # Add underscores to site IDs
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  recode_salinity(salinity_class = salinity_class) %>%
  recode_vegetation(vegetation_class = vegetation_class) %>%
  # There's a typo with Galveston Bay sites
  mutate(site_id = recode(site_id, "Gavelston_Bay" = "Galveston_Bay")) %>%
  # CRMS site IDs are going to be changed to their CRMS code in order to break up very large bounding boxes on the map. 
  # Ex: The site ID associated with CRMS core CRMS0003_2 will be CRMS0003
  mutate(site_id = ifelse(grepl("CRMS", core_id) == TRUE, 
                          gsub("_.*", "", core_id), site_id)) %>% 
  # Jaxine updates
  mutate(core_year = case_when(study_id == "Elsey_Quirk_et_al_2011" ~ 2008,
                               study_id == "Noe_et_al_2013" ~ 2010,
                               study_id == "Radabaugh_et_al_2018" ~ 2015,
                                         TRUE ~ NA_real_),
         core_elevation_method = case_when(study_id == "Hill_and_Anisfeld_2015" & !is.na(core_elevation) ~ "other high resolution",
                                           TRUE ~ NA_character_),
         core_elevation_notes = case_when(study_id == "Hill_and_Anisfeld_2015" & !is.na(core_elevation) ~ "Topcon GPT-3200 total station", 
                                          TRUE ~ NA_character_),
         core_elevation_accuracy = case_when(study_id == "Hill_and_Anisfeld_2015" & !is.na(core_elevation) ~ 0.002, TRUE ~ NA_real_),
         inundation_notes = ifelse(inundation_class %in% c("high", "low"), NA, inundation_class),
         inundation_class = ifelse(inundation_class %in% c("high", "low", "med"), inundation_class, NA)) %>%  
  full_join(Elsey_Quirk_dated_cores %>% select(-c(study_id)))

# need to make a lookup for the site_id to join with the depthseries
site_lookup <- distinct(cores, study_id, site_id, core_id)

#rose updates 2/9, removed Drake_et_al_2015 cores, now published under Drake_et_al_2024

## ... Depthseries ####

depthseries_carbon <- raw_depthseries %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"),
         method_id = "single set of methods",
         compaction_notes = ifelse(study_id == "CRMS_Database", "< 10-20%", NA),
         # remove modeled values of carbon
         fraction_carbon = case_when(core_id %in% c("WSC12", "WSC13_2", "WSC10_1") ~ NA,
                                     study_id == "Elsey_Quirk_et_al_2011" ~ NA,
                                     TRUE ~ fraction_carbon)) %>%
  filter(!(study_id %in% removed_studies)) %>% 
  full_join(Elsey_Quirk_cs137 %>% select(-c(study_id))) %>% 
  select(-site_id) %>% full_join(site_lookup)


# add site ID to final depthseries table
depthseries <- depthseries_carbon %>%
  left_join(distinct(cores, study_id, site_id, core_id)) %>% 
  select(-fraction_carbon_type) %>% 
  select(study_id, site_id, core_id, everything())

# Some visualization
depthseries %>% 
  # only Breithaupt_et_al_2017, Hill_and_Anisfeld_2015, Noe_et_al_2013, and Elsey_Quirk_et_al_2011
  drop_na(fraction_organic_matter, fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon)) +
  geom_point() +
  facet_wrap(~study_id, scales = 'free')
# Elsey_Quirk_et_al_2011 data looks modeled
# there are also modeled values in the Breithaupt et al 2017

depthseries %>% 
  drop_na(fraction_organic_matter, fraction_carbon) %>%
  # filter(study_id == "Elsey_Quirk_et_al_2011") %>%
  # filter(study_id == "Breithaupt_et_al_2017") %>%
  # filter(site_id %in% c("Broad_River", "Harney_River")) %>%
  # filter(core_id %in% c("WSC12", "WSC13_2", "WSC10_1")) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = core_id)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~core_id)
# suspicious cores: WSC12, WSC13_2, WSC10_1
# ggsave("breithaupt_2017_carbon_loi.jpg")

## ... Impacts ####
impacts <- raw_impacts %>%
  rename(impact_class = "impact_code") %>%
  # The Crooks study ID should be 2014, not 2013. 
  recode_impact(impact_class = impact_class)%>%
  drop_na(impact_class) %>% 
  filter(!(study_id %in% removed_studies))
  
## ... Species ####
species <- raw_species %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(!(study_id %in% removed_studies)) %>%
  recode_species(species_code = species_code) %>%
  drop_na(species_code)

## ... Methods ####

# Fraction carbon type should be in the methods metadata, not depthseries level 
fraction_carbon_type_metadata <- depthseries_carbon %>%
  group_by(study_id) %>%
  summarize(fraction_carbon_type = first(fraction_carbon_type)) 

methods <- raw_methods %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(!(study_id %in% removed_studies)) %>%
  select(-n, -publication_type) %>%
  merge(fraction_carbon_type_metadata, by="study_id") %>%
  mutate(fraction_carbon_type = recode(fraction_carbon_type, 
                                       "fraction_organic_carbon" = "organic carbon", 
                                       "fraction_total_carbon" = "total carbon"),
         carbonate_removal_method = recode(carbonate_removal_method,
                                           "HCl fumigation" = "acid fumigation"),
         roots_flag = recode(roots_flag, "roots seperated using floation" = "roots and rhizomes separated"),
         dry_bulk_density_flag = recode(dry_bulk_density_flag, 
                                        "time not specified" = "not specified",
                                        "no details" = "not specified"),
         loss_on_ignition_flag = recode(loss_on_ignition_flag, 
                                        "time not specified" = "not specified",
                                        "no details" = "not specified"),
         compaction_flag = ifelse(study_id == "CRMS_Database", "compaction quantified", compaction_flag)) %>% 
  # Jaxine updates
  rename(carbon_profile_notes = fraction_carbon_flag) %>% 
  mutate(sediment_sieved_flag = case_when(study_id %in% c("Elsey_Quirk_et_al_2011", "Noe_et_al_2013") ~ "sediment sieved",
                                          study_id == "Radabaugh_et_al_2018" ~ "sediment not sieved",
                                          TRUE ~ NA_character_),
         sediment_sieve_size = case_when(study_id %in% c("Noe_et_al_2013") ~ 2,
                                         study_id == "Elsey_Quirk_et_al_2011" ~ 0.42,
                                         TRUE ~ NA_real_),
         fraction_carbon_method = case_when(study_id %in% c("Hill_and_Anisfeld_2015", "Noe_et_al_2013") ~ "EA",
                                            study_id == "Elsey_Quirk_et_al_2011" ~ "Craft regression",
                                            TRUE ~ NA_character_),
         fraction_carbon_type = ifelse(study_id == "Radabaugh_et_al_2018", "total carbon", fraction_carbon_type),
         compaction_flag = case_when(study_id == "Elsey_Quirk_et_al_2011" ~ "compaction quantified",
                                     study_id == "Hill_and_Anisfeld_2015" ~ "corer limits compaction",
                                     TRUE ~ compaction_flag),
         coring_method = case_when(study_id == "Elsey_Quirk_et_al_2011" ~ "piston corer", TRUE ~ coring_method),
         roots_flag = case_when(study_id == "Elsey_Quirk_et_al_2011" ~ "roots and rhizomes separated", TRUE ~ roots_flag),
         carbon_measured_or_modeled = case_when(study_id == "Elsey_Quirk_et_al_2011" ~ "modeled", 
                                                TRUE ~ carbon_measured_or_modeled),
         carbonates_removed = ifelse(study_id == "Radabaugh_et_al_2018", FALSE, carbonates_removed),
         carbon_profile_notes = case_when(carbon_measured_or_modeled == "modeled" | study_id == "Breithaupt_et_al_2017" ~ "modeled carbon values removed from depthseries",
                                          carbon_profile_notes == "no details" ~ NA,
                                          TRUE ~ carbon_profile_notes),
         method_id = "single set of methods")

# core notes
# Drake 2015: core diameter 8.5cm; if we had dating info: "Cs137 661.62keV photopeak; Pb210 46.5keV photopeak"
# Elsey-Quirk 2011: core diameter 10cm; there is no dating info but there should be; carbon is modeled but there might be some measured values in there
# Hill_and_Anisfeld_2015: core diameter 15cm; core years: two in 2010, twelve in 2012, and two in 2013. compaction_fraction 0.05 but not corrected
# Noe et al 2013: some samples were fumigated and others were not..


library(leaflet)
leaflet(cores) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~core_longitude, lat = ~core_latitude, 
                   label = ~study_id, radius = 2)

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects

impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# guidance <- read_csv("docs/ccrcn_database_structure.csv")


# Check col and varnames
testTableCols(table_names) 
testTableVars(table_names) # quite a few uncontrolled variables
testRequired(table_names) 
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
result <- test_numeric_vars(depthseries)

# generate qa report
writeQualityReport("Holmquist_2018_synthesis")

id <- "Holmquist_2018_synthesis"
rmarkdown::render(input = "./scripts/1_data_formatting/qaqc_report.Rmd",
                  # output_format = "html_document",
                  output_file = "Holmquist_2018_synthesis_qa_report",
                  output_dir = "./docs/qa_reports/")

## Create Citations ######

# NOTE: the study citations csv generated here is also copied into the /original
#   folder for safekeeping, as many of those DOIs were generated manually

# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Holmquist et al. 2018)

## ARCHIVED STUDY CITATION WORKFLOW ##
# synthesis_doi <- "10.25572/ccrcn/10088/35684"
# synthesis_study_id <- "Holmquist_et_al_2018"
# 
# # The follow file represents the initial study citations file that has since been deprecated
# citations <- read_csv("./data/primary_studies/Holmquist_2018/intermediate/initial_citations.csv")
# 
# # Build citations for primary studies that have DOIs
# primary_dois <- citations %>%
#   filter(!(study_id %in% removed_studies)) %>%
#   filter(!is.na(doi)) %>%
#   filter(study_type != "synthesis") %>%
#   filter(study_id != "Nuttle_1996" & study_id != "Crooks_et_al_2014" & study_id != "Boyd_2012" & study_id != "CRMS_Database" & study_id != "Merrill_1999") %>%
#   select(study_id, bibliography_id, doi)
# 
# primary <- GetBibEntryWithDOI(primary_dois$doi)
# primary_df <- as.data.frame(primary)
# 
# study_citations_primary <- primary_df %>%
#   rownames_to_column("key") %>%
#   merge(primary_dois, by="doi", all.x=TRUE, all.y=TRUE) %>%
#   mutate(publication_type = bibtype) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, doi, everything())
# 
# # Manully add entries that failed (mostly because they don't have DOIS)
# crms <-  BibEntry(bibtype = "Misc", 
#                   key = "CRMS_2015", 
#                   title = "CRMS Soil Properties",
#                   author = "Coastal Protection and Restoration Authority", 
#                   year = "2015", 
#                   url = "https://cims.coastal.louisiana.gov")
# 
# merrill <- BibEntry(bibtype = "Phdthesis", 
#                    key = "Merrill_1999", 
#                    title = "Tidal Freshwater Marshes as Nutrient Sinks: Particulate Nutrient Burial and Denitrification",
#                    author = "Merrill, J Z", 
#                    school = "University of Maryland, College Park",
#                    year = "1999", 
#                    url = "https://elibrary.ru/item.asp?id=5305392"
#                    )
# 
# nuttle <- BibEntry(bibtype = "Book", 
#                    key = "Nuttle_1996", 
#                    title = "Tidal Freshwater Marshes as Nutrient Sinks: Particulate Nutrient Burial and Denitrification",
#                    author = "Nuttle, William", 
#                    publisher = "Environmental Data Initiative",
#                    year = "1996"
#                    )
# 
# primary_no_dois <- as.data.frame(list(c(crms, merrill, nuttle)))
# 
# primary_no_dois <- primary_no_dois %>% 
#   rownames_to_column("key") %>%
#   mutate(study_id = key, 
#          bibliography_id = key, 
#          publication_type = bibtype) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, everything())
# 
# study_citations_primary <- bind_rows(study_citations_primary, primary_no_dois)
# 
# biblio_synthesis <- BibEntry(bibtype = "Misc", 
#                              key = "Holmquist_et_al_2018", 
#                              title = "Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States: Public Soil Carbon Data Release",
#                              author = "Holmquist, James R. and Windham-Myers, Lisamarie and Bliss, Norman and Crooks, Stephen and Morris, James T. and Megonigal, J. Patrick and Troxler, Tiffany and Weller, Donald and Callaway, John and Drexler, Judith and Ferner, Matthew C. and Gonneea, Meagan E. and Kroeger, Kevin D. and Schile-Beers, Lisa and Woo, Isa and Buffington, Kevin and Boyd, Brandon M. and Breithaupt, Joshua and Brown, Lauren N. and Dix, Nicole and Hice, Lyndie and Horton, Benjamin P. and MacDonald, Glen M. and Moyer, Ryan P. and Reay, William and Shaw, Timothy and Smith, Erik and Smoak, Joseph M. and Sommerfield, Christopher and Thorne, Karen and Velinsky, David and Watson, Elizabeth and Wilson Grimes, Kristen and Woodrey, Mark", 
#                              doi = "10.25572/ccrcn/10088/35684",
#                              publisher = "Smithsonian Research Online",
#                              year = "2018", 
#                              url = "https://repository.si.edu/handle/10088/35684"
#                              )
# 
# biblio_synthesis <- as.data.frame(biblio_synthesis)
# 
# study_citations_synthesis <- citations %>%
#   filter(study_type == "synthesis") %>%
#   select(study_id, bibliography_id, doi) %>%
#   merge(biblio_synthesis, by="doi", all.x=TRUE, all.y=TRUE) %>%
#   mutate(key = "Holmquist_et_al_2018",
#          publication_type = "synthesis") %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, doi, everything()) %>%
#   bind_rows(study_citations_primary) %>%
#   mutate(year = as.numeric(year), 
#          volume = as.numeric(volume), 
#          number = as.numeric(number)) %>%
#   filter(!(study_id %in% removed_studies))


# read in citations
citations <- read_csv("data/primary_studies/Holmquist_2018/intermediate/Holmquist_2018_study_citations.csv")

study_citations_synthesis <- citations %>%
  select(-key) %>%
  mutate(bibliography_id = paste0(bibliography_id, "_", tolower(publication_type))) %>%
  mutate(publication_type = ifelse(publication_type != "synthesis", "synthesis source", "synthesis dataset"))

# Write .bib file
bib_file <- study_citations_synthesis %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018.bib")
write_csv(study_citations_synthesis, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_study_citations.csv")


## 6. Write to folder ########
write_csv(cores, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_cores.csv")
write_csv(depthseries, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_depthseries.csv")
write_csv(impacts, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_impacts.csv")
write_csv(species, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_species.csv")
write_csv(methods, "./data/primary_studies/Holmquist_2018/derivative/Holmquist_2018_methods.csv")
