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
library(RCurl)
library(tidyverse)
library(rcrossref)
library(bib2df)

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

cores <- read_csv("./data/Holmquist_2018/original/V1_Holmquist_2018_core_data.csv")
depthseries <- read_csv("./data/Holmquist_2018/original/V1_Holmquist_2018_depth_series_data.csv")
impacts <-read_csv("./data/Holmquist_2018/original/V1_Holmquist_2018_impact_data.csv")
species <- read_csv("./data/Holmquist_2018/original/V1_Holmquist_2018_species_data.csv")
methods <- read_csv("./data/Holmquist_2018/original/V1_Holmquist_2018_methods_data.csv")
citations <- read_csv("data/Holmquist_2018/original/V1_Holmquist_2018_study_citations.csv")

## 3. Recode and rename factors #################

# Pull from curation functions script
source("./scripts/1_data_formatting/curation_functions.R")

cores <- cores %>%
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
  mutate(core_position_notes = recode(core_position_notes, 
                                      "a" = "latitude and longitude were likely from a high quality source",
                                      "a1" = "latitude and longitude from handheld GPS or better", 
                                      "a2" = "latitude and longitude were likely high quality but may refer to a general area rather than individual core location",
                                      "b" = "latitude and longitude represented coarse and general site coordinates", 
                                      "c" = "latitude and longitude were extracted from a map figure", 
                                      "c1" = "latitude and longitude were extracted from a relatively high quality map figure", 
                                      "c2" = "latitude and longitude were extracted from a relatively low quality map figure"), 
         inundation_class = tolower(inundation_class)) %>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  # Add underscores to site IDs
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  recode_salinity(salinity_class = salinity_class) %>%
  recode_vegetation(vegetation_class = vegetation_class) %>%
  # There's a typo with Galveston Bay sites
  mutate(site_id = recode(site_id, "Gavelston_Bay" = "Galveston_Bay"))

depthseries <- depthseries %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(study_id != "Gonneea_et_al_2018")

# Fraction carbon type should be in the methods metadata, not depthseries level 
fraction_carbon_type_metadata <- depthseries %>%
  group_by(study_id) %>%
  summarize(fraction_carbon_type = first(fraction_carbon_type)) 
depthseries <- select(depthseries, -fraction_carbon_type)

impacts <- impacts %>%
  rename(impact_class = "impact_code") %>%
  # The Crooks study ID should be 2014, not 2013. 
  recode_impact(impact_class = impact_class)%>%
  filter(study_id != "Gonneea_et_al_2018")

species <- species %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015")) %>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  recode_species(species_code = species_code)

methods <- methods %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014",
                                  "Nuttle_1988" = "Nuttle_1996",
                                  "Radabaugh_et_al_2017" = "Radabaugh_et_al_2018",
                                  "Hill_and_Anisfled_2015" = "Hill_and_Anisfeld_2015"))%>%
  filter(study_id != "Gonneea_et_al_2018") %>%
  select(-n) %>%
  merge(fraction_carbon_type_metadata, by="study_id")

## 4. Create study-level data ######
# NOTE: the study citations csv generated here is also copied into the /original
#   folder for safekeeping, as many of those DOIs were generated manually

# there should be two entries per study: 
# one for the primary study associated with the Study ID
# and another for the synthesis study (Holmquist et al. 2018)

synthesis_doi <- "10.25572/ccrcn/10088/35684"
synthesis_study_id <- "Holmquist_et_al_2018"

# link each study to the synthesis 
study_data <- cores %>%
  group_by(study_id) %>%
  summarize(study_type = "synthesis",
            bibliography_id = synthesis_study_id, 
            doi = synthesis_doi)

# Manually enter each primary study DOI
citations <- tibble(
  study_id = sort(as.character(unique(cores$study_id))),
  study_type = c("mastersthesis",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "dataset",
                  "techreport",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "article",
                  "phdthesis",
                  "article",
                  "article",
                  "article",
                  "book",
                  "article",
                  "article",
                  "article",
                  "techreport",
                  "article",
                  "article",
                  "article",
                  "article",
                  "techreport"),
  bibliography_id = sort(as.character(unique(cores$study_id))),
  doi = c("http://udspace.udel.edu/handle/19716/12831",
          "10.1016/j.ecoleng.2016.03.045",
          "10.1002/2014JG002715",
          "10.1016/j.margeo.2017.07.002",
          "10.1007/s12237-012-9508-9",
          "10.1006/ecss.1997.0299",
          "10.4319/lo.2007.52.3.1220",
          "https://cims.coastal.louisiana.gov/DataDownload/DataDownload.aspx?type=soil_properties",
          "10.13140/RG.2.1.1371.6568",
          "10.1007/s00267-015-0568-z",
          "10.1007/s12237-009-9202-8",
          "10.1007/s13157-010-0139-2",
          "10.1002/2015GL066830",
          "10.1016/j.margeo.2017.07.001",
          "10.1016/j.ecss.2015.06.004",
          "10.1016/j.orggeochem.2006.06.006",
          "10.1016/j.quageo.2012.05.004",
          "10.1016/j.ecss.2014.12.032",
          "https://elibrary.ru/item.asp?id=5305392",
          "10.1006/ecss.2001.0854",
          "10.1007/s10533-012-9805-1",
          "10.1007/s12237-016-0066-4",
          "10.6073/pasta/f20bcd9b51fb51b5e26df8fa03996baf",
          "10.3354/meps096269",
          "10.1306/D4267631-2B26-11D7-8648000102C1865D",
          "10.1007/s10533-017-0312-2",
          "10.3133/ofr20111094",
          "10.1007/s12237-017-0362-7",
          "10.1016/j.ecss.2016.10.001",
          "10.1007/s12237-013-9598-z",
          "10.2307/1353175",
          "10.3133/ofr20101299"
          )
)

study_data_primary <- citations %>%
  bind_rows(study_data)

## Generate BibTex citations
# Extract only one row for each bibliography entry
bib_studies <- study_data_primary %>%
  select(bibliography_id, doi) %>%
  distinct()

bibliography <- cr_ccrcn(dois = bib_studies$doi)

# Remove NULL elements using purrr:compact()
bibliography <- compact(bibliography)

# Manully add entries that failed (mostly because they don't have DOIS)
bibliography <- c(bibliography, 
      "@Mastersthesis{boydcomparison2012,
  Author = {Boyd, Brandon},
            School = {University of Delaware},
            Title = {Comparison of sediment accumulation and accretion in impounded and unimpounded marshes of the {Delaware} {Estuary},
            Year = {2012},
            url = {http://udspace.udel.edu/handle/19716/12831}
            }",
            "@Dataset{CRMS_Database,
            Author = {Coastal Protection and Restoration Authority},},
            Title = {CRMS Soil Properties},
            Year = {2015},
            url = {https://cims.coastal.louisiana.gov/}
            }",
      "@TECHREPORT{crookscoastal2014,
  Author = {Crooks, S and Rybczyk, J and O`Connell, K and Devier, D L and Poppe, K and Emmett-Mattox, S},
Institution = {Environmental Science Associates, Western Washington University, EarthCorps, and Restore America`s Estuaries},
 Title = {Coastal Blue Carbon Opportunity Assessment for the Snohomish Estuary: The Climate Benefits of Estuary Restoration},
  Year = {2014},     
  doi = {10.13140/RG.2.1.1371.6568},
  url = {http://rgdoi.net/10.13140/RG.2.1.1371.6568}
      }",
            "@Phdthesis{Merrill_1999,
            Author = {Merrill, J Z},
            School = {University of Maryland, College Park},
            Title = {Tidal Freshwater Marshes as Nutrient Sinks: Particulate Nutrient Burial and Denitrification},
            Type = {PhD} {Thesis},
            Year = {1999},
            url = {https://elibrary.ru/item.asp?id=5305392}
            }",
      "@book{Nuttle_1996,
	title = {Marsh sediment dynamics and organic matter survey {VCR}/{LTER} 1987-1988},
  doi = {10.6073/pasta/f20bcd9b51fb51b5e26df8fa03996baf},    
  url = {http://dx.doi.org/10.6073/pasta/f20bcd9b51fb51b5e26df8fa03996baf},
  urldate = {2017-06-28},
  author = {Nuttle, William},
  publisher = {Environmental Data Initiative},
      year = {1996}
      }"
            )

# Write this content out to a .bib file
fileConn <- file("data/Holmquist_2018/derivative/V1_Holmquist_2018_citation.bib")
writeLines(as.character(bibliography), fileConn)
close(fileConn)

# # Now import back in as a df
# bib <- bib2df("data/Holmquist_2018/derivative/bibliography.bib")
# 
# # Quick curation to enable us to join by DOI
# bib <- bib %>%
#   rename(doi = DOI) %>%
#   mutate(doi = ifelse(is.na(doi), URL, doi)) # If the DOI field is blank, pull
# # in the URL...which should match the supposed "DOI" in study_data_primary
# 
# study_data_primary <- study_data_primary %>%
#   left_join(bib, by = "doi")

## 5. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores) 
test_colnames("depthseries", depthseries)
test_colnames("species", species) 
test_colnames("impact", impacts) 

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, depthseries)

## 6. Write to folder ########
write_csv(cores, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
write_csv(depthseries, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
write_csv(impacts, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
write_csv(species, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
write_csv(methods, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
write_csv(study_data_primary, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_study_citations.csv")
