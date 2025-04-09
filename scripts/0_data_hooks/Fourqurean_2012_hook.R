## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Citations for data and publication ##########

# Publication: 
# Fourqurean, James W., et al. "Seagrass ecosystems as a globally significant 
# carbon stock." Nature geoscience 5.7 (2012): 505. https://doi.org/10.1038/ngeo1477.
# Dataset provided by communication with authors.

## 2. Prep workspace #######################
library(tidyverse)
library(readxl)
library(RefManageR)
library(leaflet)

source("./scripts/1_data_formatting/qa_functions.R")


# Read in data
# 177 cores were missing a reference 
# and a subset of of these cores also included two cores under the same core serial number
# Due to the complexity of the issue, references were assigned to these cores manually in excel 
# Additionally, 12 cores were assigned new "coreserial" IDs. 
# The original data is in the "original" folder and the revised file is in the "intermediate" folder. 

Fourqurean_raw <- read_excel("./data/primary_studies/Fourqurean_2012/intermediate/JFourqurean_edited.xls")
study_doi_manual <- read_csv("data/primary_studies/Fourqurean_2012/intermediate/study_doi_manual.csv")

## 3. Curate data ######################

## ....3a. Prelim curation to raw dataset ###############
# Rename and remove attributes that will not make it into any dataset
Fourqurean <- Fourqurean_raw %>%
  separate(Location, into = c("site_id", "other"), sep = ",") %>%
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  rename(core_id = "Core or site#", vegetation_class = "Seagrass or unvegetated",
         reference = Reference) %>%
  mutate(vegetation_class = tolower(vegetation_class)) %>%
  # Standardize ontology for vegetation_class
  mutate(vegetation_class = ifelse(vegetation_class != "seagrass", 
                                   "unvegetated", vegetation_class),
         reference = recode(reference, "M. Copertino, unpubliished data" = "M. Copertino, unpublished data")) %>%
  # Join to the manual-compiled study list
  full_join(study_doi_manual) %>%
  select(-doi, -url, -reference, -bibliography_id, 
         -"Porosity (%)", -"Soil organic carbon density (mg/mL)", -"Soil organic matter density (mg/mL)",
         -"loss on ignition (%)")

## Create core IDs from study IDs 
Fourqurean_core <- Fourqurean %>%
  select(coreserial, study_id) %>%
  group_by(coreserial) %>%
  summarize(study_id = first(study_id))

# Note that you have to pass core-level data to this function
source("./scripts/1_data_formatting/curation_functions.R")
Fourqurean_core <- create_IDs_from_study_IDs_corelevel(Fourqurean_core, "core_id")
# Fourqurean_core <- Fourqurean_core %>%
#   select(-study_id)

# Now join back to master dataset
Fourqurean <- Fourqurean %>%
  select(-core_id, -study_id) %>%
  full_join(Fourqurean_core, by = "coreserial")


# One of the studies (Buzzelli 1998) must have had a typo in the core coordinates,
#   noticed from visual inspection of the Atlas by Jim. Dk then pulled the correct
#   coordinates from the Buzzelli 1998 paper: https://link.springer.com/content/pdf/10.2307%2F1353271.pdf
Fourqurean <- Fourqurean %>% 
  mutate(`longitude (DD.DDDD, >0 for E,<0 for W))` = 
           ifelse(study_id == "Buzzelli_1998", 
                  -76.39611111, 
                  `longitude (DD.DDDD, >0 for E,<0 for W))`)) %>% 
  mutate(`latitude (DD.DDDD, >0 for N, <0 for S))` = 
           ifelse(study_id == "Buzzelli_1998", 
                  37.21277778, 
                  `longitude (DD.DDDD, >0 for E,<0 for W))`))
  

## ... Filtering out or manually editing invalid cores/studies ############
# Studies without IDs
# The following code was used to determine which cores were missing study IDs in the original dataset
# All studies have been correctly assigned IDs in the "intermediate" folders
# no_study_ids <- Fourqurean %>%
#   filter(is.na(study_id)==TRUE) %>%
#   group_by(coreserial) %>%
#   summarize(n=n())

# manually selecting site-level positions on Google maps 
missing_duarte <- c("Duarte_unpublished_12", "Duarte_unpublished_15", 
                     "Duarte_unpublished_5", "Duarte_unpublished_9")
missing_kamp <- c("Kamp-Nielsen_et_al_2002_1", "Kamp-Nielsen_et_al_2002_2", 
                   "Kamp-Nielsen_et_al_2002_3")


# The following studies are missing reconstructable depth series intervals 
#   and do not have biomass data
remove_studies <- c("Furuta_et_al_2002", "Kenig_et_al_1990",
                    "Holmer_unpublished", "Holmer_et_al_2001", 
                    "van_Engeland_thesis", "Volkman_et_al_2008",
                    "Abed-Navandi_and_Dworschak_2005", "Duarte_unpublished", 
                    "Kennedy_unpublished", "Deiongh_et_al_1995", "Stoner_et_al_1998",
                    "Lo_Iacono_et_al_2008",
                    # Remove some studies that don't have depthseries data that fit into current guidance
                    "Isaksen_and_Finster_1996", "Koepfler_et_al_1993", "Mateo_and_Romero_1997", NA,
                    # No depthseries data
                    "Pedersen_et_al_1997")

Fourqurean <- Fourqurean %>%
  rename(core_latitude = "latitude (DD.DDDD, >0 for N, <0 for S)",
         core_longitude = "longitude (DD.DDDD, >0 for E,<0 for W))") %>%
  
  # insert site-level positions for cores missing lat/long 
  mutate(core_latitude = case_when(core_id %in% missing_duarte ~ 21.939162,
                                   core_id %in% missing_kamp ~ 16.346633,
                                   core_id %in% c("Vichkovitten_et_al_2005_1", "Vichkovitten_et_al_2005_2") ~ 54.967120,
                                   core_id %in% c("Vichkovitten_et_al_2005_3", "Vichkovitten_et_al_2005_4") ~ 55.466667, # 55.475912, 
                                   T ~ core_latitude),
         core_longitude = case_when(core_id %in% missing_duarte ~ -82.843898,
                                    core_id %in% missing_kamp ~ 119.926502,
                                    core_id %in% c("Vichkovitten_et_al_2005_1", "Vichkovitten_et_al_2005_2") ~ 10.541442,
                                    core_id %in% c("Vichkovitten_et_al_2005_3", "Vichkovitten_et_al_2005_4") ~ 11.083333, # 11.077794 ,
                                    T ~ core_longitude),
         position_notes = case_when(core_id %in% c(missing_duarte, missing_kamp, "Vichkovitten_et_al_2005_1", 
                                                   "Vichkovitten_et_al_2005_2", "Vichkovitten_et_al_2005_3", 
                                                   "Vichkovitten_et_al_2005_4") ~ "site-level positions chosen in Google maps based on surrounding core coordinates",
                                    T ~ NA_character_),
         position_method = case_when(core_id %in% c(missing_duarte, missing_kamp, "Vichkovitten_et_al_2005_1", 
                                                    "Vichkovitten_et_al_2005_2", "Vichkovitten_et_al_2005_3", 
                                                    "Vichkovitten_et_al_2005_4") ~ "other low resolution",
                                     study_id == "Holmer_et_al_2006" ~ "other low resolution", # coordinates are approximate
                                     T ~ NA_character_),
         site_id = case_when(core_id %in% missing_kamp ~ "Bolinao",
                             T ~ site_id)) %>% 
  filter(!(study_id %in% remove_studies))

## ....3b. Core-level data ##############

core_data <- Fourqurean %>%

  select(study_id, site_id, core_id, core_latitude, core_longitude, position_method, position_notes, vegetation_class) %>%
  group_by(core_id) %>%
  summarise_all(first) %>% 
  rename(habitat = vegetation_class)


kamp_missing_cores <- c("Kamp-Nielsen_et_al_2002_14", "Kamp-Nielsen_et_al_2002_15", "Kamp-Nielsen_et_al_2002_16", "Kamp-Nielsen_et_al_2002_17",
                        "Kamp-Nielsen_et_al_2002_18", "Kamp-Nielsen_et_al_2002_19", "Kamp-Nielsen_et_al_2002_55", "Kamp-Nielsen_et_al_2002_56",
                        "Kamp-Nielsen_et_al_2002_57", "Kamp-Nielsen_et_al_2002_58", "Kamp-Nielsen_et_al_2002_59", "Kamp-Nielsen_et_al_2002_60")

core_data <- core_data %>%
  filter(!core_id %in% kamp_missing_cores) 

cores <- reorderColumns("cores", core_data)

## ....3c. Site-level data #############
# Create site boundaries
site_boundaries <- core_data %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) 

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

site_data <- Fourqurean %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  summarize_all(first) %>%
  merge(site_boundaries, by="site_id") %>%
  select(study_id, site_id, 
         site_longitude_max, site_longitude_min, site_latitude_max, site_latitude_min,
         vegetation_class) %>%
  mutate(vegetation_class = ifelse(vegetation_class == "unvegetated marine", NA, vegetation_class))

sites <- reorderColumns("sites", site_data)

## ....3d. Depthseries data ################

synthesis_depthseries <- Fourqurean %>%
  # The depth increment attribute is messy and needs some typos fixed/substitutions
  rename(depth = `Depth increment of slice`, depth_center = `depth at center of slice (cm)`,
         thickness = `slice thickness (cm)`) %>%
  mutate(depth = gsub("--", "-", depth)) %>%
  mutate(depth = gsub("core bottom", "165-167", depth)) %>%
  mutate(depth = gsub("cm", "", depth)) %>%
  mutate(depth = gsub("'", "", depth)) %>%
    
  separate(depth, into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  
  # Manually convert depth interval information for a few cores 
  mutate(depth_min = ifelse(core_id == "Gonneea_et_al_2004_1", 
                            (depth_center - thickness / 2),
                            depth_min)) %>%
  mutate(depth_max = ifelse(core_id == "Gonneea_et_al_2004_1", 
                            depth_center + thickness / 2,
                            depth_max),
         depth_interval_notes = case_when(is.na(depth_max) ~ "surface sample, depth interval unknown",
                                          # "Townsend_and_Fonseca_1998" surface sample in bioturbation pits close to seagrass beds
                                          T ~ NA)) %>%
  
  rename(# dry bulk density was measured in g/mL, which is the same as g/c3
         dry_bulk_density = "Dry Bulk density (g/ml)",
         fraction_carbon = "Soil total Carbon contet (%dw)",
         fraction_organic_matter = "Soil organic Carbon content (%dw)",
         age = "age#") %>%
  
  # Turn negative fraction organic matter values to 0
  mutate(fraction_organic_matter = ifelse(fraction_organic_matter < 0, 0, fraction_organic_matter)) %>%
  
  # Filter out rows that do not have any relevant carbon or dating data 
  filter(!(is.na(fraction_organic_matter) & is.na(age) & is.na(fraction_carbon) & is.na(dry_bulk_density))) %>%

  # Fix some issues with age: parsed as character, some entries are approx and not #s
  mutate(age = gsub("B.P.", "", age)) %>%
  mutate(age = gsub(" yBP", "", age)) %>%
  separate(age, into = c("age", "age_2"), sep = "-") %>%
  mutate(age = as.double(age), age_2 = as.double(age_2)) %>%
  mutate(age = ifelse(!is.na(age_2), ((age + age_2)/2), age)) %>%
  
  # Change percent to fraction
  mutate(fraction_carbon = fraction_carbon/100, 
         fraction_organic_matter = fraction_organic_matter/100) %>%
  select(study_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon, 
         fraction_organic_matter, age)  %>%
  # filter out intervals missing core IDs 
  filter(!is.na(core_id))

# Check quality of depth interval
# Import the raw data file in the "original" folder and run the following code 
# to view issues with original intervals 
# intervals <- depthseries %>%
#   mutate(min_greater = ifelse(depth_min > depth_max, TRUE, FALSE), 
#          no_min = ifelse(is.na(depth_min), TRUE, FALSE), 
#          no_max = ifelse(is.na(depth_max), TRUE, FALSE)) %>%
#   select(core_id, depth_min, depth_max, min_greater, no_min, no_max) %>%
#   filter(min_greater == TRUE | no_min == TRUE | no_max == TRUE)

# Join site_ids from core_data
depthseries <- left_join(synthesis_depthseries, 
                         core_data %>% select(core_id, site_id)) %>%
  mutate(method_id = "see original source")

## ....3e. Materials and Methods data ##############

## ....3f. Biomass data #################

biomass <- Fourqurean %>%
  rename(total_biomass = "Above + Below ground seagrass biomass (g dw m-2)", 
         total_AGB = "Above ground seagrass biomass (gdw m-2)",
total_BGB = "Below ground seagrass biomass (gdw m-2)", 
AG_carbon = "Above ground seagrass biomass carbon  (gC m-2)", 
BG_carbon = "Below ground seagrass biomass Carbon (gC m-2)") %>%
  filter(!is.na(total_AGB)) %>%
  select(study_id, site_id, core_id, total_AGB, total_BGB, total_biomass, 
         AG_carbon, BG_carbon)

## ....3g. Species data ################

species <- Fourqurean %>%
  select(study_id, site_id, core_id, "seagrass species") %>%
  rename(species_code = "seagrass species") %>%
  # Prepare the species to be separated by commas 
  # Turn all non comma separators to commas 
  # and remove instances of duplicate commas 
  mutate(species_code = gsub("m. ", "m, ", species_code)) %>%
  mutate(species_code = gsub("&", ",", species_code)) %>%
  mutate(species_code = gsub("and", ",", species_code)) %>%
  mutate(species_code = gsub(", ,", ",", species_code)) %>%
  # Separate the rows by comma 
  separate_rows(species_code, sep=",") %>%
  # remove leading and trailing empty spaces from strings 
  mutate(species_code = str_trim(species_code, "both")) %>%
  # recode some species codes
  mutate(species_code = recode(species_code, 
                               "Thassia hemprichii" = "Thalassia hemprichii",
                               "Syringodium filiform" = "Syringodium filiforme", 
                               "amphibolis antartica" = "Amphibolis antarctica", 
                               "Halopohila ovalis" = "Halophila ovalis", 
                               "Halodule wirghtii" = "Halodule wrightii",
                               "Halodule wrigthii" = "Halodule wrightii")) %>%
  # remove all entries without a species code 
  filter(is.na(species_code) == FALSE) 

## ....3h. Create Citations ##########################
  
# Kristensen et al 2000 is also cited in Sanderman but the doi is not present here
# update the citation to match the Sanderman so a synthesis bib can be created without duplicates
Kristensen_bib <- as.data.frame(ReadBib("data/primary_studies/Fourqurean_2012/intermediate/Kristensen_citation.bib"))

Kristensen_citation <- Kristensen_bib %>% 
  mutate(bibliography_id = "Kristensen_et_al_2000_article",
         study_id = "Kristensen_et_al_2000",
         publication_type = "synthesis source") %>%
  select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
  remove_rownames()

# bring in all primary associated articles
primary_sources <- read_csv("data/primary_studies/Fourqurean_2012/intermediate/Fourqurean_2012_study_citations.csv") %>% 
  filter(key != "Fourqurean_2012") %>%
  select(-key) %>%
  mutate(publication_type = "synthesis source",
         bibliography_id = paste0(bibliography_id, "_", tolower(bibtype))) %>%
  mutate(bibliography_id = ifelse(bibliography_id == "Alongi_et_al_2008_article", 
                                  paste0(bibliography_id, "_", month), bibliography_id)) %>% 
  filter(bibliography_id != "Kristensen_et_al_2000_article") %>% 
  mutate_all(as.character) %>% 
  bind_rows(Kristensen_citation) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# create one synthesis citation and expand it to include all the studies
synthesis_citation <- data.frame(study_id = unique(cores$study_id),
                                 bibliography_id = "Fourqurean_et_al_2012_article",
                                 publication_type = "synthesis source",
                                 bibtype = "Article",
                                 doi = "10.1038/ngeo1477",
                                 title = "Seagrass ecosystems as a globally significant carbon stock",
                                 author = "James W. Fourqurean and Carlos M. Duarte and Hilary Kennedy and NÃºria Mar {à} and Marianne Holmer and Miguel Angel Mateo and Eugenia T. Apostolaki and Gary A. Kendrick and Dorte Krause-Jensen and Karen J. McGlathery and Oscar Serrano",
                                 publisher = "Springer Nature",
                                 year = "2012", month = "may",
                                 volume = "5", number = "7", pages = "505--509",
                                 journal = "Nature Geoscience",
                                 url = "https://doi.org/10.1038/ngeo1477")


study_citations <- bind_rows(primary_sources, synthesis_citation) %>%
  arrange(study_id) %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012.bib") # there might be some improper utf characters present
write_excel_csv(study_citations, "data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_study_citations.csv")

# -----
## ARCHIVED DATA CITATION WORKFLOW ##
# 
# ## Recode References to study_ids
# # Create vector of references
# references <- tibble(reference = na.omit(unique(Fourqurean_raw$Reference)))
# references <- references %>%
#   arrange(reference)
# 
# # Write this out to an intermediate file
# # write_csv(references, "data/primary_studies/Fourqurean_2012/intermediate/references.csv")
# 
# # We have a data table that was manually generating, which includes study IDs,
# #   DOIs, and a URL if there is no DOI. No row has a value in both DOI and URL
# 
# # Now that 'references' was manually joined and inspected to a manually-compiled
# #   set of study IDs and DOIs, read that in
# study_doi_manual <- read_csv("data/primary_studies/Fourqurean_2012/intermediate/study_doi_manual.csv")
# # Additionally, read in the full set of citations for studies with no DOI: 
# citations_without_dois <- read_csv("data/primary_studies/Fourqurean_2012/intermediate/citations_without_dois.csv")
# 
# # Select down to just the rows that have DOIs, which will reduce # of warnings
# #   thrown by GetBibEntryWithDOI()
# doi <- study_doi_manual %>%
#   select(doi) %>%
#   na.omit() %>%
#   group_by(doi) %>%
#   summarize(doi_ = first(doi))
# 
# # Get BibTex entries from DOI
# biblio_raw <- GetBibEntryWithDOI(doi$doi_)
# # Convert this to a dataframe
# biblio <- as.data.frame(biblio_raw) %>%
#   # GetBibEntryWithDOI() defaults study name as a row name, convert to column
#   rownames_to_column("key") %>%
#   mutate(doi = tolower(doi))
# 
# # Curate biblio so ready to read out as a BibTex-style .bib file
# study_citations <- study_doi_manual %>%
#   select(-url, -reference) %>%
#   # Convert uppercase to lowercase for DOIs
#   mutate(doi = tolower(doi)) %>%
#   merge(biblio, by="doi", all.x = FALSE, all.y = TRUE) %>%
#   mutate(publication_type = bibtype) %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
#   mutate(year = as.numeric(year),
#          volume = as.numeric(volume)) %>%
#   # Join entires without a DOI
#   bind_rows(citations_without_dois)
# 
# # Create a separate row for each primary study_id - synthesis_id link
# synthesis_data <- study_citations %>%
#   filter(doi == "10.1038/ngeo1477") %>%
#   select(-study_id, -publication_type) %>%
#   distinct()
# 
# synthesis_citations <- study_citations %>%
#   select(study_id) %>%
#   mutate(bibliography_id = "Fourqurean_et_al_2012", 
#          publication_type = "synthesis") %>%
#   merge(synthesis_data, by="bibliography_id") %>%
#   select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
#   bind_rows(study_citations) %>%
#   mutate(number = as.numeric(number),
#          key = recode(key, "Alongi_2008" = "Alongi_2008_seagrass")) %>%
#   distinct() 
# 
# # Write .bib file
# bib_file <- synthesis_citations %>%
#   select(-study_id, -bibliography_id, -publication_type) %>%
#   distinct() %>%
#   column_to_rownames("key")
# 
# WriteBib(as.BibEntry(bib_file), "data/Fourqurean_2012/derivative/Fourqurean_2012.bib")

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects
sites <- updated$sites
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## 4. QA/QC of data ################

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

testUniqueCores(cores)
test_unique_coords(cores)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

testIDs(cores, depthseries, by = "core")

# Mapping check
Fourqurean %>% 
  filter(grepl("Vichkovitten", study_id)) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~as.numeric(core_longitude), lat = ~as.numeric(core_latitude), 
                   radius = 2, label = ~core_id)

# # Test column names 
# test_colnames("core_level", core_data)
# test_colnames("depthseries", depthseries)
# test_colnames("site_level", site_data)
# test_colnames("species", species)
# test_colnames("associated_publications", study_citations)

# # Re-order columns
# depthseries <- select_and_reorder_columns("depthseries", depthseries, "./data/Fourqurean_2012/derivative/")
# site_data <- select_and_reorder_columns("site_level", site_data, "./data/Fourqurean_2012/derivative/")
# core_data <- select_and_reorder_columns("core_level", core_data, "./data/Fourqurean_2012/derivative/")
# # No guidance for biomass yet
# species <- select_and_reorder_columns("species", species, "./data/Fourqurean_2012/derivative/")
# study_citations <- select_and_reorder_columns("associated_publications", study_citations, "./data/Fourqurean_2012/derivative/")

# # test variable names
# test_varnames(core_data)
# test_varnames(depthseries)
# test_varnames(site_data)
# test_varnames(species)
# test_varnames(study_citations)

## 5. write out data ##############
write_csv(depthseries, "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_depthseries.csv")
write_csv(sites, "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_sites.csv")
write_csv(cores, "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_cores.csv")
write_csv(species, "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_species.csv")
write_csv(biomass, "./data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_biomass.csv")
