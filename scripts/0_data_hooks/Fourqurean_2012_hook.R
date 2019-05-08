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

# Read in data
# 177 cores were missing a reference 
# and a subset of of these cores also included two cores under the same core serial number
# Due to the complexity of the issue, references were assigned to these cores manually in excel 
# Additionally, 12 cores were assigned new "coreserial" IDs. 
# The original data is in the "original" folder and the revised file is in the "intermediate" folder. 

Fourqurean_raw <- read_excel("./data/Fourqurean_2012/intermediate/JFourqurean_edited.xls")

## Recode References to study_ids
# Create vector of references
references <- tibble(reference = na.omit(unique(Fourqurean_raw$Reference)))
references <- references %>%
  arrange(reference)

# Write this out to an intermediate file
write_csv(references, "data/Fourqurean_2012/intermediate/references.csv")

# Now that 'references' was manually joined and inspected to a manually-compiled
#   set of study IDs and DOIs, read that in
study_doi_manual <- read_csv("data/Fourqurean_2012/intermediate/study_doi_manual.csv")
# Additionally, read in the full set of citations for studies with no DOI: 
citations_without_dois <- read_csv("data/Fourqurean_2012/intermediate/citations_without_dois.csv")

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
                                   "unvegetated marine", vegetation_class)) %>%
  # Join to the manual-compiled study list
  full_join(study_doi_manual) %>%
  select(-doi, -url, -reference, -bibliography_id, 
         -"Porosity (%)", -"Soil organic carbon density (mg/mL)", -"Soil organic matter density (mg/mL)",
         -"loss on ignition (%)")

# # Manually add each study ID
# study_id_list <- c("Fourqurean_unpublished", "Orem_et_al_1999", "Figueiredo_da_Silva_et_al_2009", 
#              "Fourqurean_et_al_2010", "Carruthers_et_al_2005", "Rosenfeld_1979", 
#              "Mellors_et_al_2002", "Danovaro_and_Fabiano_1995", "Gonneea_et_al_2004", 
#              "Marba_unpublished", "Grady_1981", "Pulich_1985", "De_Falco_et_al_2006", 
#              "Furuta_et_al_2002", "De_Troch_et_al_2006", "Holmer_et_al_2003",
#              "Boschker_et_al_2000", "Vichkovitten_and_Holmer_2005", "Holmer_et_al_2006", 
#              "Holmer_et_al_2009", "Leduc_and_Probert_2011", "Yamamuro_et_al_1993", 
#              "Pedersen_et_al_1997", "Barron_et_al_2004",
#              "Calleja_et_al_2007", "Holmer_and_Frederiksen_2007", "Mateo_and_Romero_1997", 
#              "Lo_Iacono_et_al_2008", "Kairis_and_Rybczyk_2010", "Apostolaki_unpublished", 
#              "Maher_and_Eyre_2010", "Borg_et_al_2010",
#              "Spivak_et_al_2009", "Alongi_et_al_2008", "Spruzen_et_al_2008", 
#              "Yarbro_and_carlson_2008", "Lewis_et_al_2007", "Qu_et_al_2006",
#              "Bouillon_et_al_2004", "Rigollet_et_al_2004", "Grenz_et_al_2003",
#              "Larned_2003", "Danovaro_and_Gambi_2002", "Eyre_and_Ferguson_2002", 
#              "Gacia_et_al_2002", "Koch_and_Madden_2001", "Sfriso_and_Marcomini_1999", 
#              "Miyajima_et_al_1998", "Danovaro_1996", "Koepfler_et_al_1993",
#              "Amon_and_Herndl_1991", "Fourqurean_and_Kendrick_unpublished", 
#              "Fonseca_et_al_2011", "Erftemeijer_and_Middelburg_1993", 
#              "Burns_and_Swart_1992", "Hemminga_et_al_1994", "Deiongh_et_al_1995", 
#              "Danovaro_et_al_1994", "Agawin_et_al_1996", "Isaksen_and_Finster_1996", 
#              "Townsend_and_Fonseca_1998", "Stoner_et_al_1998", "Buzzelli_1998", 
#              "Kristensen_et_al_2000", "Paula_et_al_2001", "McGlathery_unpublished", 
#              "Cotner_et_al_2004", "Hebert_et_al_2007", "Oakes_and_Connolly_2004", 
#              "Thimdee_et_al_2003", "Lillebo_et_al_2006", "Lee_et_al_2005", 
#              "Al-Rousan_et_al_2005", "Kenig_et_al_1990", "Krause-Jensen_et_al_2011", 
#              "Devereux_et_al_2011", "Ooi_et_al_2011", "Povidisa_and_Delefosse_unpublished", 
#              "Gacia_unpublished", "Kamp-Nielsen_et_al_2002", "Mateo_unpublished", 
#              "Mateo_et_al_1997", "Serrano_unpublished", "Mateo_and_Serrano_unpublished", 
#              "Mateo_and_Serrano_unpublished", "Krause-Jensen_et_al_2011", "Holmer_et_al_2001", 
#              "Holmer_unpublished", "van_Engeland_thesis", "Volkman_et_al_2008", 
#              "Abed-Navandi_and_Dworschak_2005", "Duarte_unpublished", "Kennedy_unpublished", 
#              "Copertino_unpublished")
  
# Create another linking df for references and coreserial, which is the only
#   unique identifier for cores
# core_identifier <- Fourqurean %>%
#   select(coreserial, study_id) %>%
#   filter(!is.na(study_id))

# # Join df's together
# studies <- studies %>%
#   left_join(core_identifier) %>%
#   group_by(coreserial) %>%
#   summarize_all(first)
# 
# Fourqurean <- Fourqurean %>%
#   left_join(studies, by = "coreserial")

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

## ... Filtering out invalid cores/studies ############
# Studies without IDs
# The following code was used to determine which cores were missing study IDs in the original dataset
# All studies have been correctly assigned IDs in the "intermediate" folders
# no_study_ids <- Fourqurean %>%
#   filter(is.na(study_id)==TRUE) %>%
#   group_by(coreserial) %>%
#   summarize(n=n())

# 7 cores are missing lat/long values
# They're filtered out of the dataset for now

# the offending core IDs: 
missing_lat_long <- c("Duarte_unpublished_12", "Duarte_unpublished_15", "Duarte_unpublished_5", "Duarte_unpublished_9",
                      "Kamp-Nielsen_et_al_2002_1", "Kamp-Nielsen_et_al_2002_2", "Kamp-Nielsen_et_al_2002_3")


# The following studies are missing reconstructable depth series intervals and do not have biomass data
remove_studies <- c("Gonneea_et_al_2004", "Furuta_et_al_2002", "Kenig_et_al_1990", 
                    "Holmer_unpublished", "Holmer_et_al_2001", "van_Engeland_thesis", "Volkman_et_al_2008",
                    "Abed-Navandi_and_Dworschak_2005", "Duarte_unpublished", "Kennedy_unpublished",
                    "Deiongh_et_al_1995", "Stoner_et_al_1998", 
                    # The following studies have incorrect lat/long values: 
                    "Holmer_et_al_2006", "Vichkovitten_and_Holmer_2005")

Fourqurean <- Fourqurean %>%
  filter(!(core_id %in% missing_lat_long)) %>%
  filter(!(study_id %in% remove_studies))

## ....3b. Site-level data #############

site_data <- Fourqurean %>%
    filter(!is.na(site_id)) %>%
    group_by(site_id) %>%
    summarize_all(first) %>%
    select(study_id, site_id, vegetation_class)
    
## ....3c. Core-level data ##############

core_data <- Fourqurean %>%
  rename(core_latitude = "latitude (DD.DDDD, >0 for N, <0 for S)",
       core_longitude = "longitude (DD.DDDD, >0 for E,<0 for W))") %>%
  mutate(core_length_flag = ifelse(`Surficial or profile?` == "P", "core depth represents deposit depth",
                                   NA)) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_length_flag) %>%
  group_by(core_id) %>%
  summarise_all(first) 

## ....3d. Depthseries data ################

depthseries <- Fourqurean %>%
  # The depth increment attribute is messy and needs some typos fixed/substitutions
  rename(depth = `Depth increment of slice`) %>%
  mutate(depth = gsub("--", "-", depth)) %>%
  mutate(depth = gsub("core bottom", "165-167", depth)) %>%
  mutate(depth = gsub("cm", "", depth)) %>%
  mutate(depth = gsub("'", "", depth)) %>%

  separate(depth, into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  
  rename(# dry bulk density was measured in g/mL, which is the same as g/c3
         dry_bulk_density = "Dry Bulk density (g/ml)",
         fraction_carbon = "Soil total Carbon contet (%dw)",
         fraction_organic_matter = "Soil organic Carbon content (%dw)",
         age = "age#") %>%
  
  # Turn negative fraction organic matter values to 0
  mutate(fraction_organic_matter = ifelse(fraction_organic_matter < 0, 0, fraction_organic_matter)) %>%
  
  # Filter out rows that do not have any relevant carbon or dating data 
  filter(!(is.na(fraction_organic_matter) & is.na(age) & is.na(fraction_carbon) & is.na(dry_bulk_density))) %>%
  # Filter out two studies with biomass data but no depthseries interval
  filter(!(study_id %in% c("Agawin_et_al_1996", "Townsend_and_Fonseca_1998"))) %>%
  
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
         fraction_organic_matter, age) 

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
depthseries <- core_data %>%
  select(core_id, site_id) %>%
  right_join(depthseries, by = "core_id")

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
                               "Syringodium filiform" = "Syringodium filiforme", 
                               "amphibolis antartica" = "Amphibolis antarctica", 
                               "Halopohila ovalis" = "Halophila ovalis", 
                               "Halodule wirghtii" = "Halodule wrightii",
                               "Halodule wrigthii" = "Halodule wrightii")) %>%
  # remove all entries without a species code 
  filter(is.na(species_code) == FALSE) 

## ....3h. Create study-level data ##########################
  
# We have a data table that was manually generating, which includes study IDs,
#   DOIs, and a URL if there is no DOI. No row has a value in both DOI and URL

# Select down to just the rows that have DOIs, which will reduce # of warnings
#   thrown by GetBibEntryWithDOI()
doi <- study_doi_manual %>%
  select(doi) %>%
  na.omit() %>%
  group_by(doi) %>%
  summarize(doi_ = first(doi))

# Get BibTex entries from DOI
biblio_raw <- GetBibEntryWithDOI(doi$doi_)
# Convert this to a dataframe
biblio <- as.data.frame(biblio_raw) %>%
  # GetBibEntryWithDOI() defaults study name as a row name, convert to column
  rownames_to_column("key") %>%
  mutate(doi = tolower(doi))

# Curate biblio so ready to read out as a BibTex-style .bib file
study_citations <- study_doi_manual %>%
  select(-url, -reference) %>%
  # Convert uppercase to lowercase for DOIs
  mutate(doi = tolower(doi)) %>%
  merge(biblio, by="doi", all.x = FALSE, all.y = TRUE) %>%
  mutate(publication_type = bibtype) %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  mutate(year = as.numeric(year),
         volume = as.numeric(volume)) %>%
  # Join entires without a DOI
  bind_rows(citations_without_dois)

# Create a separate row for each primary study_id - synthesis_id link
synthesis_data <- study_citations %>%
  filter(doi == "10.1038/ngeo1477") %>%
  select(-study_id, -publication_type) %>%
  distinct()

synthesis_citations <- study_citations %>%
  select(study_id) %>%
  mutate(bibliography_id = "Fourqurean_et_al_2012", 
         publication_type = "synthesis") %>%
  merge(synthesis_data, by="bibliography_id") %>%
  select(study_id, bibliography_id, publication_type, key, bibtype, everything()) %>%
  bind_rows(study_citations) %>%
  mutate(number = as.numeric(number),
         key = recode(key, "Alongi_2008" = "Alongi_2008_seagrass")) %>%
  distinct() 

# Write .bib file
bib_file <- synthesis_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "data/Fourqurean_2012/derivative/Fourqurean_2012.bib")

## 4. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Test column names 
test_colnames("core_level", core_data)
test_colnames("depthseries", depthseries)
test_colnames("site_level", site_data)
test_colnames("species", species)
test_colnames("associated_publications", study_citations)

# Re-order columns
depthseries <- select_and_reorder_columns(depthseries, "depthseries", "./data/Fourqurean_2012/derivative/")
site_data <- select_and_reorder_columns(site_data, "site_level", "./data/Fourqurean_2012/derivative/")
core_data <- select_and_reorder_columns(core_data, "core_level", "./data/Fourqurean_2012/derivative/")
# No guidance for biomass yet
species <- select_and_reorder_columns(species, "species", "./data/Fourqurean_2012/derivative/")
study_citations <- select_and_reorder_columns(study_citations, "associated_publications", "./data/Fourqurean_2012/derivative/")


# test variable names
test_varnames(core_data)
test_varnames(depthseries)
test_varnames(site_data)
test_varnames(species)
test_varnames(study_citations)

## ....4B. Quality control on cell values ###################
# Make sure that all core IDs are unique
test_unique_cores(core_data)

# Provide summary stats on each numeric column, to check for oddities
numeric_test_results <- test_numeric_vars(depthseries)

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(core_data, depthseries)

## 5. write out data ##############
write_csv(depthseries, "./data/Fourqurean_2012/derivative/Fourqurean_2012_depthseries_data.csv")
write_csv(site_data, "./data/Fourqurean_2012/derivative/Fourqurean_2012_site_data.csv")
write_csv(core_data, "./data/Fourqurean_2012/derivative/Fourqurean_2012_core_data.csv")
write_csv(species, "./data/Fourqurean_2012/derivative/Fourqurean_2012_species_data.csv")
write_csv(biomass, "./data/Fourqurean_2012/derivative/Fourqurean_2012_biomass_data.csv")
write_csv(synthesis_citations, "./data/Fourqurean_2012/derivative/Fourqurean_2012_study_citations.csv")
