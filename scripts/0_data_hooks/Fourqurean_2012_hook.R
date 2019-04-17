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

# Read in data
Fourqurean_raw <- read_excel("./data/Fourqurean_2012/original/JFourqurean_Global_SeagrassSoil_Corg.xls",
                          sheet = 2)

## 3. Curate data ######################

# Rename and remove attributes that will not make it into any dataset
Fourqurean <- Fourqurean_raw %>%
  separate(Location, into = c("site_id", "other"), sep = ",") %>%
  mutate(site_id = gsub(" ", "_", site_id)) %>%
  rename(core_id = "Core or site#", vegetation_class = "Seagrass or unvegetated") %>%
  select(-linenumber, -"Depth of accumulated sediment (cm)", -"Porosity (%)", 
         -"Soil organic carbon density (mg/mL)", -"Soil organic matter density (mg/mL)",
         -"loss on ignition (%)", -"coreserial") %>%
  mutate(vegetation_class = tolower(vegetation_class)) %>%
  # Standardize ontology for vegetation_class
  mutate(vegetation_class = ifelse(vegetation_class != "seagrass", 
                                   "unvegetated marine", vegetation_class))

## Recode References to study_ids
# Create vector of references
references <- c(na.omit(unique(Fourqurean$Reference)))
  
# Manually add each study ID
study_id_list <- c("Fourqurean_unpublished", "Orem_et_al_1999", "Figueiredo_da_Silva_et_al_2009", 
             "Fourqurean_et_al_2010", "coastally_et_al_2005", "Rosenfeld_1979", 
             "Mellors_et_al_2002", "Danovaro_et_al_1995", "Gonneea_et_al_2004", 
             "Marba_unpublished", "Grady_1981", "Pulich_1987", "De_Falco_2006", 
             "Furuta_2002", "De_Troch_et_al_2006", "Holmer_et_al_2003",
             "Boschker_unpublished", "Vichkovitten_and_Holmer_2005", "Holmer_et_al_2006", 
             "Holmer_et_al_2009", "Leduc_and_Probert_2011", "Yamamuro_et_al_1993", 
             "Pedersen_et_al_1997", "Barrón_et_al_2004",
             "Calleja_et_al_2007", "Holmer_and_Frederiksen_2007", "Mateo_and_Romero_1997", 
             "Lo_Iacono_et_al_2008", "Kairis_and_Rybczyk_2010", "Apostolaki_unpublished", 
             "Maher_and_Eyre_2010", "Borg_et_al_2010",
             "Spivak_et_al_2009", "Alongi_et_al_2008", "Spruzen_et_al_2008", 
             "Yarbro_and_carlson_2008", "Lewis_et_al_2007", "Qu_et_al_2006",
             "Bouillon_et_al_2004", "Rigollet_et_al_2004", "Grenz_et_al_2003",
             "Larned_2003", "Danovaro_and_Gambi_2002", "Eyre_and_Ferguson_2002", 
             "Gacia_et_al_2002", "Koch_and_Madden_2001", "Sfriso_and_Marcomini_1999", 
             "Miyajima_et_al_1998", "Danovaro_1996", "Koepfler_et_al_1993",
             "Amon_and_Herndl_1991", "Fourqurean_and_Kendrick_unpublished", 
             "Fonseca_et_al_2011", "Erftemeijer_and_Middelburg_1993", 
             "Burns_and_Swart_1992", "Hemminga_et_al_1994", "Deiongh_et_al_1995", 
             "Danovaro_et_al_1994", "Agawin_et_al_1996", "Isaksen_and_Finster_1996", 
             "Townsend_and_Fonseca_1998", "Stoner_et_al_1998", "Buzzelli_1998", 
             "Kristensen_et_al_2000", "Paula_et_al_2001", "McGlathery_unpublished", 
             "Cotner_et_al_2004", "Hebert_et_al_2007", "Oakes_and_Connolly_2004", 
             "Thimdee_et_al_2003", "Lillebo_et_al_2006", "Lee_et_al_2005", 
             "Al-Rousan_et_al_2005", "Kenig_et_al_1990", "Krause-Jensen_et_al_2011", 
             "Devereux_et_al_2011", "Ooi_et_al_2011", "Povidisa_and_Delefosse_unpublished", 
             "Gacia_unpublished", "Kamp-Nielsen_et_al_2002", "Mateo_unpublished", 
             "Mateo_et_al_1997", "Serrano_unpublished", "Mateo_and_Serrano_unpublished", 
             "Mateo_and_Serrano_unpublished", "Krause-Jensen_et_al_2011", "Holmer_et_al_2001", 
             "Holmer_unpublished", "van_Engeland_thesis", "Volkman_et_al_2008", 
             "Abed-Navandi_and_Dworschak_2005", "Duarte_unpublished", "Kennedy_unpublished", 
             "Copertino_unpublished")
  
# Create a tibble as a linking key between references and study IDs
studies <- tibble("Reference" = c(references), "study_id" = c(study_id_list))

Fourqurean <- Fourqurean %>%
  left_join(studies)

# Create core IDs from study IDs

source("./scripts/1_data_formatting/curation_functions.R")
Fourqurean <- create_IDs_from_study_IDs(Fourqurean, "core_id")


## 3a. Site-level data #############

site_data <- Fourqurean %>%
    filter(!is.na(site_id)) %>%
    group_by(site_id) %>%
    summarize_all(first) %>%
    select(study_id, site_id, vegetation_class)
    
## 3b. Core-level data ##############

core_data <- Fourqurean %>%
  rename(core_latitude = "latitude (DD.DDDD, >0 for N, <0 for S)",
       core_longitude = "longitude (DD.DDDD, >0 for E,<0 for W))") %>%
  mutate(core_length_flag = ifelse(`Surficial or profile?` == "P", "core depth represents deposit depth",
                                   NA)) %>%
  # We only want one row per core, so filter out all rows that don't have a core_latitude
  # filter(!is.na(core_latitude)) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_length_flag)

core_data <- core_data %>%
  group_by(core_id) %>%
  summarise_all(first)

## 3c. Depthseries data ################

depthseries <- Fourqurean %>%
  # Only choose cores that have a profile
  filter(`Surficial or profile?` == "P") %>%
  mutate(study_id = "Fourqurean_2012") %>%
  # The depth increment attribute is messy and needs some typos fixed/substitutions
  mutate(`Depth increment of slice` = gsub("--", "-", `Depth increment of slice`)) %>%
  mutate(`Depth increment of slice` = gsub("core bottom", "165-167", `Depth increment of slice`)) %>%
  separate(`Depth increment of slice`, into = c("depth_min", "depth_max"), sep = "-") %>%
  rename(# dry bulk density was measured in g/mL, which is the same as g/c3
         dry_bulk_density = "Dry Bulk density (g/ml)",
         fraction_carbon = "Soil total Carbon contet (%dw)",
         fraction_organic_matter = "Soil organic Carbon content (%dw)",
         age = "age#") %>%
  # Change percent to fraction
  mutate(fraction_carbon = fraction_carbon/100, 
         fraction_organic_matter = fraction_organic_matter/100) %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_carbon, 
         fraction_organic_matter, age)

## 3d. Materials and Methods data ##############

## 3e. Biomass data #################

biomass <- Fourqurean %>%
  rename(total_biomass = "Above + Below ground seagrass biomass (g dw m-2)", 
         total_AGB = "Above ground seagrass biomass (gdw m-2)",
total_BGB = "Below ground seagrass biomass (gdw m-2)", 
AG_carbon = "Above ground seagrass biomass carbon  (gC m-2)", 
BG_carbon = "Below ground seagrass biomass Carbon (gC m-2)") %>%
  filter(!is.na(total_AGB)) %>%
  select(study_id, site_id, core_id, total_AGB, total_BGB, total_biomass, 
         AG_carbon, BG_carbon)

## 3d. Species data ################

species <- Fourqurean %>%
  select(study_id, site_id, core_id, "seagrass species") %>%
  rename(species_code = "seagrass species") %>%
  mutate(species_code = gsub("m. ", "m, ", species_code)) %>%
  mutate(species_code = gsub("  &", ",", species_code)) %>%
  mutate(species_code = gsub(" and ", ",", species_code)) %>%
  separate(species_code, into = c("species1", "species2", "species3"), 
           sep = ", |,") %>%
  filter(!is.na(species1)) %>%
  gather(key = "count", value = "species_code", -study_id, -site_id, -core_id) %>%
  select(-count) %>%
  # From the previous gsubbing, certain cells are empty but not NA, fix this
  mutate_all(na_if, "") %>%
  # Remove duplicate rows
  distinct(site_id, core_id, species_code)

## 4. QA/QC of data ################

# Re-order columns
source("./scripts/1_data_formatting/qa_functions.R")
depthseries <- select_and_reorder_columns(depthseries, "depthseries", "./data/Fourqurean_2012/derivative/")
site_data <- select_and_reorder_columns(site_data, "site_level", "./data/Fourqurean_2012/derivative/")
core_data <- select_and_reorder_columns(core_data, "core_level", "./data/Fourqurean_2012/derivative/")
# No guidance for biomass yet
species <- select_and_reorder_columns(species, "species", "./data/Fourqurean_2012/derivative/")

## 5. write out data ##############

write_csv(depthseries, "./data/Fourqurean_2012/derivative/Fourqurean_2012_depthseries_data.csv")
write_csv(site_data, "./data/Fourqurean_2012/derivative/Fourqurean_2012_site_data.csv")
write_csv(core_data, "./data/Fourqurean_2012/derivative/Fourqurean_2012_core_data.csv")
write_csv(species, "./data/Fourqurean_2012/derivative/Fourqurean_2012_species_data.csv")

