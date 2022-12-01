# Dataset: Carbon Sequestration and Sediment Accretion in San Francisco Bay Tidal Wetlands
# 
# Authors: John C. Callaway <callaway@usfca.edu>; Evyan Borgnis;  R. Turner; Charles Milan
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/data.9693251
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in six separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# callaway_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# callaway_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# callaway_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# callaway_et_al_2019_sites.csv - Contains positional information on site locations.
# callaway_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
# callaway_et_al_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - A simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - A map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# associated_publications.bib - Is a text file containing citation information in bibtex style for the associated publication accompanying this data release.
# 
# callaway_et_al_2019_associated_publication.csv - Is a CSV file containing citation information for the associated publication accompanying this data release.

library(tidyverse)
library(RefManageR)

source("scripts/1_data_formatting/curation_functions.R")


cores_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_cores.csv")
species_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_species.csv")
depthseries_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_depthseries.csv")
impacts_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_impacts.csv")
sites_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_sites.csv")
methods_raw <- read_csv("./data/primary_studies/Callaway_2019/original/callaway_et_al_2019_methods.csv")


# Remove accumulation/accretion rates for core level
cores <- cores_raw %>%
  mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
         core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK"),
         core_length_flag = "core depth limited by length of corer") %>%
  mutate(core_year = year(ymd(core_date)),
         core_month = month(ymd(core_date)),
         core_day = day(ymd(core_date))) %>%
  select(-c(cs137_accretion_rate, cs137_mineral_accumulation_rate, cs137_carbon_accumulation_rate,
            pb210_accretion_rate, pb210_mineral_accumulation_rate, pb210_carbon_accumulation_rate,
            core_date))

# Species data needs to combine genus and species into one and remove other columns
species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  mutate(species_code = recode(species_code,
                               "Typa domingensis" = "Typha domingensis")) %>% 
  select(study_id, site_id, core_id, species_code) 
  # resolveTaxa(.) %>% 
  # mutate(species_code = ifelse(species_code != resolved_taxa, resolved_taxa, species_code)) %>%
  # select(-resolved_taxa)

# Remove NS cores from depthseries (no other metadata)
depthseries <- depthseries_raw %>%
  filter(!grepl("NS_B", core_id)) %>%
  mutate(method_id = "single set of methods", 
         cs137_unit = ifelse(!is.na(cs137_activity), "picocuriesPerGram", NA), 
         pb210_unit = ifelse(!is.na(excess_pb210_activity), "picocuriesPerGram", NA),
         bi214_unit = ifelse(!is.na(bi214_activity), "becquerelsPerKilogram", NA)) 

sites <- sites_raw

impacts <- impacts_raw

methods <- methods_raw %>% 
  mutate(method_id = "single set of methods",
         carbonates_removed = FALSE,
         fraction_carbon_type = "total carbon")

## Create citation info  

if(!file.exists("data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_study_citations.csv")){
  associated_bib_doi <- "10.1007/s12237-012-9508-9"
  data_release_doi <- "10.25573/data.9693251"
  study_id_value <- unique(cores$study_id)
  
  paper_bib_raw <- GetBibEntryWithDOI(associated_bib_doi)
  data_bib_raw <- GetBibEntryWithDOI(data_release_doi)
  
  # Convert this to a dataframe
  paper_biblio <- as.data.frame(paper_bib_raw) %>%
    mutate(study_id = study_id_value,
           bibliography_id = "Callaway_et_al_2012_article",
           publication_type = "associated source")
  
  data_biblio <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = study_id_value,
           bibliography_id = "Callaway_et_al_2019_data",
           publication_type = "primary dataset")
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- data_biblio %>%
    bind_rows(paper_biblio) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019.bib")
  write_csv(study_citations, "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("sites", "methods", "cores", "depthseries", "species", "impacts")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

testTaxa(table_names)

testRequired(table_names)
testConditional(table_names)

testUniqueCores(cores)
testUniqueCoords(cores)

testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "core")

fractionNotPercent(depthseries)
results <- test_numeric_vars(depthseries)

# Data visualization report
writeDataVizReport("Callaway_et_al_2019")

# write files
write_csv(methods, "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_methods.csv")
write_csv(species, "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_species.csv")
write_csv(cores, "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_cores.csv")
write_csv(depthseries, "data/primary_studies/Callaway_2019/derivative/Callaway_et_al_2019_depthseries.csv")
write_csv(sites, "data/primary_studies/Callaway_2019/derivative/callaway_et_al_2019_sites.csv")
write_csv(impacts, "data/primary_studies/Callaway_2019/derivative/callaway_et_al_2019_impacts.csv")

