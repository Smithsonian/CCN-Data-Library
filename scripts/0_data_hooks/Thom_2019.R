# Dataset: Accretion rates of low intertidal salt marshes in the Pacific Northwest
# 
# Authors: Ronald M. Thom <thom.ronald@gmail.com>
#   
#   Any use of this dataset must include a citation. The DOI: 10.25573/data.10046189
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in five separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# thom_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# thom_2019_cores.csv - Contains positional and descriptive information on core locations.
# thom_2019_depthseries.csv - Contains raw depth-series information for all cores.
# thom_2019_species.csv - Contains information on the dominant plant species at coring locations.
# thom_2019_impacts.csv - Contains information on the anthropogenic impacts at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# thom_2019_associated_publication.bib - A citation in bibtex style for associated publications to this data release

library(tidyverse)
library(RefManageR)

depthseries_raw <- read_csv("./data/primary_studies/Thom_2019/original/thom_2019_depthseries.csv")
methods_raw <-read_csv("./data/primary_studies/Thom_2019/original/thom_2019_material_and_methods.csv")
species_raw <- read_csv("./data/primary_studies/Thom_2019/original/thom_2019_species.csv")
cores_raw <- read_csv("./data/primary_studies/Thom_2019/original/thom_2019_cores.csv")
  
# hook data
cores <- cores_raw %>%
  mutate(core_length_flag = "core depth limited by length of corer")

#RC update, classifying habitat
cores <- cores %>% 
  mutate(habitat = "marsh")

depthseries <- depthseries_raw %>%
  mutate(cs137_unit = "countsPerGramDryWeightPerHour",
         method_id = "single set of methods")

species <- species_raw %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(-c(genus, species))

methods <- methods_raw %>%
  mutate(study_id = "Thom_1992",
         method_id = "single set of methods")

# Create Citation ####

if(!file.exists("data/primary_studies/Thom_2019/derivative/thom_2019_study_citations.csv")){
  data_release_doi <- "10.25573/data.10046189"
  associated_pub_doi <- "10.1007/BF03160603"
  study_id <- "Thom_1992"
  
  data_bib_raw <- GetBibEntryWithDOI(c(data_release_doi, associated_pub_doi))
  
  study_citations <- as.data.frame(data_bib_raw) %>%
    mutate(study_id = study_id) %>%
    mutate(bibliography_id = c("Thom_1992_data", "Thom_1992_article"),
           publication_type = c("primary dataset", "associated source")) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Thom_2019/derivative/Thom_2019.bib")
  write_csv(study_citations, "data/primary_studies/Thom_2019/derivative/thom_2019_study_citations.csv")
  
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "species")

updated <- updateTables(table_names)

# save listed tables to objects

methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores %>% 
  # add core PB1 as a site level replicate of core PB3 to match depthseries table 
  add_row(study_id = "Thom_1992",
          site_id = "Padilla_Bay",
          core_id = "PB1",
          latitude = 48.50561,
          longitude = -122.4829,
          position_method = "other low resolution",
          position_notes = "site level position",
          vegetation_class = "emergent",
          vegetation_method = "measurment",
          year = 1991,
          core_length_flag = "core depth limited by length of corer",
          habitat = "marsh")
          
species <- updated$species

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)

# Write files ####
write_csv(depthseries, "data/primary_studies/Thom_2019/derivative/thom_2019_depthseries.csv")
write_csv(cores, "data/primary_studies/Thom_2019/derivative/thom_2019_cores.csv")
write_csv(methods, "data/primary_studies/Thom_2019/derivative/thom_2019_methods.csv")
write_csv(species, "data/primary_studies/Thom_2019/derivative/thom_2019_species.csv")
