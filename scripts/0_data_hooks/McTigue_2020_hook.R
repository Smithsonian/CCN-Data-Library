# Dataset: Carbon accumulation rates in a salt marsh over the past two millennia
# 
# Authors: Nathan  McTigue  <mctigue@utexas.edu>, Jenny  Davis, Antonio  Rodriguez, Brent  McKee, Anna  Atencio and Carolyn  Currin
# 
# Any use of this dataset must include a citation. The DOI: 10.25573/serc.11421063
# 
# The data release contains tidal wetland soil carbon profiles. The data itself is housed in four separate .csv files which can by joined by the core_id and/or site_id attributes. 
# 
# mctigue_et_al_2019_materials_and_methods.csv - Contains information on materials and methods broken down by study 
# mctigue_et_al_2019_cores.csv - Contains positional and descriptive information on core locations.
# mctigue_et_al_2019_depthseries.csv - Contains raw depth-series information for all cores.
# mctigue_et_al_2019_species.csv - Contains information on the dominant plant species at coring locations.
# 
# metadata.xml - Contains a full suite of descriptive metadata with all attribute and variables defined, units clarified, and geographic, temporal, and taxonomic coverages described according to Ecological Metadata Language Standards.
# 
# metadata.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
# 
# map.html - Is a map widget showing the geographic coverages described in the metadata. It can be accessed on its own or through metadata.html.
# 
# custom.css - Contains display information for metadata.html.
# 
# citations.bib - Citation for associated journal article and this data release in .BIB format. 

# Core TB_2LPb is being incorporated into TB_2L as sample ID rows
library(tidyverse)
library(RefManageR)
library(lubridate)

cores_raw <- read.csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_cores.csv")
depthseries_raw <- read.csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_depthseries.csv")
species_raw <- read_csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_species.csv")
methods_raw <- read_csv("./data/primary_studies/mctigue_2020/original/mctigue_et_al_2020_materials_and_methods.csv")

study_id_value <- "McTigue_et_al_2020"

depthseries_PB <- depthseries_raw %>%
  filter(study_id == "McTigue_et_al_2020b") %>%
  mutate(sample_id = paste(core_id, depth_min, sep="_"),
         core_id = "TB_2L",
         study_id = "McTigue_et_al_2020")

depthseries <- depthseries_raw %>%
  filter(study_id != "McTigue_et_al_2020b") %>%
  mutate(study_id = study_id_value) %>%
  bind_rows(depthseries_PB) %>%
  rename(fraction_carbon = fraction_carbon_measured) %>%
  select(-fraction_carbon_modeled, -mass_depth) %>%
  group_by(core_id, depth_min) %>%
  arrange(depth_min, .by_group=T) %>%
  select(study_id, site_id, core_id, depth_min, depth_max, sample_id, everything()) %>%
  ungroup()
  
cores <- cores_raw %>%
  mutate(core_year = year(core_date)) %>%
  filter(study_id != "McTigue_et_al_2020b") %>%
  mutate(study_id = study_id_value)
  
species <- species_raw  %>%
  mutate(species_code = paste(genus, species, sep=" ")) %>%
  select(study_id, site_id, core_id, species_code) %>%
  filter(core_id != "TB_2LPb") %>%
  mutate(study_id = study_id_value)

methods <- methods_raw %>%
  filter(study_id != "McTigue_et_al_2020b") %>%
  mutate(pb210_counting_method = "alpha",
         excess_pb210_rate = "mass accumulation",
         excess_pb210_model = "CFCS",
         study_id = study_id_value)

# Citations ####
if(!file.exists("data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_study_citations.csv")){
  
  bib <- ReadBib("./data/primary_studies/mctigue_2020/original/citations.bib")
  
  study_citations <- as.data.frame(bib) %>%
    mutate(bibliography_id = c("McTigue_et_al_2019_article", "McTigue_et_al_2020_data"),
           study_id = study_id_value,
           publication_type = c("associated", "primary")) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything()) %>%
    remove_rownames()
  
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020.bib")
  write_csv(study_citations, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_study_citations.csv")
  
}

## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names = c("methods", "cores", "depthseries", "species"), version = "1")
testTableVars(table_names = c("methods", "cores", "depthseries", "species"))

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
test_numeric_vars(depthseries)


# write files
write_csv(cores, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_cores.csv") 
write_csv(depthseries, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_species.csv")
write_csv(methods, "./data/primary_studies/mctigue_2020/derivative/mctigue_et_al_2020_methods.csv")

