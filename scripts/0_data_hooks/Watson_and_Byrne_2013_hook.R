# Coastal Carbon Research Coordination Network
# This script curates Watson_and_Byrne_2013 age depth data extracted from figures
# and joins with carbon stock data from the clearinghouse 
# Contact: lonnemanm@si.edu

# Watson, Elizabeth Burke, and Roger Byrne. "Late Holocene Marsh Expansion in Southern San Francisco Bay, California." 
# Estuaries and coasts 36.3 (2013): 643-653.

## Workspace prep ###################

library(tidyverse)
library(RefManageR)
library(readxl)
library(stringr)

## Read in data #####################
age_depth_data <- read_excel("./data/primary_studies/Watson_Byrne_2013/original/watson_byrne_2013_age_depth.xlsx")
carbon_stock_data <- read_csv("./data/primary_studies/Watson_Byrne_2013/intermediate/wb_depthseries_data.csv")
cores <- read_csv("./data/primary_studies/Watson_Byrne_2013/intermediate/wb_core_data.csv")

## Curate data ######################

## ... depthseries data #############
# carbon stocks and age-depth information will be joined, 
# with each age-depth interval being it's own row (by a sample_id value)

depthseries <- age_depth_data %>%
  # core IDs need to be formatted to match the carbon stock data core IDs
  mutate(core_id = gsub(" ", "_", `Site (core)`)) %>%
  mutate(core_id = gsub("\\(", "", core_id)) %>%
  mutate(core_id = gsub("\\)", "", core_id)) %>%
  mutate(core_id = recode(core_id, 
                          "Plummer_Slough" = "Plummer_Slough_1")) %>%
  # rename depth (cm) to both depth_min and depth_max
  mutate(depth_min = `Depth (cm)`, 
         depth_max = `Depth (cm)`) %>%
  # rename variables to meet CCRCN guidelines
  rename(sample_id = `Lab No.`,
         c14_material = `Material dated`) %>%
  # I'm removing the modern age depth core 
  filter(c14_age_years_bp != "Modern") %>%
  # formatting the c14 age in years BP prior to separating 
  mutate(c14_age_years_bp = gsub("+-", "_", fixed=TRUE, c14_age_years_bp)) %>%
  # I'm assuming the first value is the c14 age and the second represents the standard deviation 
  separate(col="c14_age_years_bp", into=c("c14_age","c14_age_sd"), sep = "_") %>%
  # Assumptions: the min and max age values are at the two extremes of this range. 
  # The range divided by 2 is the sd
  # The middle value in the range is the age
  separate(col=`1σ_age_range_ce`, into=c("age_min","age_max"), sep = "–") %>%
  mutate(age_max = as.numeric(age_max), 
         age_min = as.numeric(age_min),
         c14_age = as.numeric(c14_age),
         c14_age_sd = as.numeric(c14_age_sd)) %>%
  mutate(age_sd = (age_max - age_min)/2) %>%
  mutate(age = age_max - age_sd) %>%
  mutate(study_id = "Watson_and_Byrne_2013") %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd, c14_material, age, age_sd) %>%
  # bind to carbon stock data
  bind_rows(carbon_stock_data) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, 
         dry_bulk_density, fraction_organic_matter, 
         c14_age:age_sd) %>%
  group_by(core_id) %>%
  # order it according to core id and depth
  arrange(depth_min, .by_group = TRUE) %>%
  # Filter out the core that does not have matching carbon stock data
  filter(core_id != "Alviso_1")

## ... Site data ###########
source("./scripts/1_data_formatting/curation_functions.R") 

site_boundaries <- cores %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude) 

site_boundaries <- create_multiple_geographic_coverages(site_boundaries)

# Not including sites as there is no site-level info available from the publication - this table will be automatically generated 

## Bibliography #######
doi <- "10.1007/s12237-013-9598"
study_id_value <- "Watson_and_Byrne_2013"

# Get bibtex citation from DOI
biblio_raw <- GetBibEntryWithDOI("10.1111/rec.12941")
biblio_df <- as.data.frame(biblio_raw)
study_citations <- biblio_df %>%
  rownames_to_column("key") %>%
  mutate(bibliography_id = study_id_value, 
         study_id = study_id_value,
         key = study_id_value,
         publication_type = "Article", 
         year = as.numeric(year)) %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -bibliography_id, -publication_type) %>%
  column_to_rownames("key")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Watson_Byrne_2013/final/watson_and_byrne_2013.bib")


## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

results <- test_core_relationships(cores, depthseries)

test_numeric_vars(depthseries)


## Write files #########
write_csv(depthseries, "./data/primary_studies/Watson_Byrne_2013/final/watson_and_byrne_2013_depthseries.csv")
write_csv(study_citations, "./data/primary_studies/Watson_Byrne_2013/final/watson_and_byrne_2013_study_citations.csv")
