## CCRCN Data Library ####

## test conditional attributes of each study and record output

# prepare workspace
library(tidyverse)

source("scripts/1_data_formatting/qa_functions.R") 
# will be using the testConditional() fxn

guidance <- read_csv("docs/ccrcn_database_structure.csv")

# read in synthesis
methods_raw <- read_csv("data/CCRCN_synthesis/CCRCN_methods.csv")
cores_raw <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max = 7000)
ds_raw <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 50000)
species_raw <- read_csv("data/CCRCN_synthesis/CCRCN_species.csv")

# test conditional attributes across all tables for each study

table_names <- c("methods", "cores", "depthseries", "species") 
# impacts table does have any conditional attributes

ids <- unique(cores_raw$study_id) # studies to test
results <- data.frame() # empty df to populate

for(id in ids){
  
  # subset tables
  methods <- methods_raw %>% filter(study_id == id) %>% select_if(function(x) {!all(is.na(x))})
  cores <- cores_raw %>% filter(study_id == id) %>% select_if(function(x) {!all(is.na(x))})
  depthseries <- ds_raw %>% filter(study_id == id) %>% select_if(function(x) {!all(is.na(x))})
  species <- species_raw %>% filter(study_id == id) %>% select_if(function(x) {!all(is.na(x))})
  
  temp_results <- data.frame(study_id = id, test_conditional = testConditional(table_names))
  
  results <- bind_rows(results, temp_results) %>% 
    filter(!grepl("all conditional attributes present", test_conditional))
}

results_clean <- results %>% 
  separate(test_conditional, into = c('table', 'test_conditional'), sep = ": ") %>% 
  mutate(table = gsub("\\s*\\([^\\)]+\\)","", as.character(table))) %>% 
  arrange(study_id)

write_csv(results_clean, "data/QA/missing_conditional_attributes.csv")

