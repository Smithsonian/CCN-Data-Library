# Coastal Carbon Research Coordination Network ####
# Database Guidance V2
# Synthesis Post-Processing Script

# Make corrections and align CCRCN synthesis data curated under v1.2 guidance to v2 guidance
# contact: James Holmquist (HolmquistJ@si.edu) or Jaxine Wolfe (wolfejax@si.edu)

library(tidyverse)
library(lubridate)

source("scripts/3_post_processing/scripts/qa_functions.R")

## 1. Prepare Workspace ####

# Load up data
# methods
methods <- read_csv("data/CCRCN_synthesis/CCRCN_methods.csv")
# cores
cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max=6206)
# depthseries
depthseries <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 42698)
# species
species <- read_csv("data/CCRCN_synthesis/CCRCN_species.csv")
# sites
sites <- read_csv("data/CCRCN_synthesis/CCRCN_sites.csv")
# impacts
impacts <- read_csv("data/CCRCN_synthesis/CCRCN_impacts.csv")

# guidance 
guidance <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/converting_v1p2_to_v2.csv")

# Species table fixes 
species_fixes <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/species-habitat-classification-JH-20200824.csv")

# Depth code fixes
core_depth_fixes <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/studies_revisited.csv") %>% 
  select(study_id, core_length_flag) %>% 
  rename(core_length_flag_correct=core_length_flag)

## 2. Prepare Tables for Update ####

# Fix the two different 210Pb in the depth series
depthseries_fixed <- depthseries %>% 
  mutate(pb214_activity = ifelse(is.na(pb214_activity),
                                 (pb214_activity_352keV+pb214_activity_295keV)/2,
                                 pb214_activity),
         # Assume errors are 100% correlated
         pb214_activity_se = ifelse(is.na(pb214_activity_se),
                                 (pb214_activity_se_352keV+pb214_activity_se_295keV)/2,
                                 pb214_activity_se))

# Fix habitat classification
# separate these out

# Fix dates
cores_fixed <- cores %>% 
  # correct Breithaupt core date
  mutate(core_date = ifelse(study_id == "Breithaupt_et_al_2020", 
                            as.character(strptime(core_date, format = "%m/%d/%Y")), core_date)) %>%
  # separate out year month day
  mutate(year = year(ymd(core_date)), 
         month = month(ymd(core_date)),
         day = day(ymd(core_date))) %>%
  # resolve cases where core date is NA but core year month or day are provided
  mutate(year = ifelse(is.na(year) & !is.na(core_year), yes = core_year, no = year),
         month = ifelse(is.na(month) & !is.na(core_month), yes = core_month, no = month),
         day = ifelse(is.na(day) & !is.na(core_day), yes = core_day, no = day)) %>%
  select(-core_date) %>% # get rid of date
  # Fix depths
  left_join(core_depth_fixes) %>% # join with fixed core depths
  mutate(core_length_flag = ifelse(is.na(core_length_flag),
                                   core_length_flag_correct,
                                   core_length_flag)) %>% 
  select(-core_length_flag_correct) 
  # left_join(habitat2)

# Fix species
species_fixed <- species %>%
  left_join(species_fixes) %>% 
  mutate(species_code = ifelse(!is.na(recode_as), 
                               recode_as, 
                               species_code)) %>% 
  select(-c(recode_as, notes))


## 3. Update Synthesis Tables ####

# Rename attributes from guidance v1.2 to 2
list_of_tables <- list(methods = methods,
                       cores = cores_fixed,
                       depthseries = depthseries_fixed,
                       species = species_fixed)

# Iterate through list of tables
for (i in 1:length(list_of_tables)) {
  # Subset guidance by name of the table 
  category <- names(list_of_tables[i])
  
  guidance_subset <- filter(guidance, heirarchy == category)
  guidance_subset_actions <- filter(guidance_subset, !is.na(action))
  
  # If the guidance involves a rename, rename it
  if (nrow(guidance_subset_actions) > 0) {
    for (j in 1:nrow(guidance_subset_actions)) {
      if (guidance_subset_actions$action[j] == "rename") {
        
        # Old names
        if (guidance_subset_actions$attribute_name_v1p2[j] %in% names(list_of_tables[[i]])) {
          old_column <- list_of_tables[[i]][,guidance_subset_actions$attribute_name_v1p2[j]][[1]]
          
          # For any not NA entries in the old category, 
          list_of_tables[[i]][!is.na(old_column),guidance_subset_actions$attribute_name[j]] <- 
            old_column[!is.na(old_column)] # write them over to a column with a new category
        }
      }
    }
  }
  
  # Classes to include
  include_these <- unique(guidance_subset$attribute_name[guidance_subset$attribute_name %in% names(list_of_tables[[i]])])
  
  # Remove attributes not in guidance 2
  # ... and order columns as in guidance v2
  renamed_and_reordered <- list_of_tables[[i]][,include_these]
  
  list_of_tables[[i]] <- renamed_and_reordered
} 

for (i in 1:length(list_of_tables)) {
  write_csv(list_of_tables[[i]], paste("data/CCRCN_V2/", names(list_of_tables)[i], ".csv", sep=""))
}

# Site and Impact tables weren't updated but they should be written to the CCRCN_V2 folder
write_csv(sites, "data/CCRCN_V2/sites.csv")
write_csv(impacts, "data/CCRCN_V2/impacts.csv")

