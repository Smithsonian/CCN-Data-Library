# Make corrections and convert from from v 1.2 to 2

library(tidyverse)
library(lubridate)
source("scripts/3_post_processing/scripts/qa_functions.R")

# Load up data
# methods
methods <- read_csv("data/CCRCN_synthesis/CCRCN_methods.csv")

# sites
# cores
cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max=6206)

# depthseries
depthseries <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 42698)

# species
species <- read_csv("data/CCRCN_synthesis/CCRCN_species.csv")

# guidence 
guidence <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/converting_v1p2_to_v2.csv")

# Species table fixes 
species_fixes <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/species-habitat-classification-JH-20200824.csv")

# Depth code fixes
core_depth_fixes <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/studies_revisited.csv") %>% 
  select(study_id, core_length_flag) %>% 
  rename(core_length_flag_correct=core_length_flag)

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
habitat2 <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/cleaned_up_habitat_classifications.csv")


# Fix dates
cores_fixed <- cores %>% 
  mutate(year = year(ymd(core_date)),
         month = month(ymd(core_date)),
         day = day(ymd(core_date))) %>% 
  select(-core_date) %>%
  # Fix depths
  left_join(core_depth_fixes) %>% 
  mutate(core_length_flag = ifelse(is.na(core_length_flag),
                                   core_length_flag_correct,
                                   core_length_flag)) %>% 
  select(-core_length_flag_correct) %>% 
  left_join(habitat2)
  
# Fix species
species_fixed <- species %>%
  left_join(species_fixes) %>% 
  mutate(species_code = ifelse(!is.na(recode_as), 
                               recode_as, 
                               species_code)) %>% 
  select(-c(recode_as, notes))


# Rename attributes from guidence v1.2 to 2
list_of_tables <- list(methods = methods,
                       cores = cores_fixed,
                       depthseries = depthseries_fixed,
                       species = species_fixed)

# Iterate through list of tables
for (i in 1:length(list_of_tables)) {
  # Subset guidence by name of the table 
  category <- names(list_of_tables[i])
  
  guidence_subset <- filter(guidence, heirarchy == category)
  guidence_subset_actions <- filter(guidence_subset, !is.na(action))
  
  # If the guidence involves a rename, rename it
  if (nrow(guidence_subset_actions) > 0) {
    for (j in 1:nrow(guidence_subset_actions)) {
      if (guidence_subset_actions$action[j] == "rename") {
        
        # Old names
        if (guidence_subset_actions$attribute_name_v1p2[j] %in% names(list_of_tables[[i]])) {
          old_column <- list_of_tables[[i]][,guidence_subset_actions$attribute_name_v1p2[j]][[1]]
          
          # For any not NA entries in the old category, 
          list_of_tables[[i]][!is.na(old_column),guidence_subset_actions$attribute_name[j]] <- 
            old_column[!is.na(old_column)] # write them over to a column with a new category
        }
      }
    }
  }
  
  # Classes to include
  include_these <- unique(guidence_subset$attribute_name[guidence_subset$attribute_name %in% names(list_of_tables[[i]])])
  
  # Remove attributes not in guidence 2
  # ... and order columns as in guidence v2
  renamed_and_reordered <- list_of_tables[[i]][,include_these]
  
  list_of_tables[[i]] <- renamed_and_reordered
} 

for (i in 1:length(list_of_tables)) {
  write_csv(list_of_tables[[i]], paste("data/CCRCN_V2/", names(list_of_tables)[i], ".csv", sep=""))
}

ggplot(data = list_of_tables$cores, aes(x=longitude, y=latitude)) +
  geom_point(aes(color=habitat, shape=habitat), alpha=0.4) +
  theme_dark()
