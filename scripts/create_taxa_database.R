## CCRCN Data Library ####

## Create a taxa database 

library(tidyverse)

source("scripts/1_data_formatting/curation_functions.R")

species <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_species.csv") %>%
  filter(code_type != "description")
 
# create a unique taxa list to resolve
taxa <- unique(sort(species$species_code))

# they need to filter for genus species 

resolved <- resolveTaxa(taxa)

# Unresolved
# "Arrow arum" => Peltandra virginica (Merill_1999 => Holmquist 2018)
# "Thassia hemprichii" => Thalassia hemprichii (Agawin_et_al_1996 => Forquerean)

write_csv(resolved, "docs/CCRCN_taxa_database.csv")


### MWG Taxa Workflow

# # resolve taxa names using GNR
# taxa <- sort(unique(species$species))
# 
# taxa_index <- which(!(taxa %in% taxa_db$species))
# 
# if(length(taxa_index) > 0){
#   
#   taxa_resolved <- resolveTaxa(taxa[taxa_index])
#   
#   clean_resolved <- taxa_resolved %>% select(user_supplied_name, matched_name2) %>% 
#     rename(species = user_supplied_name,
#            resolved_species = matched_name2)
#   # add entries to the taxa database
#   updated_taxa_db <- bind_rows(taxa_db, clean_resolved) %>% arrange(species)
#   write_csv(updated_taxa_db, "resources/MWG_taxa_database.csv")
#   
#   # join resolved names to species table
#   final_species <- left_join(species, updated_taxa_db) %>%
#     select(-species) %>%
#     rename(species_code = resolved_species) %>%
#     select(study_id, site_id, chamber_id, species_code, everything())
#   # colname "species" is misleading because some taxa are only classified at the family level
# } else {
#   print("No new taxa.")
#   # correct species codes with resolved taxa in the database
#   final_species <- left_join(species, taxa_db) %>%
#     select(-species) %>%
#     rename(species_code = resolved_species) %>%
#     select(study_id, site_id, chamber_id, species_code, everything())
# }
# 
# # correct the 'unpublished' study IDs that have associated published theses
# study_species <- final_species %>% 
#   left_join(recoded_ids) %>% 
#   mutate(study_id = ifelse(!is.na(new_study_id), new_study_id, study_id)) %>%
#   select(-new_study_id)
