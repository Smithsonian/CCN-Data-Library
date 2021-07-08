## CCRCN Data Library ####

## Create a taxa database 

library(tidyverse)

source("scripts/1_data_formatting/curation_functions.R")

# taxa_db <- read_csv("docs/CCRCN_taxa_database.csv")

species_habitats <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv") %>%
  select(species_code, code_type, habitat)

species <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_species.csv") %>%
  filter(code_type != "description") %>% 
  mutate(species_code = recode(species_code,
                               "Arrow arum" = "Peltandra virginica",
                               "Thassia hemprichii" = "Thalassia hemprichii")) %>% 
  mutate(species_code = str_split(species_code, ";")) %>% 
  unnest(species_code) %>% 
  mutate(species_code = trimws(species_code))
 
# extract taxa from species habitat table that aren't in the species synthesis table
more_taxa <- species_habitats %>%
  filter(!(species_code %in% unique(species$species_code))) %>% 
  filter(species_code != "Avicennia marina; Sonneratia alba") %>% # from Sanderman
  filter(code_type != "description")

# create a unique taxa list to resolve
taxa <- unique(sort(c(species$species_code, more_taxa$species_code)))

# resolve misspellings
resolved <- resolveTaxa(taxa)

# Unresolved
# "Arrow arum" => Peltandra virginica (Merill_1999 => Holmquist 2018)
# "Thassia hemprichii" => Thalassia hemprichii (Agawin_et_al_1996 => Forquerean)
# "Asppagus officinalis" => Ignore: doesn't exist in the synthesis but the correct spelling exists in the database
# "Typa domingensis" => correct spelling exists in the database (Callway 2019)

cleaned_taxa <- resolved %>%
  rename(resolved_taxa = matched_name2,
         data_source = data_source_title,
         species_code = user_supplied_name) %>%
  mutate(name_updated = ifelse(species_code != resolved_taxa, T, F)) %>% 
  select(species_code, resolved_taxa, data_source, score, name_updated)

# These following species are not recognized by any data source
# GNR drops species and leaves the genus 
# Avicennia corniculatum
# Schoenoplectus montevidensis
# Amphibolis australis
# Trapa natis => trapa natans? (which exists in database) Not in the synthesis: Ignore

taxa_merged <- ccrcn_taxa %>%
  left_join(species_habitats)
# habitat and code_type have to be assigned

# write finalized taxa database
write_csv(cleaned_taxa, "docs/CCRCN_taxa_database.csv")

### MWG Taxa Workflow

# # resolve taxa names using GNR
# taxa <- sort(unique(species$species_code))
# 
# taxa_index <- which(!(taxa %in% taxa_db$user_supplied_name))
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
