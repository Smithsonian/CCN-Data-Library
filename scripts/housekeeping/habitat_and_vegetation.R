## CCRCN Data Library
## Jaxine Wolfe, wolfejax@si.edu

# Investigate vegetation classes and associated habitats in the CCN cores table

library(tidyverse)

synth_cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max = 7000)
synth_species <- read_csv("data/CCRCN_synthesis/CCRCN_species.csv", guess_max = 7000)
synth_ds <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 50000)

# look at differences in the assignment of vegetation and habitat
mangrove <- synth_cores %>%
  filter(habitat == "mangrove") %>% 
  distinct(study_id, vegetation_class, habitat)

veghab <- synth_cores %>%
  distinct(study_id, vegetation_class, habitat) %>% 
  arrange(habitat)

# which species were not assigned a habitat
no_sp_habitat <- synth_species %>% 
  filter(is.na(habitat)) %>% 
  filter(code_type != 'description' | is.na(code_type)) %>%
  distinct(study_id, species_code) %>% arrange(species_code)
unique(no_sp_habitat$study_id)

# studies: "Kauffman_et_al_2020"  "StLaurent_et_al_2020" "Osland_et_al_2016"    "Baustian_et_al_2021"  "Merrill_1999" 

# species
# "Acer circinatum"           "Atriplex patula"           "Crassostrea virginica"     "Elodea canadensis"         "Gaultheria shallon"       
# "Grindelia integrifolia"    "Impatiens sp."             "Juncus romerianus"         "Lilaeopsis occidentalis"   "Nanozostera japonica"     
# "Nuphar advena"             "Polygonum persicaria"      "Polystichum munitum"       "Pontederia cordata"        "Rosa sp."                 
# "Rubus parviflorus"         "Rubus spectabilis"         "Salix sitchensis"          "Scheonoplectus americanus" "Schoenoplectus pungens"   
# "Schoenoplectus sp."        "Scirpus spp"               "Sparganium eurycarpum" 


# check out the habitat assignment in the species vs the cores table

hab_species <- synth_species %>% 
  drop_na(habitat) %>% 
  distinct(study_id, site_id, core_id, habitat) %>% 
  rename(sp_habitat = habitat)

hab_cores <- synth_cores %>% distinct(study_id, site_id, core_id, habitat) %>% 
  rename(core_habitat = habitat)

hab_compare <- full_join(hab_species, hab_cores) %>% 
  mutate(hab_match = ifelse(sp_habitat == core_habitat, TRUE, FALSE)) %>% 
  filter(hab_match == FALSE | is.na(hab_match)) %>% 
  arrange(study_id)

# 50 ish habitat assignments that contradict each other 
# the rest (6000+ cases) are assigned habitats that exist in one table but not the other
# sometimes habitat is assigned for different sites within a study that don't overlap

# need to revisit how habitat is assigned for each table...
# species table habitat is assigned in the hook scripts
# specifically in updateTables() referencing species-habitat-classification-JH-20200824.csv
# core habitat is assigned during post-processing (using the species table?)

# I think we should only have a habitat column in the cores table
# but there should be a lookup table linking species in our database to habitats that the habitat assignment script uses for reference

library(leaflet)

synth_cores %>% filter(study_id == 'Duncan_et_al_2016') %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude)

## MISC

ds <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_depthseries.csv", guess_max = 60000)
c14 <- ds %>% select(contains("_id"), depth_min, depth_max, contains("c14"), delta_c13) %>% drop_na(c14_age, delta_c13)

