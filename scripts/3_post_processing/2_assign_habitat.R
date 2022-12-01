# Coastal Carbon Research Coordination Network ####
# Database Guidance V2

# Synthesis Post-Processing Script
# Assign habitat to each core in the synthesis
# contact: James Holmquist (HolmquistJ@si.edu) or Jaxine Wolfe (wolfejax@si.edu)
# require(tidyverse)

methods2 <- ccrcn_synthesis$methods
cores2 <- ccrcn_synthesis$cores
depthseries2 <- ccrcn_synthesis$depthseries
species2 <- ccrcn_synthesis$species

# read in our taxa database with associated habitat
# spref <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv")

# synthspecies <- species2 %>% 
#   select(-habitat, -code_type) %>% 
#   left_join(spref)

# Habitat specific species are now ASSIGNED IN THE HOOK SCRIPTS
# First classify habitat according to the big habitat-specific syntheses
# sanderman <- read_csv("docs/post_processing/habitat_assignment/Sanderman_2018_cores.csv") %>% 
#   select(study_id, core_id) %>% 
#   mutate(habitat2 = "mangrove")
# 
# forquean <- read_csv("docs/post_processing/habitat_assignment/Fourqurean_2012_cores.csv") %>% 
#   select(study_id, core_id) %>% 
#   mutate(habitat2 = "seagrass")
# 
# two_big_syntheses <- bind_rows(sanderman, forquean)

# classify core habitat by salinity and vegetation 
cores_sal_veg_habitat <- cores2 %>% 
  # habitats defined explicitly in hook scripts will be given their own category
  rename(habitat1 = habitat) %>% 
  # Good classification
  mutate(habitat3 = case_when(vegetation_class == "emergent" ~ "marsh",
                              vegetation_class == "seagrass"~ "seagrass",
                              vegetation_class %in% c("mudflat", "unvegetated") ~ "unvegetated",
                              vegetation_class == "scrub shrub" ~ "scrub shrub", 
                              TRUE ~ NA_character_),
        # Kind of junk classification, but best we can do if none is better
        habitat7 = ifelse(vegetation_class %in% c("forested", "forested to shrub", "forested to emergent") &
                            salinity_class %in% c("saline", "brackish", "mesohaline", "brackish to fresh", "polyhaline", 
                                                  "estuarine C-CAP", "estuarine", "bracish to saline", "intermediate salinity"),
                          # If it's from Nhalick and fennesey and it's forested then it's U.S.
                          ifelse(study_id == "Nahlik_and_Fennessy_2016", 
                                 # Cavanaugh et al 2014, Use the max northern limit for mangroves observed by Cavanaugh
                                 ifelse(latitude <= 29.75, "mangrove", "swamp"), 
                                 NA),  
                          NA)) %>%
  select(study_id:core_id, vegetation_class, salinity_class, habitat1, habitat3, habitat7)

# classify core habitat by species
cores_species_habitat <- species2 %>% 
  mutate(habitat = factor(habitat, levels = rev(c("unvegetated", "algal mat", "seagrass", "marsh", "scrub shrub", "swamp", "mangrove")))) %>% 
  arrange(study_id, core_id, habitat) %>% 
  group_by(study_id, core_id) %>% 
  summarise(habitat4 = first(habitat)) %>% 
  mutate(habitat4 = as.character(habitat4))

# conditional assignment of habitat based on pattern matching in core ID
habitat_cases <- cores2 %>% select(study_id, site_id, core_id) %>% 
  mutate(habitat6 = case_when(grepl("mangrove", site_id) | grepl("mangrove", core_id) ~ "mangrove",
                              grepl("swamp", site_id) | grepl("swamp", core_id) | grepl("forest", site_id) | grepl("forest", core_id) ~ "swamp",
                              grepl("marsh", site_id) | grepl("marsh", core_id) ~ "marsh",
                              grepl("seagrass", site_id) |  grepl("seagrass", core_id) ~ "seagrass",
                              TRUE ~ NA_character_)) 

# manually classify the cores from certain studies
study_habitat_ids_manual <- cores2 %>% select(study_id, site_id, core_id) %>% 
  mutate(habitat5 = case_when(study_id %in% c("Gonneea_et_al_2018", "Nuttle_1996", "Smith_et_al_2015", "Thorne_et_al_2015") ~ "marsh",
                              study_id == "Campbell_et_al_2015" ~ "seagrass", 
                              study_id == "Trettin_et_al_2017" ~ "mangrove",
                              TRUE ~ NA_character_))

# compare all the classification methods
habitat_comparison <- cores_sal_veg_habitat %>% # veg and sal defined habitat
  # full_join(two_big_syntheses) %>% # synthesis studies
  left_join(cores_species_habitat) %>% # species defined habitat
  left_join(study_habitat_ids_manual) %>% # manual classifications
  left_join(habitat_cases) %>% # fuzzy matching
  select(study_id, site_id, core_id, vegetation_class, salinity_class, 
         habitat1, 
         # habitat2, # assigned in hook scripts now
         habitat3, habitat4, habitat5, habitat6, habitat7)


# investigate the NA habitats left over
na_habitat_comparison <- filter(habitat_comparison, is.na(habitat1) & 
                                  # is.na(habitat2) & 
                                  is.na(habitat3) & 
                                  is.na(habitat4) & is.na(habitat5) & is.na(habitat6) & is.na(habitat7))
# Seems pretty reasonable to leave these NA's
# there are some species described that haven't been associated with a habitat in our species-habitat table

habitat_final <- habitat_comparison %>% 
  select(study_id, core_id, habitat1:habitat6) %>%
  gather(key="habitat_assignment_method",
         value = "habitat", -c(study_id, core_id)) %>% 
  arrange(study_id, core_id, habitat_assignment_method) %>% 
  filter(complete.cases(.)) %>% 
  group_by(study_id, core_id) %>% 
  summarise(habitat = first(habitat),
            habitat_assignment_method = first(habitat_assignment_method)) %>% 
  mutate(habitat_assignment_method = recode(habitat_assignment_method, 
                                            "habitat1" = "habitat explicitly defined in original hook script",
                                            "habitat2" = "study is from a habitat specific synthesis",
                                            "habitat3" = "habitat determined from vegetation_class",
                                            "habitat4" = "determined based on species presence",
                                            "habitat5" = "study-level habitat manually coded by manager",
                                            "habitat6" = "partial text matching used to detect habitat descriptions in site_id or core_id",
                                            "habitat7" = "habitat determined from vegetation_class, salinity_class, and latitude for forested wetlands"))


cores_with_habitat <- cores2 %>% 
  select(-habitat) %>% # drop the original habitat column
  left_join(habitat_final) %>% 
  select(-habitat_assignment_method) # more for internal use

if(length(which(is.na(cores_with_habitat$habitat))) > 0){
  write_csv(filter(cores_with_habitat, is.na(habitat)), "data/QA/no_habitat_cores.csv")
}


# write to synthesis
ccrcn_synthesis$cores <- cores_with_habitat
# write_csv(cores_with_habitat, "data/CCRCN_synthesis/derivative/CCRCN_cores.csv")

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
