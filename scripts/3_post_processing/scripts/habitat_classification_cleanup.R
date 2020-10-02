methods2 <- read_csv("data/CCRCN_V2/methods.csv") # methods
cores2 <- read_csv("data/CCRCN_V2/cores.csv", guess_max=6206)
depthseries2 <- read_csv("data/CCRCN_V2/depthseries.csv", guess_max = 42698)
species2 <- read_csv("data/CCRCN_V2/species.csv")

# First classify habitat according to the big habitat-specific syntheses
sanderman <- read_csv("scripts/3_post_processing/tables/input_files/synthesis_studies/Sanderman_2018_cores.csv") %>% 
  select(study_id, site_id, core_id) %>% 
  mutate(habitat1 = "mangrove")

forquean <- read_csv("scripts/3_post_processing/tables/input_files/synthesis_studies/Fourqurean_2012_cores.csv") %>% 
  select(study_id, site_id, core_id) %>% 
  mutate(habitat1 = "seagrass")

two_big_syntheses <- bind_rows(sanderman, forquean)

# Cores classified by salinity and vegetation 
cores_sal_veg_habitat <- cores2 %>% 
  # Good classification
  mutate(habitat2 = ifelse(vegetation_class == "emergent", "marsh",
                           ifelse(vegetation_class == "seagrass", "seagrass",
                                  ifelse(vegetation_class %in% c("mudflat", "unvegetated"),
                                         "unvegetated",
                                         ifelse(vegetation_class == "scrub shrub", "scrub/shrub", NA))))) %>% 
         # Kind of junk classification, but best we can do if none is better
         #habitat4 = ifelse(vegetation_class %in% c("forested", "forested to shrub", "forested to emergent"),
        #                   ifelse(salinity_class %in% c("saline", "brackish", "mesohaline",
        #                                                "brackish to fresh", "polyhaline", "estuarine C-CAP",
        #                                                "estuarine", "bracish to saline", "intermediate salinity"
        #                       
        # , NA), "mangrove", "swamp") 
        #,NA)) %>% 
  select(study_id:core_id, vegetation_class, salinity_class, habitat2)

cores_species_habitat <- species2 %>% 
  mutate(habiat = factor(habitat, levels = rev(c("unvegetated", "algal mat", "seagrass", "marsh", "scrub/shrub", "swamp", "mangrove")))) %>% 
  arrange(study_id, core_id, habiat) %>% 
  group_by(study_id, core_id) %>%
  summarise(habitat3 = first(habitat)) %>% 
  mutate(habitat3 = as.character(habitat3))

habitat_comparison <- full_join(two_big_syntheses, cores_sal_veg_habitat) %>%
  full_join(cores_species_habitat) %>% 
  select(study_id:core_id, vegetation_class, salinity_class, habitat1, habitat2, habitat3)

# Any habitat still NA
habitat_comparison_NA <- filter(habitat_comparison, is.na(habitat1) & is.na(habitat2) & is.na(habitat3))

study_habitat_ids_manual <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/missing_habitat_manual_cleanup.csv")

habitat_comparison <- habitat_comparison %>% 
  left_join(study_habitat_ids_manual)

habitat_comparison <- habitat_comparison %>% 
  mutate(habitat5 = ifelse(grepl("mangrove", site_id) | grepl("mangrove", core_id), "mangrove",
                           ifelse(grepl("swamp", site_id) | grepl("swamp", core_id) | grepl("forest", site_id) | grepl("forest", core_id), "swamp",
                                  ifelse(grepl("marsh", site_id) | grepl("marsh", core_id), "marsh",
                                         ifelse(grepl("seagrass", site_id) |  grepl("seagrass", core_id), "seagrass", NA)))))

habitat_comparison_NA2 <- filter(habitat_comparison, is.na(habitat1) & is.na(habitat2) & is.na(habitat3) & is.na(habitat4) & is.na(habitat5))

# Seems pretty reasonable to leave these NA's

habitat_final <- habitat_comparison %>% 
  select(study_id:core_id, habitat1:habitat5) %>%
  gather(key="habitat_assignment_method",
         value = "habitat", -c(study_id:core_id)) %>% 
  arrange(study_id, site_id, core_id, habitat_assignment_method) %>% 
  filter(complete.cases(.)) %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(habitat = first(habitat),
            habitat_assignment_method = first(habitat_assignment_method)) %>% 
  mutate(habitat_assignment_method = recode(habitat_assignment_method, 
                                            "habitat1" = "study is from a habitat specific synthesis",
                                            "habitat2" = "habitat determined from vegetation_class",
                                            "habitat3" = "determined based on species presence",
                                            "habitat4" = "study-level habitat manually coded by manager",
                                            "habitat5" = "partial text matching used to detect habitat descriptions in site_id or core_id"))

write_csv(habitat_final, "cleaned_up_habitat_classifications.csv")
