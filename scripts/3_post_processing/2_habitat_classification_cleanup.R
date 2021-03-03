# Coastal Carbon Research Coordination Network ####
# Database Guidance V2

# Synthesis Post-Processing Script
# Assign habitat to each core in the synthesis
# contact: James Holmquist (HolmquistJ@si.edu) or Jaxine Wolfe (wolfejax@si.edu)
require(tidyverse)

methods2 <- read_csv("data/CCRCN_V2/methods.csv") # methods
cores2 <- read_csv("data/CCRCN_V2/cores.csv", guess_max = 6206)
depthseries2 <- read_csv("data/CCRCN_V2/depthseries.csv", guess_max = 42698)
species2 <- read_csv("data/CCRCN_V2/species.csv")

# If habitat is defined explicitly, then it will be used
if ("habitat" %in% names(cores2)) {
  cores2 <- cores2 %>% rename(habitat1 = habitat)
} else {
  cores2 <- cores2 %>% mutate(habitat1 = NA_character_)
}

# First classify habitat according to the big habitat-specific syntheses
sanderman <- read_csv("scripts/3_post_processing/tables/input_files/synthesis_studies/Sanderman_2018_cores.csv") %>% 
  select(study_id, core_id) %>% 
  mutate(habitat2 = "mangrove")

forquean <- read_csv("scripts/3_post_processing/tables/input_files/synthesis_studies/Fourqurean_2012_cores.csv") %>% 
  select(study_id, core_id) %>% 
  mutate(habitat2 = "seagrass")

two_big_syntheses <- bind_rows(sanderman, forquean)

# Cores classified by salinity and vegetation 
cores_sal_veg_habitat <- cores2 %>% 
  # Good classification
  mutate(habitat3 = ifelse(vegetation_class == "emergent", "marsh",
                           ifelse(vegetation_class == "seagrass", "seagrass",
                                  ifelse(vegetation_class %in% c("mudflat", "unvegetated"),
                                         "unvegetated",
                                         ifelse(vegetation_class == "scrub shrub", "scrub/shrub", NA)))),
         # Kind of junk classification, but best we can do if none is better
        habitat7 = ifelse(vegetation_class %in% c("forested", "forested to shrub", "forested to emergent"),
                           ifelse(salinity_class %in% c("saline", "brackish", "mesohaline",
                                                        "brackish to fresh", "polyhaline", "estuarine C-CAP",
                                                        "estuarine", "bracish to saline", "intermediate salinity"
                               
         , NA), ifelse(latitude >= -25 & latitude <= 25, "mangrove", "swamp"), "swamp")  # from Giri et al. 2010 Giri, C.; Ochieng, E.; Tieszen, L.L.; Zhu, Z.; Singh, A.; Loveland, T.; Masek, J. & Duke, N. (2010). "Status and distribution of mangrove forests of the world using earth observation satellite data" (PDF). Global Ecology and Biogeography. 20 (1): 154–159. doi:10.1111/j.1466-8238.2010.00584.x. Retrieved 2012-02-08
         
        , NA)) %>%
  select(study_id:core_id, vegetation_class, salinity_class, habitat1, habitat3, habitat7)

cores_species_habitat <- species2 %>% 
  mutate(habiat = factor(habitat, levels = rev(c("unvegetated", "algal mat", "seagrass", "marsh", "scrub/shrub", "swamp", "mangrove")))) %>% 
  arrange(study_id, core_id, habiat) %>% 
  group_by(study_id, core_id, core_id) %>%
  summarise(habitat4 = first(habitat)) %>% 
  mutate(habitat4 = as.character(habitat4))

habitat_comparison <- cores_sal_veg_habitat %>% full_join(two_big_syntheses, by = c("study_id", "core_id")) %>%
  left_join(cores_species_habitat, by = c("study_id", "core_id")) %>% 
  select(study_id, site_id, core_id, vegetation_class, salinity_class, habitat1, habitat2, habitat3, habitat4, habitat7)

study_habitat_ids_manual <- read_csv("scripts/3_post_processing/tables/input_files/v1p2_to_v2/missing_habitat_manual_cleanup.csv")

habitat_comparison <- habitat_comparison %>% 
  left_join(study_habitat_ids_manual)

habitat_comparison <- habitat_comparison %>% 
  mutate(habitat6 = ifelse(grepl("mangrove", site_id) | grepl("mangrove", core_id), "mangrove",
                           ifelse(grepl("swamp", site_id) | grepl("swamp", core_id) | grepl("forest", site_id) | grepl("forest", core_id), "swamp",
                                  ifelse(grepl("marsh", site_id) | grepl("marsh", core_id), "marsh",
                                         ifelse(grepl("seagrass", site_id) |  grepl("seagrass", core_id), "seagrass", NA)))))

habitat_comparison_NA2 <- filter(habitat_comparison, is.na(habitat1) & is.na(habitat2) & is.na(habitat3) & is.na(habitat4) & is.na(habitat5) & is.na(habitat6) & is.na(habitat7))

# Seems pretty reasonable to leave these NA's
# StLaurent cores are probably marsh
# there are some species described that havent been assicated with a habitat in our species-habitat table

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
  left_join(habitat_final, by=c("study_id", "core_id")) %>%
  select(-habitat1) # Keshta habitat is defined in the hook script

# write to file!
write_csv(cores_with_habitat, "data/CCRCN_V2/cores.csv")

# ggplot(data = cores_with_habitat, aes(x=longitude, y=latitude)) +
#    geom_point(aes(color=habitat), alpha=0.4) +
#    theme_dark()

