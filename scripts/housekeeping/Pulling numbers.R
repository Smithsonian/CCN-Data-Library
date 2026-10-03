# Pulling numbers

library(tidyverse)

sources <- read_csv("data/CCN_synthesis/CCN_study_citations.csv")

length(unique(sources$bibliography_id))

sources_in_order <- sources %>% 
  arrange(year, author)

print(paste(unique(sources_in_order$bibliography_id), collapse = ", "))


cores <- read_csv("data/CCN_synthesis/CCN_cores.csv")

stocks_summary <- cores %>% 
  filter(! is.na(stocks_qual_code)) %>% 
  group_by(stocks_qual_code) %>% 
  summarise(n = n())

stocks_summary$pct <- stocks_summary$n / sum(stocks_summary$n) * 100

b_summary <- cores %>% 
  filter(! is.na(dates_qual_code)) %>% 
  group_by(dates_qual_code) %>% 
  summarise(n = n())

b_summary$pct <- b_summary$n / sum(b_summary$n) * 100

a_summary <- cores %>% 
  filter(! is.na(elevation_qual_code)) %>% 
  group_by(elevation_qual_code) %>% 
  summarise(n = n())

a_summary$pct <- a_summary$n / sum(a_summary$n) * 100

sum(a_summary$n)


us_vs_non_us <- cores %>% 
  mutate(us = ifelse(country == "United States", "United States", "non-us")) %>% 
  group_by(us) %>% 
  summarise(n = n())

us_vs_non_us$pct <- us_vs_non_us$n / sum(us_vs_non_us$n) *100

cores$habitat[grepl("scrub shrub", cores$habitat)] <- "scrub/shrub"

habitat_summary <- cores %>% 
  select(habitat) %>% 
  group_by(habitat) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

habitat_summary$pct <- habitat_summary$n / sum(habitat_summary$n) * 100

sources <- read_csv("data/CCN_synthesis/CCN_depthseries.csv", 
                    guess_max = 10000)

# Number of measurements
measurements <- sources %>% 
  select(study_id:depth_max, dry_bulk_density:fraction_carbon,
         cs137_activity, total_pb210_activity, ra226_activity, pb214_activity, bi214_activity, c14_age,
         be7_activity, marker_date) %>% 
  gather(key = "variable", value = "value", -c(study_id:depth_max)) %>% 
  filter(! is.na(value))

nrow(measurements)

# Number of depth increments
depth_incs <- measurements %>% 
  select(study_id:depth_max) %>% 
  distinct()

nrow(depth_incs)

# Number of countries
length(unique(cores$country))

# Taxa
taxa <- read_csv("data/CCN_synthesis/CCN_species.csv")

taxa_gs1 <- taxa %>% 
  filter(code_type == "Genus species") %>% 
  select(study_id:core_id) %>% 
  distinct() %>% 
  mutate(gs_present = "yes")

core_level <- taxa_gs1 %>% 
  filter(!is.na(core_id)) 

site_level <- taxa_gs1 %>% 
  filter(is.na(core_id)) %>% 
  select(-core_id)

cores_w_species <- cores %>% 
  left_join(core_level) %>% 
  left_join(site_level, by = c("study_id", "site_id")) %>% 
  mutate(gs_present = ifelse(is.na(gs_present.x), gs_present.y, gs_present.x))

cores_w_species_sum <- cores_w_species %>% 
  group_by(gs_present) %>% 
  summarise(n = n())

cores_w_species_sum$pct <- cores_w_species_sum$n / sum(cores_w_species_sum$n) * 100

taxa_gs <- taxa %>% 
  filter(code_type == "Genus species") %>% 
  group_by(species_code) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

taxa_gs$pct <- taxa_gs$n / sum(taxa_gs$n) * 100

# Impacts
impacts <- read_csv("data/CCN_synthesis/CCN_impacts.csv")

impacts_present <- impacts %>% 
  filter(impact_class != "natural") %>% 
  select(study_id:core_id) %>% 
  distinct() %>% 
  mutate(impacts_present = "yes")

core_level_imp <- impacts_present %>% 
  filter(!is.na(core_id)) 

site_level_imp <- impacts_present %>% 
  filter(is.na(core_id)) %>% 
  select(-core_id)

cores_w_imp <- cores %>% 
  left_join(core_level_imp) %>% 
  left_join(site_level_imp, by = c("study_id", "site_id")) %>% 
  mutate(impacts_present = ifelse(is.na(impacts_present.x), impacts_present.y, impacts_present.x))

cores_w_imp_sum <- cores_w_imp %>% 
  group_by(impacts_present) %>% 
  summarise(n = n())

cores_w_imp_sum$pct <- cores_w_imp_sum$n / sum(cores_w_imp_sum$n) * 100

imp_summary <- impacts %>% 
  filter(!is.na(impact_class),
         impact_class != "natural") %>% 
  group_by(impact_class) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

imp_summary$pct <- imp_summary$n / sum(imp_summary$n) * 100

# Cores deeper than 1 m
deeper_than_1m <- cores %>% 
  filter(complete.cases(max_depth)) %>% 
  mutate(gt_1m = ifelse(max_depth>= 100, "yes", "no")) %>% 
  group_by(gt_1m) %>% 
  summarise(n = n())

deeper_than_1m$pct <- deeper_than_1m$n / sum(deeper_than_1m$n) * 100

# Median depth of whole-profile cores
whole_profile_cores <- cores %>% 
  filter(complete.cases(core_length_flag),
         core_length_flag == "core depth represents deposit depth")

nrow(whole_profile_cores)

summary(whole_profile_cores$max_depth)

summary(cores$max_depth)


# 
cores$habitat[grepl("scrub shrub", cores$habitat)] <- "scrub/shrub"

habitat_summary2 <- cores %>% 
  filter(!is.na(stocks_qual_code)) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass")) %>% 
  select(habitat) %>% 
  group_by(habitat) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

habitat_summary2$pct <- habitat_summary2$n / sum(habitat_summary2$n) * 100


habitat_summary3 <- cores %>% 
  filter(!is.na(dates_qual_code)) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass")) %>% 
  select(habitat) %>% 
  group_by(habitat) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

habitat_summary3$pct <- habitat_summary3$n / sum(habitat_summary3$n) * 100
(habitat_summary3)

