## CCN Data Library
## Jaxine Wolfe <wolfejax@si.edu>

## Script to conjure up summary statistics for the data library

## Prepare Workspace ####

library(tidyverse)
# library(viridis)
library(RColorBrewer)

# read in synthesis data
cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", col_types = cols())
ds <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 80000)
impacts <- read_csv("data/CCRCN_synthesis/CCRCN_impacts.csv", guess_max = 2000)


country_smry <- cores %>% 
  count(country, name = "core_count") %>% 
  arrange(core_count)

# cores %>% filter(is.na(country)) %>% distinct(study_id)

# cores per habitat type
cores_per_hab <- cores %>%
  # drop_na(habitat) %>% 
  count(habitat, name = "core_count") %>% 
  arrange(core_count)

# cores_per_hab %>% 
#   # NA doesnt get picked up, needs reassignment or to be left out
#   mutate(habitat = fct_reorder(habitat, core_count)) %>% 
#   ggplot(aes(x = habitat, y = core_count, fill = habitat)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_brewer(palette = "BrBG") +
#   theme_bw()


all_habitat <- cores %>% 
  mutate(habitat = recode(habitat, 
                          "scrub shrub" = "scrub/shrub",
                          "mudflat" = "unvegetated")) %>% 
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))



## Reproducing 2020 Town Hall Stats ####

us_cores <- cores %>% filter(country == "United States")

# Investigate all cores ####

# core quality 
# stock_cores <- length(which(!is.na(us_cores$stocks_qual_code)))
# dated_cores <- length(which(!is.na(us_cores$dates_qual_code)))
# elevation_cores <- length(which(!is.na(us_cores$elevation_qual_code)))

# other metrics
length(unique(us_cores$core_id)) # 4041
length(unique(us_cores$admin_division)) # 23 states
min(unique(us_cores$year), na.rm = T) # oldest core: 1969

# us_ds <- ds %>% filter(core_id %in% unique(us_cores$core_id))
max(unique(us_cores$max_depth), na.rm = T)

us_cores %>% 
  filter(!is.na(max_depth)) %>%
  ggplot(aes(max_depth)) + 
  geom_density() + geom_rug() +
  ggtitle("Max Depth Distribution for U.S. Atlas Cores")
ggsave("data/library_metrics/figures/maxdepth_dist_US.jpg", width = 6, height = 6)

cores %>% 
  filter(!is.na(max_depth)) %>%
  ggplot(aes(max_depth)) + 
  geom_density() + geom_rug() +
  ggtitle("Max Depth Distribution for Atlas Cores")
ggsave("data/library_metrics/figures/maxdepth_dist.jpg", width = 6, height = 6)

## ... Habitats ####

# compare to habitats across all cores
# studies w a lot of unidentified habitats: Schile-Beers_and_Megonigal_2017, Osland_et_al_2016, Drexler_et_al_2019
us_habitats <- us_cores %>% 
  mutate(habitat = recode(habitat, 
                          # "scrub shrub" = "scrub/shrub",
                          "mudflat" = "unvegetated")) %>% 
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))

# plot
# us_habitats %>%
all_habitat %>%
  mutate(habitat = fct_reorder(habitat, percent)) %>% 
  filter(!is.na(habitat)) %>% 
  ggplot(aes(habitat, percent, fill = percent)) + 
  geom_col(fill = "darkgreen") + 
  # scale_color_brewer(palette = "BuGn") +
  xlab("Habitat Type") + ylab("Proportion of Cores (%)") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), size = 2.75, hjust = -0.2) +
  ylim(0, 50) +
  # theme_classic() +
  coord_flip() + 
  theme_classic(base_size = 17)  
  # theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("data/library_metrics/figures/cores_per_habitat.jpg", width = 6, height = 6)

# habitat area coverage?
  
## ... State Data Quality ####

corequal <- us_cores %>%
  filter(habitat == "mangrove") %>%
  select(core_id, stocks_qual_code, dates_qual_code, elevation_qual_code) %>% 
  pivot_longer(-core_id, names_to = "utility", values_to = "quality") %>% 
  drop_na(quality) %>% 
  group_by(core_id) %>% 
  summarize(data_type = paste0(quality, collapse = ", "),
            level = n()) %>% 
  mutate(level = case_when(grepl("A", data_type) ~ "Level 3",
                           grepl("B", data_type) ~ "Level 2",
                           grepl("C", data_type) ~ "Level 1")) %>% 

  # mutate(stocks_qual_code = ifelse(!is.na(stocks_qual_code), 1, 0),
  #        dates_qual_code = ifelse(!is.na(dates_qual_code), 1, 0),
  #        elevation_qual_code = ifelse(!is.na(elevation_qual_code), 1, 0)) %>% 
  # 
  # distinct() %>%
  mutate(data_tier = recode(level,
                            "Level 1" = "Carbon Stocks",
                            "Level 2" = "Carbon Stocks + Age Depth Model",
                            "Level 3" = "Carbon Stocks + Age Depth Model + Elevation"))
  # mutate(data_type = gsub(";", " +", data_type)) %>%
  # group_by(data_type, data_tier) %>%
  # tally()

# plot
corequal %>% 
  count(level, data_tier) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(level, percent, fill = data_tier)) + 
  geom_col() +
  theme(legend.position = "bottom") +
  ylab("Proportion of Cores (%)") + xlab("Data Quality Tiers")
  # theme_classic()
# ggsave("agu_town_hall/figures/data_quality.jpg")

# proportion of impact classes

impacts_prep <- impacts %>% drop_na(impact_class) %>% 
  mutate(impact_class_site = ifelse(is.na(core_id), impact_class, NA_character_)) %>% 
  rename(impact_class_core = impact_class)

us_impacts <- us_cores %>%
  left_join(impacts_prep) %>% 
  mutate(impact_class = coalesce(impact_class_core, impact_class_site)) %>% 
  group_by(impact_class) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))

us_impacts %>% 
  mutate(impact_class = fct_reorder(impact_class, percent)) %>% 
  filter(!is.na(impact_class)) %>% 
  ggplot(aes(impact_class, percent, fill = percent)) + 
  geom_col(fill = "darkgreen") + 
  # scale_color_brewer(palette = "BuGn") +
  xlab("Impact Type") + ylab("Proportion of Cores (%)") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), size = 2.75, hjust = -0.2) +
  ylim(0, 50) +
  # theme_classic() +
  coord_flip() + 
  theme_classic(base_size = 17)  
# theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("data/library_metrics/figures/cores_per_habitat.jpg", width = 6, height = 6)

