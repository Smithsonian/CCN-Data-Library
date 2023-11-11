## Synthesis Metrics

## This scripts will generate summary figures and stats for the current data synthesis 

ccrcn_synthesis$cores %>% 
  drop_na(habitat) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub",
                          "mudflat" = "unvegetated")) %>% 
  count(habitat) %>% mutate(habitat = fct_reorder(habitat, n)) %>%  
  ggplot(aes(n, habitat)) + geom_col(fill = "darkgreen") +
  xlab("Number of Cores") + ylab("") +
  geom_text(aes(label = n), size = 3.5, hjust = -0.2) +
  xlim(0, 4000) +
  theme_classic(base_size = 15)
ggsave("data/library_metrics/figures/ccn_core_habitats_v100.jpg", width = 6, height = 6)

# plot
ccrcn_synthesis$cores %>% 
  drop_na(habitat) %>% 
  mutate(habitat = recode(habitat, 
                          "scrub shrub" = "scrub/shrub",
                          "mudflat" = "unvegetated")) %>%
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n))) %>% 
  mutate(habitat = fct_reorder(habitat, percent)) %>% 
  filter(!is.na(habitat)) %>% 
  ggplot(aes(habitat, percent, fill = percent)) + 
  geom_col(fill = "darkgreen") + 
  # scale_color_brewer(palette = "BuGn") +
  xlab("Habitat Type") + ylab("Proportion of Cores (%)") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), size = 3.5, hjust = -0.2) +
  ylim(0, 50) +
  # theme_classic() +
  coord_flip() + 
  theme_classic(base_size = 15)  
# theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("data/library_metrics/figures/ccn_habitat_proportions_v100.jpg", width = 6, height = 6)


ccrcn_synthesis$cores %>% filter(is.na(habitat)) %>% distinct(study_id, core_id)

ccrcn_synthesis$cores %>% filter(habitat == "peatland") %>% distinct(study_id)
