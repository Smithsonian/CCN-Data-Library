## Synthesis Metrics

## This scripts will generate summary figures and stats for the current data synthesis 

cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max = 10000) %>% 
# spot fixes
  mutate(habitat = case_when(study_id %in% c("Boyd_et_al_2017", "Watson_and_Byrne_2013", "Thom_1992",
                                             # one of carlin's is actually mudflat
                                             "Drexler_et_al_2019", "Carlin_et_al_2021") ~ "marsh",
                             core_id == "W4" ~ "marsh",
                             core_id %in% c("W1", "W2", "W2") ~ "swamp",
                             study_id == "Kauffman_et_al_2020" & vegetation_class == "forested" ~ "swamp",
                             # "Krauss_et_al_2018" and "Ensign_et_al_2020" is a mix of marsh and swamp
                             T ~ habitat),
         country = case_when(study_id == "Nsombo_et_al_2016" ~ "Cameroon",
                             country == "Galapagos" ~ "Ecuador",
                             T ~ country)) %>% 
  drop_na(habitat) %>%
  filter(habitat != "peatland") %>%
  mutate(habitat = recode(habitat, "mudflat" = "unvegetated"))
  
cores %>% count(habitat) %>% 
  mutate(habitat = fct_reorder(habitat, n)) %>%  
  ggplot(aes(n, habitat)) + geom_col(fill = "darkgreen") +
  xlab("Number of Cores") + ylab("") +
  geom_text(aes(label = n), size = 3.5, hjust = -0.2) +
  xlim(0, 4000) +
  theme_classic(base_size = 15)

ggsave("data/library_metrics/figures/ccn_core_habitats_v100.jpg", width = 6, height = 6)

# plot
cores %>% 
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


# ccrcn_synthesis$cores %>% filter(is.na(habitat)) %>% distinct(study_id, core_id)
# 
# ccrcn_synthesis$cores %>% filter(habitat == "peatland") %>% distinct(study_id)

## map
library(leaflet)

map_cores <- cores %>%
  drop_na(longitude) %>% 
  filter(country != "Laos")

habs <- unique(map_cores$habitat)

# pal <- colorFactor(palette = 'Dark2', domain = map_cores$habitat)
pal <- colorFactor(viridis_pal(option = "H")(length(habs)), domain = habs)

leaflet(map_cores) %>%
  addTiles() %>%
  # addProviderTiles("") %>% 
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), radius = 1,
                   label = ~paste0(study_id, "; ", habitat), color = ~pal(habitat)) %>%
  addLegend(pal = pal, values = ~habitat)


