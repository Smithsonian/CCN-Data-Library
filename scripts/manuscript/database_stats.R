## CCRCN Data Library
## Jaxine Wolfe, wolfejax@si.edu

# Generate database stats

library(tidyverse)

synth_cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max = 7000)

country_smry <- synth_cores %>% 
  count(country, name = "core_count") %>% 
  arrange(core_count)

# cores per habitat type
cores_per_hab <- synth_cores %>%
  # drop_na(habitat) %>% 
  count(habitat, name = "core_count") %>% 
  arrange(core_count)

cores_per_hab %>% 
  # NA doesnt get picked up, needs reassignment or to be left out
  mutate(habitat = fct_reorder(habitat, core_count)) %>% 
ggplot(aes(x = habitat, y = core_count, fill = habitat)) +
  geom_col() +
  coord_flip() +
  theme_bw()
