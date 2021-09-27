## CCRCN Data Library
## Jaxine Wolfe, wolfejax@si.edu

# Generate database stats

library(tidyverse)

synth_cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max = 7000)

country_smry <- synth_cores %>% 
  count(country, name = "core_count") %>% 
  arrange(core_count)
