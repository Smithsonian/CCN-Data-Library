## CCRCN Data Library

## Script queries library for mangrove cores for global blue carbon inventorying

library(tidyverse)

cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max=10000, col_types = cols()) %>% 
  mutate(habitat = ifelse(grepl("mangrove", core_notes), "mangrove", habitat)) %>% 
  filter(habitat == "mangrove")

write_csv(cores, "query/data/ccn_mangrove_cores.csv")
