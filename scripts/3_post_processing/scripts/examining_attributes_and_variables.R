# Variables
library(tidyverse)

depths <- read_csv("CCRCN/CCRCN_depthseries.csv",
                   guess_max=38878)

guidance <- read_csv("converting_v1p2_to_v2.csv")

which_studies <- depths %>% 
  gather(key="attribute", value="value", -study_id) %>% 
  filter(!is.na(value)) %>% 
  select(-value) %>% 
  distinct()

View(which_studies)

how_many <- which_studies %>% 
  group_by(attribute) %>%
  summarise(n=n()) %>% 
  arrange(-n)

not_in_guidence <- how_many %>% 
  filter(! (attribute %in% c(guidance$attribute_name_v1p2, 
                             guidance$attribute_name)))

View(not_in_guidence)
