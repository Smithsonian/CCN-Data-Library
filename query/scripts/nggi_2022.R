library(tidyverse)
library(readxl)

climate_zones <- read_xlsx("query/data/inventorying_bib/Climate zones.xlsx") %>% 
  mutate(Color = tolower(Color),
         Color = recode(Color, "oragen" = "orange"),
         State = recode(State, "Delamare" = "Delaware")) %>% 
arrange(State)

dated_bib <- read_csv("query/data/dated_core_articles.csv") 

dated_cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max = 7000) %>% 
  filter(study_id %in% unique(dated_bib$study_id))

us_dated_cores <- dated_cores %>% filter(country == "United States")
sort(unique(us_dated_cores$study_id))

