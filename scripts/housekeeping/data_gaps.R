## CCRCN Data Library
## Date: 2023-08-23
## Jaxine Wolfe <wolfejax@si.edu>

library(tidyverse)

# QA maintenance check 

ds <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 50000)

# na depths
ds %>% filter(is.na(depth_max)) %>% distinct(study_id)