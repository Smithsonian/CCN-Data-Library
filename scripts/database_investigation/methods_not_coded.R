## CCRCN Data Library ####
## Database investigation

# How many studies don't have materials and methods coded?

library(tidyverse)

methods2 <- read_csv("data/CCRCN_synthesis/original/CCRCN_methods.csv") # methods
cores2 <- read_csv("data/CCRCN_synthesis/original/CCRCN_cores.csv", guess_max=10000)

all_core_studies <- unique(cores2$study_id)

not_in_methods <- all_core_studies[!(all_core_studies %in% methods2$study_id)]
# studies lacking methods are largely from the data synthesis hooks

# Percent not coded
length(not_in_methods) / length(all_core_studies) * 100
