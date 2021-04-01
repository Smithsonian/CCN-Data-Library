# How many studies don't have materials and methods coded?

methods2 <- read_csv("data/CCRCN_V2/methods.csv") # methods
cores2 <- read_csv("data/CCRCN_V2/cores.csv", guess_max=10000)

all_core_studies <- unique(cores2$study_id)

not_in_methods <- all_core_studies[!(all_core_studies %in% methods2$study_id)]

# Percent not coded
length(not_in_methods) / length(all_core_studies) * 100
