# How many studies don't have materials and methods coded?

methods2 <- read_csv("CCRCN_v2/methods.csv") # methods
cores2 <- read_csv("CCRCN_v2/cores.csv", guess_max=5603)

all_core_studies <- unique(cores2$study_id)

not_in_methods <- all_core_studies[!(all_core_studies %in% methods2$study_id)]

# Percent not coded
length(not_in_methods) / length(all_core_studies) * 100
