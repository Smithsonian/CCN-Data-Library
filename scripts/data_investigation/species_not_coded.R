# Not in species table

species2 <- read_csv("data/CCRCN_V2/species.csv") %>%
  filter(complete.cases(species_code))

cores2 <- read_csv("data/CCRCN_V2/cores.csv", guess_max=10000)

all_core_studies <- unique(cores2$study_id)

not_in_species <- all_core_studies[!(all_core_studies %in% species2$study_id)]

# Percent not coded
length(not_in_species) / length(all_core_studies) * 100
