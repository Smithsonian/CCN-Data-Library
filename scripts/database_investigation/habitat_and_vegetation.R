## CCRCN Data Library
## Jaxine Wolfe, wolfejax@si.edu

# Investigate vegetation classes and associated habitats in the CCN cores table

synth_cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max = 7000)

mangrove <- synth_cores %>%
  filter(habitat == "mangrove") %>% 
  distinct(study_id, vegetation_class, habitat)

veghab <- synth_cores %>%
  distinct(study_id, vegetation_class, habitat) %>% 
  arrange(habitat)

