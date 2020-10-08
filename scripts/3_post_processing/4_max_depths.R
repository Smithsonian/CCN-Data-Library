# Max depths

cores <- read_csv("data/CCRCN_V2/cores.csv", guess_max=6206)
depthseries <- read_csv("data/CCRCN_V2/depthseries.csv", guess_max = 42698)

max_depths <- depthseries %>% 
  filter(complete.cases(depth_max)) %>% 
  group_by(study_id, core_id) %>% 
  summarise(max_depth = max(depth_max, na.rm = T))

cores_w_max_depth <- cores %>% 
  left_join(max_depths, by=c("study_id", "core_id"))

write_csv(cores_w_max_depth, "data/CCRCN_V2/cores.csv")
