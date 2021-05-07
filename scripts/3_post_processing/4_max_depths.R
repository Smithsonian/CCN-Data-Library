# Max depths

cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max = 7000)
depthseries <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_depthseries.csv", guess_max = 50000)

max_depths <- depthseries %>% 
  filter(complete.cases(depth_max)) %>% 
  group_by(study_id, core_id) %>% 
  summarise(max_depth = max(depth_max, na.rm = T))

cores_w_max_depth <- cores %>% 
  left_join(max_depths, by=c("study_id", "core_id"))

write_csv(cores_w_max_depth, "data/CCRCN_synthesis/derivative/CCRCN_cores.csv")
