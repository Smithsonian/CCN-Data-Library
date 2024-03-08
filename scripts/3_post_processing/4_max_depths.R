# Max depths

cores <- ccrcn_synthesis$cores
depthseries <- ccrcn_synthesis$depthseries

max_depths <- depthseries %>% 
  mutate(depth_min = case_when(is.na(depth_min) ~ representative_depth_min, T ~ depth_min),
         depth_max = case_when(is.na(depth_max) ~ representative_depth_max, T ~ depth_max)) %>% 
  filter(complete.cases(depth_max)) %>% 
  mutate(depth_max = as.numeric(depth_max)) %>% 
  group_by(study_id, core_id) %>% 
  summarise(max_depth = max(depth_max, na.rm = T))

cores_w_max_depth <- cores %>% 
  left_join(max_depths, by=c("study_id", "core_id"))

# write to synthesis
ccrcn_synthesis$cores <- cores_w_max_depth

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
