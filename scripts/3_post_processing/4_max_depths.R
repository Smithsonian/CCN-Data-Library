# Max depths

cores <- ccrcn_synthesis$cores
depthseries <- ccrcn_synthesis$depthseries

max_depths <- depthseries %>% 
  filter(complete.cases(depth_max)) %>% 
  group_by(study_id, core_id) %>% 
  summarise(max_depth = max(depth_max, na.rm = T))

cores_w_max_depth <- cores %>% 
  left_join(max_depths, by=c("study_id", "core_id"))

# write to synthesis
ccrcn_synthesis$cores <- cores_w_max_depth

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results"))])
