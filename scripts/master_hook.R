# Coastal Carbon Research Coordination Network
# This script presses the big red button and re-hooks all data sources
# Contact: klingesd@si.edu

files <- list.files("./scripts/0_data_hooks", pattern = ".R", full.names = TRUE) 

files %>% walk(source)
