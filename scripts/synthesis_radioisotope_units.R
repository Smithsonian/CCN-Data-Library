## CCRCN Data Library
## Date: 2022-08-01
## Jaxine Wolfe <wolfejax@si.edu>

# QA maintenance check 
# investigate the units specified for radioisotopes throughout the synthesis

ds <- read_csv("data/CCRCN_synthesis/CCRCN_depthseries.csv", guess_max = 50000)

ds_units <- ds %>% select(contains("_unit")) %>% distinct() %>% 
  pivot_longer(everything(), names_to = "attribute_name", values_to = "unit") %>% 
  # mutate(attribute_name = gsub("_unit", "", attribute_name)) %>% 
  drop_na(unit) %>% arrange(attribute_name) %>% distinct()
# in need of some standardization

write_csv(ds_units, "data/QA/synthesis_dating_units.csv")