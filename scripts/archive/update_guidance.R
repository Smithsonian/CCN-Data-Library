# Coastal Carbon Research Coordination Network
# Database Guidance V2

# Create updated guidance table
# This script will be archived eventually and the V2 guidance will be treated as static
# Contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)

# read in guidance and conversion template
old_guidance <- read_csv("docs/ccrcn_database_structure.csv")
guidance_conversion <- read_csv("docs/post_processing/tables/input_files/v1p2_to_v2/converting_v1p2_to_v2.csv")

new_guidance <- guidance_conversion %>%
  filter(!grepl("deletion", action)) %>%
  select(-c(attribute_name_v1p2, reason)) %>%
  distinct() %>%
  filter(!grepl(" deviation", definition)) %>%
  add_count(attribute_name, heirarchy) %>%
  mutate(to_remove = case_when(attribute_name == "year" & data_type == "Date" ~ T,
                               heirarchy == "depthseries" & action == "rename" & n == 2 ~ T,
                               TRUE ~ F)) %>%
  filter(to_remove == F) %>%
  mutate(attribute_name = recode(attribute_name, "impacts" = "impact_class")) %>%
  select(-c(to_remove, n, action, manager_notes)) %>%
  rename(table = heirarchy) %>%
  select(table, attribute_name, definition, required, data_type, format_unit_codes, data_category, 
         parent_data_category, quality_class, uncontrolled, everything())

# check against old guidance
# isolate the table-attribute name combos that are present in the new guidance but not the old
not_in_old <- new_guidance %>% select(table, attribute_name) %>%
  anti_join(old_guidance %>% select(table, attribute_name))

# isolate the table-attribute name combos that are present in the old guidance but not the new
not_in_new <- old_guidance %>% select(table, attribute_name) %>%
  anti_join(new_guidance %>% select(table, attribute_name))

# write_csv(new_guidance, "docs/ccrcn_database_structure_V2.csv")

# Attributes added manually:
# post-processing attributes (i.e. quality codes)
# study citation attributes 
