# Coastal Carbon Research Coordination Network
# Database Guidance V2

# Create updated guidance table
# Contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)

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
  select(-c(to_remove, n, action, manager_notes, contains("conditional"))) %>%
  rename(table = heirarchy) %>%
  select(attribute_name, table, definition, required, data_type, format_unit_codes, data_category, 
         parent_data_category, quality_class, uncontrolled)

write_csv(new_guidance, "docs/ccrcn_database_structure_V2.csv")  

