## CCCRN Data Library
# Jaxine Wolfe, wolfejax@si.edu

# Update database guidance

library(tidyverse)

# load guidance and controlled variables
guidance <- read_csv("docs/ccrcn_database_structure.csv")
variables <- read_csv("docs/controlled_variables.csv")
# attributes <- read_csv("docs/controlled_attributes.csv")

# Current Workflow:
# all controlled variables are defined in the controlled variables table
# however the corresponding attribute needs to exist in the database guidance first
# All attribute definitions will be defined in the database guidance
# The controlled attributes table will be created from the database guidance

## Update variable codes in the db guidance from controlled var table

## Collapse controlled vars and populate database 
collapsed_vars <- variables %>% 
  mutate(variable_codes = str_c(variable_name, variable_definition, sep = " = ")) %>% 
  group_by(attribute_name) %>% 
  summarise(variable_codes = paste(variable_codes, collapse="; "))

# attribute_count <- guidance %>% 
#   distinct(attribute_name, definition) %>% 
#   count(attribute_name)
# year and month have two different definitions...
# core year/month vs publication year/month

updated_guidance <- guidance %>% 
  # distinct(attribute_name, definition, data_type, format_unit_codes) %>%
  left_join(collapsed_vars) %>% # attributes must be defined in database
  mutate(format_unit_codes = ifelse(!is.na(variable_codes), variable_codes, format_unit_codes)) %>% 
  select(-variable_codes)

# write updated guidance to docs folder
write_csv(updated_guidance, "docs/ccrcn_database_structure.csv")

# Update controlled attributes table

# update 
updated_attributes <- updated_guidance %>% 
  distinct(attribute_name, attribute_definition, data_type, format_unit_codes, number_type) %>%
  # create the columns: unit, format_string, values
  mutate(unit = ifelse(data_type == "numeric", format_unit_codes, NA),
         format_string = case_when(data_type == "Date" ~ format_unit_codes,
                                   format_unit_codes == "nominalYear" ~ "YYYY",
                                   format_unit_codes == "nominalMonth" ~ "MM",
                                   format_unit_codes == "nominalDay" ~ "DD",
                                   TRUE ~ NA_character_),
         values = ifelse(data_type == "factor", format_unit_codes, NA))
write_csv(updated_attributes, "docs/controlled_attributes.csv")

