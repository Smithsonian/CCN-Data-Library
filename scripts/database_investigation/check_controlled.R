## CCRCN Data Library ####

# Script to check that all vars in the controlled vars table are defined in the guidance and vice versa
# Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)

# load guidance and controlled variables
new_guidance <- read_csv("docs/ccrcn_database_structure.csv")
controlled_vars <- read_csv("docs/controlled_variables.csv")
controlled_attributes <- read_csv("docs/controlled_attributes.csv")

## Inspect Variables ####
# extract variables
v2_vars <- new_guidance %>% filter(data_type == "factor") %>% select(attribute_name) %>% distinct() %>% pull(attribute_name)
v1_vars <- controlled_vars %>% select(attribute_name) %>% distinct() %>% pull(attribute_name)

# v1 vars not in v2 vars
v1_vars[which(!(v1_vars %in% v2_vars))]

# v1 vars not in v2 vars
v2_vars[which(!(v2_vars %in% v1_vars))]

# Actions taken:
# Vars added for these new attributes: habitat, code_type, stocks_qual_code, dates_qual_code, elevation_qual_code
# Updated the vars for this existant attribute: publication_type

## Inspect Column Names ####
# extract attributes
v2_attr <- new_guidance %>% select(attribute_name) %>% distinct() %>% pull(attribute_name)
v1_attr <- controlled_attributes %>% select(attribute_name) %>% distinct() %>% pull(attribute_name)

# v1 vars not in v2 vars
v1_attr[which(!(v1_attr %in% v2_attr))]

# v1 vars not in v2 vars
v2_attr[which(!(v2_attr %in% v1_attr))]

# Should ideally use the database guidance to generate 
# both the controlled variables and attributes tables

