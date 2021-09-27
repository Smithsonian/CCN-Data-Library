## CCRCN Database Paper ####

## Create clean tabular versions of the hierarchical database

library(tidyverse)

# columns to include
# field name, description, class, units, required?
guidance <- read_csv("docs/ccrcn_database_structure.csv") %>%
  mutate(unit = ifelse(data_type != "numeric", NA, format_unit_codes)) %>% 
  select(-c(data_category, parent_data_category, quality_class, uncontrolled,
            format_unit_codes, number_type, contains("conditional")))

# subset guidance into individual tables
methods <- guidance %>% filter(table == "methods") %>% select(-table)
sites <- guidance %>% filter(table == "sites") %>% select(-table)
cores <- guidance %>% filter(table == "cores") %>% select(-table)
depthseries <- guidance %>% filter(table == "depthseries") %>% select(-table)
species <- guidance %>% filter(table == "species") %>% select(-table, -unit)
impacts <- guidance %>% filter(table == "impacts") %>% select(-table, -unit)

# write tables
write_csv(methods, "database_paper/tables/methods_attributes.csv")
write_csv(sites, "database_paper/tables/site_attributes.csv")
write_csv(cores, "database_paper/tables/core_attributes.csv")
write_csv(depthseries, "database_paper/tables/depthseries_attributes.csv")
write_csv(species, "database_paper/tables/species_attributes.csv")
write_csv(impacts, "database_paper/tables/impact_attributes.csv")
# write_csv(citations, "database_paper/tables/citation_attributes.csv")