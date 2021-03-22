## CCRCN Database Paper ####

## Modeling the CCN Relational Database
## contact: Jaxine Wolfe, wolfejax@si.edu

library(dm)
library(DiagrammeR)
library(tidyverse)

new_guidance <- read_csv("docs/ccrcn_database_structure_V2.csv")

methods <- read_csv("data/CCRCN_V2/methods.csv")
sites <- read_csv("data/CCRCN_V2/sites.csv")
cores <- read_csv("data/CCRCN_V2/core_geography.csv", guess_max = 7000) # will be cores.csv in the future
depthseries <- read_csv("data/CCRCN_V2/depthseries.csv", guess_max = 50000)
species <- read_csv("data/CCRCN_V2/species.csv")
impacts <- read_csv("data/CCRCN_V2/impacts.csv")


# set up tables as dm objects
dm_tables <- dm(methods, sites, cores, depthseries, species, impacts)
# I think these do the same thing
# dm_list <- new_dm(list(sites = sites, cores = cores))
# dm_setas <- as_dm(list(sites = sites, cores = cores))

# explore the dms
validate_dm(dm_tables)
names(dm_tables)
dm_tables$sites

# Link tables by adding keys ####

# primary keys = attributes that uniquely identifies a row in a table
# foreign keys = counterpart of a primary key in one table is the foreign key in another table

# IDs in the database: 
# - study (shared by all) 
# - site (shared by all except methods) 
# - core (shared by core, depthseries, species, impacts)

dm_keys <- dm_tables %>%
  # define primary keys
  # all tables need a primary key first
  dm_add_pk(methods, study_id) %>%
  dm_add_pk(sites, site_id) %>%
  dm_add_pk(cores, core_id) %>%
  dm_add_pk(depthseries, core_id) %>%
  dm_add_pk(impacts, core_id) %>%
  dm_add_pk(species, core_id) %>%
  # establish relationships (this generates the arrows)
  # relate all study ids
  dm_add_fk(sites, study_id, methods) %>%
  dm_add_fk(cores, study_id, sites) %>%
  dm_add_fk(depthseries, study_id, cores) %>%
  dm_add_fk(species, study_id, cores) %>%
  dm_add_fk(impacts, study_id, cores) %>%
  # relate all site ids
  dm_add_fk(cores, site_id, sites) %>%
  dm_add_fk(depthseries, site_id, cores) %>%
  dm_add_fk(species, site_id, cores) %>%
  dm_add_fk(impacts, site_id, cores) %>%
  # relate core ids
  dm_add_fk(depthseries, core_id, cores) %>%
  dm_add_fk(species, core_id, cores) %>%
  dm_add_fk(impacts, core_id, cores)

# visualize the data model
dm_keys %>% 
  dm_set_colors(green = sites, blue = cores, red = c(depthseries, impacts, species), yellow = methods) %>%
  dm_draw()

# examine constraints
dm_tables %>% dm_examine_constraints() # no constraints defined currently
