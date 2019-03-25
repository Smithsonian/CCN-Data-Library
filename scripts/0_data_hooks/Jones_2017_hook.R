# Coastal Carbon Research Coordination Network
# This script prepares Jones et al 2017 for use by
#   the Soil Carbon working group.
# Contact: klingesd@si.edu

## Workspace prep ########################

library(tidyverse)

depthseries_raw <- read_csv("./data/Jones_2017/original/Jones_2017_depthseries.csv",
                        col_names = FALSE)


## Curate depthseries data #########

depthseries <- depthseries_raw %>%
  rename(depth = X1, age = X2)



## Write data ###########

write_csv(depthseries, "./data/Jones_2017/derivative/Jones_2017_depthseries.csv")
