## CCRCN Data Library
# contact: klingesd@si.edu


# This script creates and organizes progress-tracking metadata of the growth
#   of the CCRCN library


## Workspace Prep ####################

library(tidyverse)
library(taskscheduleR)


## Compile RCN library size metrics ################

# DK initialized this script on 2019-04-01. Because DK's knowledge of R is not
#   sufficient enough at this time in order to go back in time (i.e. dive into
#   Git commit history) it is easier to just draft the template for the size
#   metrics table by hand

growth_metrics <- tibble(
  date = as.Date(c("2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", 
                 "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01",
                 "2019-02-01", "2019-03-01", "2019-04-01")),
  total_cores = c(1535, 1535, 1535, 1535, 1546, 1546, 2555, 4025, 4263, 4276, 4571),
  study_IDs = c(32, 32, 32, 32, 33, 33, 34, 188, 190, 192, 194),
  # Dated cores are inclusive of WG repo-only datasets, which can't be tapped into
  #   from this script
  dated_cores = c(0, 0, 0, 0, 11, 11, 22, 22, 66, 70, 133)
)

# NOTE: 
CCRCN_depthseries <- read_csv("./data/CCRCN_synthesis/CCRCN_depthseries_data.csv",
                              col_types = cols(.default = col_number(),
                                               study_id = col_character(),
                                               site_id = col_character(),
                                               core_id = col_character(),
                                               fraction_carbon_type = col_factor(),
                                               DBD_measured_or_modeled = col_factor(),
                                               OC_measured_or_modeled = col_factor(),
                                               CD_measured_or_modeled = col_factor(),
                                               CD_reported = col_logical()
                                               ))

stop_for_problems(CCRCN_depthseries)


CCRCN_depthseries_dated <- CCRCN_depthseries %>%
  filter(!is.na(total_pb210_activity) | !is.na(cs137_activity) | !is.na(ra226_activity)
         | !is.na(be7_activity)  | !is.na(age))

length(unique(CCRCN_depthseries_dated$core_id))


## Visualize growth #####################

ggplot(growth_metrics, aes(date, total_cores)) +
  geom_step()
