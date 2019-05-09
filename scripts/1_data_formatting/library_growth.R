## CCRCN Data Library
# contact: klingesd@si.edu


# This script creates and organizes progress-tracking metadata of the growth
#   of the CCRCN library


## Workspace Prep ####################

library(tidyverse)
library(taskscheduleR)
library(scales)
library(ggnewscale)
library(extrafont)
font_import()
loadfonts(device = "win")


## Compile RCN library size metrics ################

# DK initialized this script on 2019-04-01. Because DK's knowledge of R is not
#   sufficient enough at this time in order to go back in time (i.e. dive into
#   Git commit history) it is easier to just draft the template for the size
#   metrics table by hand

growth_metrics <- tibble(
  date = as.Date(c("2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", 
                 "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01",
                 "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01")),
  total_cores = c(1535, 1535, 1535, 1535, 1546, 1546, 2555, 4025, 4263, 4276, 
                  4571, 5487),
  study_IDs = c(32, 32, 32, 32, 33, 33, 34, 188, 190, 192, 194, 254),
  # Dated cores are inclusive of WG repo-only datasets, which can't be tapped into
  #   from this script
  dated_cores = c(0, 0, 0, 0, 11, 11, 22, 22, 66, 70, 133, 150)
)

# Read in first year of data 
growth_metrics <- read_csv("data/library_metrics/original/growth_metrics.csv")

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

CCRCN_depthseries <- read_csv("./data/CCRCN_synthesis/CCRCN_depthseries_data.csv")
stop_for_problems(CCRCN_depthseries)


CCRCN_depthseries_dated <- CCRCN_depthseries %>%
  filter(!is.na(total_pb210_activity) | !is.na(cs137_activity) | !is.na(ra226_activity)
         | !is.na(be7_activity)  | !is.na(age))

length(unique(CCRCN_depthseries_dated$core_id))

length(unique(CCRCN_depthseries$study_id))


## Visualize growth #####################

ggplot(growth_metrics, aes(date, total_cores)) +
  # geom_step() +
  geom_col() +
  scale_fill_gradient2(low = '#003300', high = '#990000', mid = '#eae43f',
                       midpoint = 3000, limits = c(0, 6000), oob = squish) +
  theme_classic() + 
  xlab("Date") +
  ylab("Size of Library (# Cores")




ggplot(growth_metrics, aes(x = date, y = total_cores,
             fill = total_cores)) +
  geom_col() +
  # Set range of color scale for bars
  # scale_color_gradient(low = "#56B1F7", high = "#132B43") +
  
  coord_cartesian(ylim = c(0, 6000)) + 
  
  # new_scale("fill") +
  geom_col(data = growth_metrics, aes(x = date, y = dated_cores, fill = dated_cores)) +
  # scale_fill_gradient2(low = "#1B7837", high = "#762A83") +
  theme_classic() +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "2 months") +
  theme(
    # Remove undesired elements
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",

    # NOTE: this plot will fail if the Tunga font family is not installed
    # axis.text.x = element_text(family = "Tunga", face = "bold", size = 12),
    # axis.text.y = element_text(family = "Tunga", face = "bold", size = 14),
    aspect.ratio = .35)


