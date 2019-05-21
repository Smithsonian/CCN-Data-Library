## CCRCN Data Library
# contact: klingesd@si.edu


# This script creates and organizes progress-tracking metadata of the growth
#   of the CCRCN library


## Workspace Prep ####################

library(tidyverse)
library(taskscheduleR)
library(scales)
library(ggnewscale)
library(magick)

marsh <- image_read("docs/images/sercwetland_grace_schwartz.jpg") %>%
  image_colorize(opacity = 40, color = 'white')

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
growth_metrics <- read_csv("data/library_metrics/growth_metrics.csv",
                           col_types = cols(date = col_date()))

CCRCN_cores <- read_csv("data/CCRCN_synthesis/CCRCN_core_data.csv")
CCRCN_depthseries <- read_csv("./data/CCRCN_synthesis/CCRCN_depthseries_data.csv",
                              col_types = cols(.default = col_number(),
                                               study_id = col_character(),
                                               site_id = col_character(),
                                               core_id = col_character()
                                               ))

CCRCN_depthseries_dated <- CCRCN_depthseries %>%
  filter(!is.na(total_pb210_activity) | !is.na(cs137_activity) | !is.na(ra226_activity)
         | !is.na(be7_activity)  | !is.na(age))

# Create tibble for this month's date
growth_metrics_newMonth <- tibble(
  date = as.Date(Sys.Date()),
  total_cores = length(unique(CCRCN_cores$core_id)),
  study_ids = length(unique(CCRCN_cores$study_id)),
  dated_cores = length(unique(CCRCN_depthseries_dated$core_id))
)


# Add this month's data to past growth metrics
growth_metrics <- growth_metrics %>%
  bind_rows(growth_metrics_newMonth)


## Save updated growth metrics ###########

write_csv(growth_metrics, "data/library_metrics/growth_metrics.csv")

## Visualize growth #####################

growth_plot <- ggplot() +
  annotation_raster(marsh, ymin = -Inf, ymax= Inf, xmin = -Inf, xmax = Inf) +
  geom_col(data = growth_metrics, aes(x = date, y = total_cores,
                                      fill = total_cores), width = 15) +

  # Set range of color scale for bars
  # scale_color_gradient(low = "#56B1F7", high = "#132B43") +
  
  coord_cartesian(ylim = c(0, 6000)) + 
  
  # new_scale("fill") +
  # geom_col(data = growth_metrics, aes(x = date, y = dated_cores, fill = dated_cores),
  #          width = 15) +
  # scale_fill_gradient2(low = "#1B7837", high = "#762A83") +
  
  theme_classic() +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "2 months") +
  xlab("Date") +
  ylab("# Cores in CCRCN Library") +
  theme(
    # Format axis titles
    axis.title.x = element_text(size = 16, face = "bold", vjust = .6),
    axis.title.y = element_text(size = 12, face = "bold", angle = 60, vjust = .5,
                                margin = margin(t = 0, r = 30, b = 0, l = 0)),
    
    # Remove undesired elements
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    legend.position = "none",

    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 14),
    aspect.ratio = .35) 


# Write plot
ggsave("docs/images/CCRCN_library_growth.png", plot = growth_plot, device = "png", 
       scale = 1.2, width = 160, height = 50,
       units = "mm", limitsize = FALSE)
