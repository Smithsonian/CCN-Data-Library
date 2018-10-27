## CCRCN Data Library
# contact: klingesd@si.edu

# This script contains simple functions built to curate datasets

library(tidyverse)

# For conversion of depth interval values: if a dataframe
#   has a mean depth attribute, this will create a min depth
#   and max depth attributes
convert_mean_depth_to_min_max <- function(dataframe, mean.depth) {
  # Set l as th length of the dataframe
  l <- length(mean.depth)
  mean.depth <- as.numeric(mean.depth)
  # For each row....
  for (i in 1:l) {
    min <- (mean.depth[i - 1] + mean.depth[i])/2 # Take mean of previous cell and current cell
    max <- (mean.depth[i + 1] + mean.depth[i])/2 # Take mean of current cell and next cell
      # If initial iteration...
      if (i == 1) {
        depth_min = as.vector(min) # create depth_min vector
        depth_max = as.vector(max) # create depth_max vector
      } else { # If not initial iteration, add min/max
               #  value to depth_min/max vector
        
          depth_min[i] <- min
          depth_max[i] <- max
        }
  }

  if (mean.depth[1] == 0) {
    depth_min[1] <- 0
    depth_max[1] <- 0
    depth_min[2] <- 0
    depth_max[l] <- depth_min[l] + 2 * (mean.depth[l] - depth_min[l])
  } else {
    depth_min[1] <- 0
    depth_max[l] <- depth_min[l] + 2 * (mean.depth[l] - depth_min[l])
    }

  dataframe <- dataframe %>%
    mutate(depth_min = depth_min) %>%
    mutate(depth_max = depth_max)

  return(dataframe)
}       


# ours: Pb: 1 becquerel  = 1 disintegration/second
#   her data is in 1 

# Convert disintegration/g/gram to becquerel/kilogram
d_min_g_to_bec_kg <- function(dataframe, activity_input, output_name) {
  dataframe <-dataframe %>%
    as.numeric(activity_input)
  dataframe <- dataframe %>%
    mutate(output_name = activity_input/60/1000)
}

