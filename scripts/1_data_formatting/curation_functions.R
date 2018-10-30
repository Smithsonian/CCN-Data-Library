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

# Convert disintegration/min/gram to becquerel/kilogram
convert_dpm_g_to_bec_kg <- function(col_input) {
  col_output <- as.numeric(col_input)/60 * 1000
  return(col_output)
}

# Convert percent to fraction

convert_percent_to_fraction <- function(col_input) {
  col_output <- as.numeric(col_input)/100
  return(col_output)
}




# dataframe[output_name] <- vector(mode = "numeric", length = length(dataframe[col_name]))
# dataframe[output_name] <- as.numeric(dataframe[col_name])/60 * 1000
# dataframe <-dataframe %>%
#   as.numeric(col_name)
# col_output <- interp(x, x = output_name)
#
# col_output <- vector(mode = "numeric", length = length(col_input))
#
# col_input <- enquo(col_name)
# col_input <- as.numeric(col_input)/60 * 1000
# dataframe[col_output] <- col_input
# becquerel is 1 disintegration per second
# divide by 60 seconds, then multiply by 1000 grams
# col_input <- interp(lazy(as.numeric(x)/60 * 1000), x = as.name(dataframe[col_name]))
# dataframe <- dataframe %>%
#   mutate(new = col_input)


