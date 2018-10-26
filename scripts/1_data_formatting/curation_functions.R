
library(tidyverse)


gonneea_test <- Gonneea_2018


convert_mean_depth_to_min_max <- function(dataframe, mean.depth) {
  
  l <- length(mean.depth)
  mean.depth <- as.numeric(mean.depth)
  for (i in 1:l) {
    min <- (mean.depth[i - 1] + mean.depth[i])/2 # Take mean of previous cell and current cell
    max <- (mean.depth[i + 1] + mean.depth[i])/2 # Take mean of current cell and next cell
      if (i == 1) {
        depth_min = as.vector(min)
        depth_max = as.vector(max)
      }
    depth_min[i] <- min
    depth_max[i] <- max
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



