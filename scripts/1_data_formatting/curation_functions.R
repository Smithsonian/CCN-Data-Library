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

# Convert UTM to lat/long
convert_UTM_to_latlong <- function(easting, northing, zone) {
  
  # Remove non-numeric characters from zone attribute
  zone <- gsub("[^0-9]", "", zone)
  
  # Change value of digits to ensure as.numeric does not round
  options(digits=22)
  
  # Ensure that easting, northing, and zone are numeric
  easting <- as.numeric(easting)
  northing <- as.numeric(northing)
  zone <- as.numeric(zone)
  
  # Combine the three attributes
  spatialData <- as.matrix(cbind(easting, northing, zone))
  
  # Some rows may have NA values. We'll want to flag those rows, notify the user,
  #   remove them before transforming, and then add them back at the end.
  NA_rows <- c()
  
  # For each of the columns in spatialData (easting, northing, zone)...
  for (i in 1:3) {
    col <- spatialData[,i]
    
    # ...and for each cell in those columns...
    for (j in 1:length(col)) {
      
      # ...if the cell's value is NA...
      if (is.na(col[[j]])) {
        
        # ...tell the user so...
        warning(paste0("Row ", j, " in column ", colnames(spatialData)[i], " is 'NA.' 
            Removing row from data frame for now but will add back with NA at end
            of function. NOTE: CHECK TO MAKE SURE THE SAME ROWS HAVE 'NA' AND 
            'NA' HAS NOT BEEN ADDED TO OTHER ROWS"))
        
        # And add the row # of that cell to a vector that we initialized before
        NA_rows <- c(NA_rows, j)
        # In case some rows have more than one NA. We only need that row once
        NA_rows <- unique(NA_rows)
      }
    }
    NA_rows <- unique(NA_rows)
  }
  
  # We'll need spatialData as a tibble or data frame going forward
  require(tidyverse)
  spatialData <- as_tibble(cbind(easting, northing, zone))

  # Now that we flagged the NA rows, we can get rid of them
  spatialData <- na.omit(spatialData)
  
  # And get rid of any zone values that were NA
  zone_list <- as.list(unique(na.omit(zone)))
  
  # Establish the projection we'll convert to. NOTE: this could be a user input
  wgs84 = "+init=epsg:4326"
  
  # Initialize our output dataset
  output <- matrix(nrow = 0, ncol = 2)
  colnames(output) <- c("X", "Y")
  
  # We'll need to transform the projection separately for data that are in
  #   different zones. So we'll need to subset by the zone values, which we
  #   stored in zone_list
  for (i in 1:length(zone_list)) {
    
    # Filter to just one zone
    spatialData_sub <- spatialData %>%
      filter(zone == zone_list[[i]]) %>%
      select(easting, northing) %>% # We don't need the zone attribute anymore
      na.omit() # Just in case
    
    # Create a dataframe for out subsetted data
    output_sub <- spatialData_sub
    
    # Define the proj4string, using the zone that the subsetted data are in
    proj4string <- CRS(as.character(paste0("+proj=utm +zone=", zone_list[[i]],
                                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
    
    # Finally, perform the transformation
    require(sp)
    sp <-  sp::spTransform(sp::SpatialPoints(list(spatialData_sub$easting, 
              spatialData_sub$northing), proj4string=proj4string),sp::CRS(wgs84))
  

  output_sub <- na.omit(sp@coords) # Get rid of NAs again
  colnames(output_sub) <- c("X", "Y") # Rename the output columns
  output <- rbind(output, output_sub) # And slap the data subset into our final output
  }
  
  output <- data.frame(output)# Turn our output into a dataframe
  
  # Now let's add those NA rows back in
  require(DataCombine)
  for (i in 1:length(NA_rows)) {
    output <- InsertRow(data = output, NewRow = rep(NA, 2), RowNum = NA_rows[[i]])
  }
  output
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


