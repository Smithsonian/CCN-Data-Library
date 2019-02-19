## CCRCN Data Library
# contact: klingesd@si.edu

# This script contains simple functions built to curate datasets

library(tidyverse)

## Convert mean depth to min and max depth ############
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

## Convert disintegration/min/gram to becquerel/kilogram ###########
convert_dpm_g_to_bec_kg <- function(col_input) {
  col_output <- as.numeric(col_input)/60 * 1000
  return(col_output)
}

## Convert percent to fraction ###############
convert_percent_to_fraction <- function(col_input) {
  col_output <- as.numeric(col_input)/100
  return(col_output)
}

## Convert UTM to lat/long ###############
convert_UTM_to_latlong <- function(easting, northing, zone, core_id) {
  
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
  colnames(output) <- c("core_longitude", "core_latitude")
  
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
  colnames(output_sub) <- c("core_longitude", "core_latitude") # Rename the output columns
  output <- rbind(output, output_sub) # And slap the data subset into our final output
  }
  
  output <- data.frame(output) # Turn our output into a dataframe
  
  # Now let's add those NA rows back in
  require(DataCombine)
  if(length(NA_rows) > 0){
    for (i in 1:length(NA_rows)) {
      output <- InsertRow(data = output, NewRow = rep(NA, 2), RowNum = NA_rows[[i]])
    }
  }
  # And add the core_id attribute back for joining purposes
  output <- cbind(output, core_id)
  
  # And output the output
  output
}

## Create site-level bounding box from core-level locations #############
create_multiple_geographic_coverages <- function(core_table) {
  
  subsite_bounding_box <- core_table %>%
    # group by subsite
    group_by(site_id) %>% 
    # get min and max latitudes for each subsite
    summarise(site_longitude_max = max(core_longitude),
              site_longitude_min = min(core_longitude),
              site_latitude_max = max(core_latitude),
              site_latitude_min = min(core_latitude)
    ) %>%
    # Make a 10% buffered bounding box from the site data table
    mutate(lon_buffer = (site_longitude_max - site_longitude_min) / 10,
           lat_buffer = (site_latitude_max - site_latitude_min) / 10,
           site_longitude_max = site_longitude_max + lon_buffer,
           site_longitude_min = site_longitude_min - lon_buffer,
           site_latitude_max = site_latitude_max + lat_buffer,
           site_latitude_min = site_latitude_min - lat_buffer) %>%
    dplyr::select(site_id, site_longitude_max, site_longitude_min, 
                  site_latitude_max, site_latitude_min)
}

## Create IDs from other ID ############
create_new_IDs <- function(df, old_ID, new_ID) {

  warning("I think I broke this function when modifying it for more universal use.
          The potential issues like in how the function loop sthrough the
          # old_ID_list and the gsub at the end (easier to put the gsub inside
          # the loop but faster to have gsub outside the loop)")
  
  # create list of unique study IDs
  old_ID_list <- unique(df[, old_ID])

  # Iterate through study ID list
  for (i in 1:nrow(old_ID_list)) {
    
    # subset to a particular study
    study <- subset(df, df[, old_ID] == old_ID_list[[i]])
    
      # Create a unique ID from the row number
      study <- study %>%
        rowid_to_column("ID")
      
      # Combine the old ID with the row number separated by underscore
      study$new <- paste0(study[, old_ID], "_", study[, "ID"])
      
      # Rename the 'new' column with the new_ID string given by the user
      colName <- new_ID
      study <- study %>%
        mutate(!!quo_name(colName) := new)

      # Now removed temporary columns
      study <- study %>%
        select(-ID, -new)
    
    # Combine back together
    if (i == 1) {
      df_out <- study
    } else {
      df_out <- bind_rows(df_out, study)
      }
  }
  
  # Replace spaces with underscores in ID
  df_out <- !!quo_name(colName) := gsub(" ", "_", df_out)
  
  # Return final data frame
  df_out
}

