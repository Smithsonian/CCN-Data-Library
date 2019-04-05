## CCRCN Data Library
# contact: Michael Lonneman, lonnemanM@si.edu 

# This script contains QA/QC scripts to test validity of curated data

## Test header names to ensure they meet CCRCN specifications ##########
# Possible variable names according to the CCRCN: 

site_var <- c(
              "study_id", "site_id", 
              "site_description", "site_latitude_max", "site_latitude_min", "site_longitude_max", "site_longitude_min", "site_description",
              "salinity_class", "salinity_method", "salinity_notes", 
              "vegetation_class", "vegetation_method", "vegetation_notes", 
              "inundation_class", "inundation_method", "inundation_notes"
              )

core_var <- c(
              "study_id", "site_id", "core_id", 
              'core_date', "core_notes",
              "core_latitude", "core_longitude", "core_position_accuracy", "core_position_method", "core_position_notes", 
              "core_elevation", "core_elevation_datum", "core_elevation_accuracy", "core_elevation_method", "core_elevation_notes",
              "salinity_class", "salinity_method", "salinity_notes", 
              "vegetation_class", "vegetation_method", "vegetation_notes", 
              "inundation_class", "inundation_method", "inundation_notes", 
              "core_length_flag"
              )

soil_depth_var <- c(
              "study_id", "site_id", "core_id", "sample_id",
              "depth_min", "depth_max", 
              "dry_bulk_density", "fraction_organic_matter", "fraction_carbon", "compaction_fraction", "compaction_notes", 
              "cs137_activity", "cs137_activity_sd", 
              "total_pb210_activity", "total_pb210_activity_sd", 
              "ra226_activity", "ra226_activity_sd", 
              "excess_pb210_activity", "excess_pb210_activity_sd",
              "c14_age", "c14_age_sd", "c14_material", "c14_notes", 
              "delta_c13", 
              "be7_activity", "be7_activity_sd",
              "am241_activity", "am241_activity_sd", 
              "marker_date", "marker_type", "marker_notes", 
              "age", "age_min", "age_max", "age_sd", 
              "depth_interval_notes"
)

species_var <- c(
              "study_id", "site_id", "core_id", 
              "species_code"
              )

impact_var <- c(
              "study_id", "site_id", "core_id", 
              "impact_class"
              )

## Test column names function ###########
# Make sure column names match CCRCN guidelines
test_colnames <- function(category, dataset) {
  categories <- c("sites", "cores", "depthseries", "species", "impacts")
  if(category %in% categories == FALSE) {
    # Warn user they have not supplied a valid table category and provide the list 
    print(paste(category,paste(c("is not a valid category. Please use one of these options:", categories), collapse=" ")))
    return()
  }
  
  column_names <- colnames(dataset)
  
  if(category == "sites"){
    non_matching_columns <- subset(column_names, !(column_names %in% site_var))
    
  } else if (category == "cores") {
    non_matching_columns <- subset(column_names, !(column_names %in% core_var))
    
  } else if (category == "depthseries") {
    non_matching_columns <- subset(column_names, !(column_names %in% soil_depth_var))
    
  } else if (category == "species") {
    non_matching_columns <- subset(column_names, !(column_names %in% species_var))
    
  } else { # category must be impacts
    non_matching_columns <- subset(column_names, !(column_names %in% impact_var))
    
  }
  
  if(length(non_matching_columns)==0) {
    print("Looks good! All column names match CCRCN standards")
  } else {
    print(paste(c("Non-matching variable names/column headers:", non_matching_columns), collapse=" "))
  }
}

## Check core_id uniqueness ########
# All cores in synthesis should have unique id

test_unique_cores <- function(data) {
  # First, get a list of all unique core_ids
  core_list <- data %>%
    group_by(core_id) %>%
    summarize(n = n()) %>%
    filter(n > 1)
  
  if(length(core_list$core_id)>0){
    print("WARNING: check the following core_ids in the core-level data:")
    print(core_list$core_id)
  } else {
    print("All core IDs are unique.")
  }
  
  return(core_list)
}

## Check lat/long uniqueness ########
# Cores in synthesis should likely have unique lat/long values

test_unique_coords <- function(data) {
  # First, get a list of all unique core_ids
  core_list <- data %>%
    mutate(lat_long = paste(core_latitude,core_longitude, sep=",")) %>%
    group_by(lat_long) %>%
    summarize(n = n(), 
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1)

  if(length(core_list$core_ids)>0){
    print("WARNING: some cores in the core-level data have duplicate coordinates. Check 'data/QA/duplicate_cores.csv' for the list.")
  } else {
    print("All core coordinates are unique.")
  }
  
  return(core_list)
}

## Check for core_id relationships ########
# All entries in core_data files should have relationships to relevant depth series and biomass datasets

test_core_relationships <- function(core_data, depth_data) {
  # this will return any core_ids that do not have a match in the core-level and depthseries data
  results <- (anti_join(core_data, depth_data, by="core_id"))$core_id
  results2 <- (anti_join(depth_data, core_data, by="core_id"))$core_id
  
  if(length(results)>0 & length(results2)>0){
    print("WARNING: check the following core_ids in the core-level data:")
    print(results)
    
    print("WARNING: check the following core_ids in the depthseries data:")
    print(results2)
  } else {
    print("Core IDs match.")
  }
  append(results,results2)
  return(results)
}


## Ensure fractions not percetanges #################

# This function reviews all attributes that ought to be a fraction and determines
#   whether they are in their proper format (e.g. less than 1)

fraction_not_percent <- function(dataset) {
  
  relevant_cols <- dataset %>%
    select(contains("fraction")) %>%
    filter_all(any_vars(. > 1))
  
  
  if(nrow(relevant_cols) > 1) {
    
    stop(paste0("At least one of the attributes intended to be expressed as a
                fraction is expressed as a percent (values > 1). Please review all
                fraction columns: ", paste(colnames(relevant_cols), collapse = ', ')))
  } else {
    print("All fractions are expressed as fractions, safe to continue.")
  }
}

## Re-order data according to database structure #####################

reorder_columns <- function(data, table) {
# Read in database structure
database_structure <- read_csv("./docs/ccrcn_database_structure.csv")

# Subset database structure according to designated table
database_structure <- database_structure %>%
  filter(table == "depthseries")

# Create list of database attributes
db_attributes <- as.list(database_structure$attribute)

# Create list of chosen dataset attributes
data_attributes <- colnames(data)

# Subset the database attributes to just those in the dataset...the output
#   will be in the order of the database attributes
data_attributes_reorder <- subset(db_attributes, db_attributes %in% data_attributes)

# Now, use select_ to reorder the dataset to the right order
# I tried a lot of methods to do this with various ways to parse the list of 
#   character strings from data_attributes_reorder, and this is what worked.
# Not completely certain why cycling through the list worked and parsing didn't...
#   but not a concern for now
extract_columns <- function(data) {
  extracted_data <- data %>%
    select_(.dots = data_attributes_reorder)
  return(extracted_data)
}

# Create an output dataset with the correct order
data_out <- extract_columns(data)

# ...if there are more attributes in your dataset than in the subsetted
#   database attributes, and therefore there are attributes in your dataset
#   for which there is no guidance in the database...
if (length(data_attributes_reorder) < length(data_attributes)) {
  # Notify the user
  print("Some columns in your data are not present in the current database guidelines. Appending these to the end of the dataset.")
}

# Then, bind the attributes without guidance to the end of the dataset
# Figure out what attributes are missing guidance by removing all of the common
#   attributes between 'data' and 'data_out', leaving just the ones missing from
#   data_out
missing_guidance <- data %>%
  select(-one_of(colnames(data_out)))

# Now bind those, which will put them at the end
data_out <- data_out %>%
  bind_cols(missing_guidance)

return(data_out)
}

