## CCRCN Data Library QA/QC scripts
# contact: Michael Lonneman, lonnemanM@si.edu 
#          David Klinges, klingesD@si.edu

## Test column names function ###########
# Make sure column names match CCRCN guidelines
test_colnames <- function(category, dataset) {
  
  database_structure <- read_csv("./docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  if(category %in% database_structure$table == FALSE) {
    # Warn user they have not supplied a valid table category and provide the list 
    print(paste(category,"is not a valid category. Please use one of these options:"))
    print(tables)
    return()
  }
  
  # Gather column names from dataset and pull out columns with variables that are defined in our database structure
  column_names <- colnames(dataset)
  valid_columns <- filter(database_structure, table == category)$attribute
  non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
  
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

reorder_columns <- function(data, datalevel_table) {
  
# Read in database structure
col_types <- cols(
  attribute = col_character(),
  table = col_character(),
  definition = col_character(),
  data_type = col_character(),
  format_unit_codes = col_character(),
  data_category = col_character(),
  dependency_class = col_character(),
  parent_data_category = col_character()
)
database_structure <- read_csv("./docs/ccrcn_database_structure.csv",
                               col_types = col_types)

# Subset database structure according to designated table
# ML COMMENT: I'm not sure the filter call is working properly. I think the value (the particular table type) label cannot match the variable label. 
database_structure <- database_structure %>%
  filter(table == datalevel_table)

# Create list of database attributes
# ML COMMENT: making it a vector rather than a list might be a better idea 
# That way select() will work effectively 
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

