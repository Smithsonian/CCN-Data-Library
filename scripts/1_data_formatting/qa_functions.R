## CCRCN Data Library QA/QC scripts
# contact: Michael Lonneman, lonnemanM@si.edu 
#          David Klinges, klingesD@si.edu

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
  
  if(length(results) > 0){
    print("WARNING: check the following core_ids in the core-level data:")
    print(results)
  } 
  
  if(length(results2) > 0) {
    print("WARNING: check the following core_ids in the depthseries data:")
    print(results2)
  }
  
  if(length(results)== 0 & length(results2) == 0) {
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

## Test variable names #############

test_varnames <- function(input_data) {
  
  controlled_variables_list <- read_csv("./docs/controlled_variables.csv",
                                        col_types = cols())
  
  # Subset controlled variables by the attributes that are in the tested data frame
  var_names <- unique(controlled_variables_list$attribute_name)
  to_check <- subset(colnames(input_data), colnames(input_data) %in% var_names)
  core_subset <- select(input_data, to_check)
  
  # Create an empty data frame 
  # Invalid variables and their attribute will be stored
  df <- data.frame(matrix(nrow=0, ncol=2))
  colnames(df) <- c("attribute_name", "variable_name")
  
  # Check each column at a time
  # Append any invalid variables to the empty data frame
  if(is_empty(to_check)==FALSE){
    for(i in 1:length(to_check)){
      attribute <- to_check[i]
      variable_list <- filter(controlled_variables_list, attribute_name == attribute)
      
      x <- core_subset %>%
        filter(!(get(attribute) %in% variable_list$variable_name))
      
      invalid_variables <- na.omit(unique(get(attribute, x)))
      
      if(is_empty(invalid_variables) == FALSE) {
        df <- bind_rows(df, data.frame("attribute_name" = rep(attribute, length(invalid_variables)), "variable_name" = invalid_variables))
      }
    }
  }
  
  # If there are no invalid variables don't pass along the df 
  # Otherwise indicate to the user there are problems and to check the table 
  if(nrow(df)==0) {
    print("Looks good! All variable names match CCRCN standards")
    
  } else {
    print("View resulting table for invalid variable names")
    return(df)  
  }
}

## Select only controlled attributes, re-order according to database structure #####################

select_and_reorder_columns <- function(datalevel_table, # A string corresponding
                                        # to the name of the data level table
                                        data, # The dataset you seek to re-order 
                                       uncontrolled_file_path # File path to where
                                       # the uncontrolled attributes should be saved
) {
  
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
  database_structure <- database_structure %>%
    filter(table == datalevel_table)
  
  # Create list of database attributes
  # ML COMMENT: making it a vector rather than a list might be a better idea 
  # That way select() will work effectively 
  db_attributes <- c(database_structure$attribute)
  
  # Create list of chosen dataset attributes
  data_attributes <- colnames(data)
  
  # Subset the database attributes to just those in the dataset...the output
  #   will be in the order of the database attributes
  data_attributes_reorder <- subset(db_attributes, db_attributes %in% data_attributes)
  
  # Now, use select to reorder the dataset to the right order
  data_out <- data %>%
    select(data_attributes_reorder)
  
  # ...If there are more attributes in your dataset than in the subsetted
  #   database attributes, and therefore there are attributes in your dataset
  #   for which there is no guidance in the database...
  if (length(data_attributes_reorder) < length(data_attributes)) {
    # ...Notify the user
    print(paste0("Some columns in your data are not present in the current database guidelines.
                 Removing these from dataset and saving to `", datalevel_table, "_uncontrolled_attributes.csv`."))
  }
  
  # Figure out what attributes are missing guidance by removing all of the common
  #   attributes between 'data' and 'data_out', leaving just the ones missing from
  #   data_out
  uncontrolled_attributes <- data %>%
    select(-one_of(colnames(data_out)))
  
  # Save uncontrolled attributes to a .csv if they exist
  if (ncol(uncontrolled_attributes) > 0) {
    write_csv(uncontrolled_attributes, paste0(getwd(), uncontrolled_file_path, 
                                              datalevel_table, "_",
                                              "uncontrolled_attributes.csv"))
  }
  
  # Return re-ordered dataset
  return(data_out)
  }

## Test numeric columns #############

test_numeric_vars <- function(input_data) {
  
  controlled_numeric_attributes <- read_csv("./docs/controlled_attributes.csv", col_types = cols()) %>%
    filter(data_type == "numeric")
  
  numeric_attributes <- read_csv("./docs/uncontrolled_attributes.csv", col_types = cols()) %>%
    filter(data_type == "numeric") %>%
    bind_rows(controlled_numeric_attributes)
  
  # select only numeric columns 
  to_check <- subset(colnames(input_data), colnames(input_data) %in% numeric_attributes$attribute_name)
  testing_data <- input_data %>%
    ungroup() %>%
    select(to_check) 
  
  library(skimr)
  
  skim_with_defaults()
  
  # list of functions to run on numeric attributes
  funs <- list(
    min = function(x) min(x, na.rm=TRUE), 
    max = function(x) max(x, na.rm=TRUE), 
    median = function(x) median(x, na.rm=TRUE),
    na_count = function(x) sum(is.na(x)),
    NaN_count = function(x) sum(is.nan(x)),
    p0 = NULL, 
    p25 = NULL, 
    p50 = NULL, 
    p75 = NULL, 
    p100 = NULL,
    hist = NULL
  )
  
  # Set skimr to run with our custom list of functions
  skim_with(numeric = funs, append = TRUE)
  
  # Organize into a wide form table 
  results <- testing_data %>%
    skim() %>%
    select(variable, type, stat, formatted) %>%
    spread(stat, formatted) %>%
    select(variable, type, n, min, max, median, mean, sd, missing, na_count, NaN_count)
  
  return(results)
}
