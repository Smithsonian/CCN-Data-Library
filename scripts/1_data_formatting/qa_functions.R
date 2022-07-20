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
    warning("Check the following core_ids in the core-level data:")
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
    mutate(lat_long = paste(latitude, longitude, sep=",")) %>%
    group_by(lat_long) %>%
    summarize(n = n(), 
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1)

  if(length(core_list$core_ids)>0){
    warning("Some cores in the core-level data have duplicate coordinates. Check 'data/QA/duplicate_cores.csv' for the list.")
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
    warning("Check the following core_ids in the core-level data:")
    print(unique(results))
  } 
  
  if(length(results2) > 0) {
    warning("Check the following core_ids in the depthseries data:")
    print(unique(results2))
  }
  
  if(length(results)== 0 & length(results2) == 0) {
    print("Core IDs match.")
  }
  append(results,results2)
  return(unique(results))
}


## Ensure fractions not percetanges #################

# This function reviews all attributes that ought to be a fraction and determines
#   whether they are in their proper format (e.g. less than 1)

fraction_not_percent <- function(dataset) {
  
  relevant_cols <- dataset %>%
    select(contains("fraction")) %>% # are there any fraction cols that don't include "fraction" in the colname?
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
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  if(category %in% database_structure$table == FALSE) {
    # Warn user they have not supplied a valid table category and provide the list 
    warning(paste(category,"is not a valid category. Please use one of the above listed options."))
    print(tables)
    return()
  }
  
  # Gather column names from dataset and pull out columns with variables that are defined in our database structure
  column_names <- colnames(dataset)
  valid_columns <- filter(database_structure, table == category)$attribute_name
  non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
  
  if(length(non_matching_columns)==0) {
    print("Looks good! All column names match CCRCN standards")
  } else {
    warning(paste(c("The following column headers do not match CCRCN guidance:", non_matching_columns), collapse=" "))
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
  db_attributes <- c(database_structure$attribute_name)
  
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
    warning(paste0("Some columns in your data are not present in the current database guidelines.
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
  
  numeric_attributes <- read_csv("./docs/ccrcn_database_structure.csv", col_types = cols()) %>%
    filter(data_type == "numeric")
  
  # select only numeric columns 
  to_check <- subset(colnames(input_data), colnames(input_data) %in% numeric_attributes$attribute_name)
  testing_data <- input_data %>%
    ungroup() %>%
    select(all_of(to_check))
  
  library(skimr)
  
  funs <- sfl(
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
  my_skim <- skim_with(numeric = funs, append = TRUE)
  
  results <- testing_data %>%
    my_skim()
  
  return(results)
}

# Check the data types of all variables 
# The function attempts to convert all attributes that should be factors but are character type
# Other type conversions are not conducted since unexpected results could occur. Users are encouraged to 
# conduct all other data type conversions elsewhere in the hook script and check for any underlying 
# issues in the data that could be leading toward an incorrect data type classification (commas in numeric columns, etc.)
testDataTypes <- function(df) {
  library(lubridate)
  
  controlled_numeric_attributes <- read_csv("./docs/controlled_attributes.csv", col_types = cols()) 
  
  all_attributes <- read_csv("./docs/uncontrolled_attributes.csv", col_types = cols()) %>%
    bind_rows(controlled_numeric_attributes)
  
  for(i in 1:length(colnames(df))){
    attribute <- colnames(df)[i]
    data_type <- filter(all_attributes, attribute_name == attribute)$data_type
    if(data_type == "numeric"){
      if(!is.numeric(df[[attribute]])) {
        warning(paste(attribute,"is not in the numeric data type."))    
      }
      
    } else if (data_type == "character") {
      if(!is.character(df[[attribute]])) {
        warning(paste(attribute,"is not in the character data type."))    
      }
      
    } else if (data_type == "factor") {
      if(!is.factor(df[[attribute]])) {
        warning(paste(attribute,"is not in the factor data type. Attempting to convert..."))
        df <- mutate(df := as.factor(!! sym(attribute)))
      }
      
    } else if (data_type == "Date") {
      if(!is.Date(df[[attribute]])) {
        warning(paste(attribute,"is not in the Date data type."))    
      }
    }
  }
  
  return(df)
}

## Reorder columns to reflect guidance ####
reorderColumns <- function(category, df) {
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  if(category %in% database_structure$table == FALSE) {
    # Warn user they have not supplied a valid table category and provide the list 
    warning(paste(category,"is not a valid category. Please use one of the above listed options."))
    print(tables)
    return()
  }
  
  # Get controlled attributes for the current table
  table_structure <- database_structure %>%
    filter(table == category) %>%
    filter(attribute_name %in% colnames(df))
  
  # Controlled attributes selected first, then all approved uncontrolled attributes
  df %>% select(table_structure$attribute_name, everything())
  
}

## Test column names in table list ###########

# Make sure column names match CCRCN guidelines
testTableCols <- function(table_names, current_version = TRUE) {
  # create a list of datasets from the provided table names
  datasets <- mget(table_names, envir = .GlobalEnv)
  
  # load guidance
  if(current_version == FALSE){
    database_structure <- read_csv("docs/versioning/archive/ccrcn_database_structure_v1.csv", col_types = cols())
  }else{
    database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  }
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  # controlled_list <- list()
  for (i in 1:length(datasets)) {
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$table == FALSE) {
      # Warn user they have not supplied a valid table categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    # Gather column names from dataset and pull out columns with variables that are defined in our database structure
    column_names <- names(datasets[[i]])
    valid_columns <- filter(database_structure, table == category)$attribute_name
    non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
    
    # workflow to store/return matching colnames..don't think this is necessary
    # matching_columns <- subset(column_names, (column_names %in% valid_columns))
    # subset table of controlled attributes for each group
    # controlled <- database_structure %>%
    #   filter(table == category & attribute_name %in% matching_columns) %>%
    #   select(table, attribute_name, data_type, format_unit_codes)
    # controlled_list[[i]] <- controlled # not sure why this is necessary
    
    if(length(non_matching_columns)==0) {
      print("Looks good! All column names match CCRCN standards")
    } else {
      print(paste(c("Non-matching attributes in", category, ":", non_matching_columns), collapse=" "))
    }
  }
  # concatenate list of controlled attributes 
  # controlled_all <- do.call(rbind, controlled_list)
  # return(controlled_all)
}

# test
# testTableCols(table_names = table_names, version = "1")

## Test variables from a table list ########

testTableVars <- function(table_names) {
  # load controlled vars
  controlled_variables_list <- read_csv("docs/controlled_variables.csv", col_types = cols())
  
  # Subset controlled variables by the attributes that are in the tested data frame
  var_names <- unique(controlled_variables_list$attribute_name)
  
  # create a list of datasets from the provided table names
  datasets <- mget(table_names, envir = .GlobalEnv)
  
  # create df to store invalid vars for all tables
  invalid_df <- data.frame() 
  
  # loop through each dataset
  for (k in 1:length(datasets)) {
    
    dataset <- datasets[[k]] # store table
    table_subset <- dataset %>% select(any_of(var_names)) # subset table
    # [, which(names(dataset) %in% var_names)] 

    # Check each column of current dataset
    # Append any invalid variables to the empty data frame
    if(!is_empty(table_subset)){
      
      for(i in 1:length(names(table_subset))){
        # identify attribute to find corresponding variables for
        attribute <- names(table_subset)[i] 
        # list controlled variables for attribute in question
        controlled_vars <- controlled_variables_list %>% filter(attribute_name == attribute) %>% pull(variable_name)
        
        x <- table_subset %>% filter(!(get(attribute) %in% controlled_vars))
        
        invalid_variables <- na.omit(unique(get(attribute, x)))
        
        if(length(invalid_variables) > 0) {
          invalid <- data.frame("table" = names(datasets)[k],
                                "attribute_name" = rep(attribute, length(invalid_variables)),
                                "variable_name" = invalid_variables)
          invalid_df <- bind_rows(invalid_df, invalid)
        }
      }
    }
  }
  # If there are no invalid variables don't pass along the df 
  # Otherwise indicate to the user there are problems and to check the table 
  if(nrow(invalid_df)==0) {
    print("Looks good! All variable names match CCRCN standards")
    
  } else {
    print("View resulting invalid variable names:")
    return(invalid_df)  
  }
}

# example code
# testTableVars(table_names = table_names)

## Check for required attributes ####
# compile a table of the required attributes missing in each table
testRequired <- function(table_names){
  # read in database guidance
  guidance <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) 
  
  # isolate required attributes
  required <- guidance %>% filter(required == "required") %>%
    filter(attribute_name != "study_id") %>%
    select(table, attribute_name, required)
  
  # create a list of datasets from the provided table names
  tables <- mget(table_names, envir = .GlobalEnv)
  
  # create df to store results
  missing_required <- data.frame()
  
  # loop through all the tables
  for(table in 1:length(tables)){
    # store table name
    table_name <- names(tables)[table]
    # subset the required cols for given table
    required_for_table <- required %>% filter(table == table_name) %>% pull(attribute_name)
    
    # create table of attributes present in each table for each study
    table_cols <- tables[[table_name]] %>%
      mutate_all(as.character) %>%
      pivot_longer(cols = -study_id, names_to = "attribute_name", values_to = "value") %>%
      drop_na(value) %>%
      select(-value) %>% distinct() %>%
      # filter for required attributes
      filter(attribute_name %in% required_for_table)
    
    # identify missing attributes in current table
    missing_cols <- required_for_table[which(!(required_for_table %in% table_cols$attribute_name))]
    
    if(length(missing_cols) > 0){
      # create table to identify studies lacking required attributes in their tables 
      missing_from_study <- data.frame(table = table_name,
                                       missing_attributes = missing_cols)
      
      # compile into missing required table
      # output table should have cols: table, missing_attributes
      missing_required <- bind_rows(missing_required, missing_from_study)
    }
  }
  
  if(!is_empty(missing_required)){
    return(missing_required)
  } else {
    print("All required attributes are present!")
  }
}

## Check for duplicate cores in the database ####
checkDuplicates <- function(){
  # read in CCRCN synthesis tables (cores and depthseries)
  cores <- read_csv("data/CCRCN_synthesis/original/CCRCN_cores.csv", guess_max = 10000, col_types = cols())
  depthseries <- read_csv("data/CCRCN_synthesis/original/CCRCN_depthseries.csv", guess_max = 100000, col_types = cols())
  
  # Strategy: look for similarities in values across coords and depth intervals
  
  # check out cores table
  core_subset <- cores %>%
    group_by(latitude, longitude) %>%
    summarize(n = n(),
              core_ids = paste(unique(core_id), collapse = ", "),
              study_ids = paste(unique(study_id), collapse = ", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1 & num_studies > 1)
  
  # depthseries
  ds_subset <- depthseries %>%
    group_by(depth_min, depth_max, dry_bulk_density,
             fraction_carbon, fraction_organic_matter) %>%
    summarise(n = n(),
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1 & num_studies > 1)
  
  # join_subsets <- left_join(core_subset, ds_subset)
  
  # if(length(core_list$core_ids)>0){
  #   warning("Some cores in the core-level data have duplicate coordinates. Check 'data/QA/duplicate_cores.csv' for the list.")
  # } else {
  #   print("All core coordinates are unique.")
  # }
  
  print("Function is still in the works.")
}

## Check for conditionally required attributes ####

## Need a new QA function that iterates through the guidence, makes sure that every required attribute,
# and any conditional are there. (repurposed from the SWG, written by James Holmquist)
testConditional <- function(table_names, database_structure_doc = "docs/ccrcn_database_structure.csv") {
  # original guidance in the SWG
  # biomass/agb_biomass_inundation/docs/CCN Biomass and Elevation Data Synthesis Guidence Draft 200925.csv
  
  database_structure <- readr::read_csv(database_structure_doc, col_types = cols())
  datasets <- mget(table_names, envir = .GlobalEnv)
  
  requirement_warnings <- c()
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  for (i in 1:length(datasets)) {
    
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$table == FALSE) {
      # Warn user they have not supplied a valid table categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    this_table = category
    
    # conditional attributes
    conditional_attributes <- database_structure %>%
      dplyr::filter(table == this_table,
                    required == "conditional",
                    ! is.na(conditional_on)) %>% 
      separate_rows(conditional_on, sep="; ") %>% 
      filter(conditional_on %in% names(datasets[[i]]))
    
    if (all(conditional_attributes$attribute_name %in% names(datasets[[i]]))) {
      requirement_warnings <- c(requirement_warnings, 
                                paste(this_table, " (conditional): all conditional attributes present.", sep=""))
    } else {
      missing_attributes <- unique(conditional_attributes$attribute_name[! (conditional_attributes$attribute_name %in% names(datasets[[i]]))])
      requirement_warnings <- c(requirement_warnings, 
                                paste(this_table, " (conditional): ",
                                      paste(missing_attributes, sep="", collapse = ", "),
                                      " missing.", sep=""))
    }
  }
  print(requirement_warnings)
}

writeDataVizReport <- function(study_id){
  
  # generate data contributor report
  rmarkdown::render(input = "./scripts/data_visualization_report.Rmd",
                    # output_format = "html_document",
                    output_file = paste0(study_id, "_dataviz_report"),
                    output_dir = "./docs/dataviz_reports/")
}
