## Check core_id uniqueness ########
# All cores in synthesis should have unique id

testUniqueCores <- function(data) {
  # First, get a list of all unique core_ids
  # Remove all cores with only one occurance
  core_list <- data %>%
    group_by(core_id) %>%
    summarize(n = n()) %>%
    filter(n > 1)
  
  if(length(core_list$core_id)>0){
    result <- paste0("Check the following core_id(s) in the core-level data: ", paste(core_list$core_id, collapse = ", "))
  } else {
    result <- ("Passed")
  }
  
  setNames(data.frame(matrix(c("Core ID uniqueness", result),nrow=1,ncol=2)), c("Test", "Result")) %>%
    mutate(Test = as.character(Test),
           Result = as.character(Result))
  
}

# Ensure column names match attributes in the database structure 
testAttributeNames <- function(tables, ccrcn_synthesis) {
  # Remove "studycitations" from tables as it is independent of this test
  tables <- tables[!tables %in% "studycitations"]
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) %>%
    bind_rows(read_csv("docs/uncontrolled_attributes.csv")) %>%
    # Only need column names in the tables of the synthesis
    filter(table %in% tables)
   
  # Create empty dataframe for test results
  results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))

  # Find invalid attribute names in each table
  for(i in seq_along(tables)){
    table_type <- tables[i]
    # Extract column names for that table and subset to all columns that aren't in the controlled or uncontrolled attribute tables
    column_names <- colnames(ccrcn_synthesis[[table_type]])
    valid_columns <- filter(database_structure, table == table_type)$attribute_name
    non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
    
    if(length(non_matching_columns) > 0){
      table_result <- paste("Undefined columns:", paste(non_matching_columns, collapse=", "))
    } else table_result <- "Passed"
    
    results <- setNames(data.frame(matrix(c(table_type, table_result), 
                                                    nrow=1, ncol=2)), c("Test", "Result")) %>%
      mutate(Test = as.character(Test),
             Result = as.character(Result)) %>%
      bind_rows(results)
      
  }
  
  # Format Test column and return results
  mutate(results, Test = paste0("Validity of column names in ", Test, " table"))
  
}

# Test variable names to make sure they are in database structure
testVariableNames <- function(tables, ccrcn_synthesis) {
  
  # Remove "studycitations" from tables as it is independent of this test
  tables <- tables[!tables %in% "studycitations"]
  
  controlled_variables_list <- read_csv("./docs/controlled_variables.csv",
                                        col_types = cols())
  
  # Gather all attributes to that can be tested
  attributes_names <- unique(controlled_variables_list$attribute_name)
  # Create empty dataframe for test results
  results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))
  
  # Check each table
  for(i in seq_along(tables)){
    table_type <- tables[i]
    
    to_check <- subset(colnames(ccrcn_synthesis[[table_type]]), 
                       colnames(ccrcn_synthesis[[table_type]]) %in% attributes_names)
    df_to_check <- select(ccrcn_synthesis[[table_type]], to_check)
    
    # Create empty vector
    invalid_variables <- c()
    # Check each column one at a time
    # Append any invalid variables to a vector
    if(!is_empty(to_check)){
      for(j in 1:length(to_check)){
        
        attribute <- to_check[j]
        variable_list <- filter(controlled_variables_list, attribute_name == attribute)
        
        x <- df_to_check %>%
          filter(!(get(attribute) %in% variable_list$variable_name))
        
        invalid_variables <- append(invalid_variables, na.omit(unique(get(attribute, x))))
      }
      
      if(!is_empty(invalid_variables)) {
        table_result <- paste("Undefined variables:", paste(invalid_variables, collapse=", "))
      } else table_result <- "Passed"
      
      results <- setNames(data.frame(matrix(c(table_type, table_result), 
                                            nrow=1, ncol=2)), c("Test", "Result")) %>%
        mutate(Test = as.character(Test),
               Result = as.character(Result)) %>%
        bind_rows(results)
      
    }
  }
  
  # Format Test column and return results
  mutate(results, Test = paste0("Validity of variable names in ", Test, " table"))
  
}

testCoreRelationships <- function(ccrcn_synthesis) {
  # this will return any core_ids that do not have a match in the core-level and depthseries data
  core_results <- (anti_join(ccrcn_synthesis$cores, ccrcn_synthesis$depthseries, by="core_id"))$core_id
  depthseries_results <- (anti_join(ccrcn_synthesis$depthseries, ccrcn_synthesis$cores, by="core_id"))$core_id
  
  # Create empty dataframe for test results
  results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))
  
  if(length(core_results) > 0){
    core_results <- paste0("No core ID in depthseries table: ", 
                          paste(core_results, collapse = ", "))
  } else core_results <- "Passed"
  
  results <- setNames(data.frame(matrix(c(table_type, core_results), 
                                        nrow=1, ncol=2)), c("Test", "Result")) %>%
    mutate(Test = as.character("Valid core ID links in core table"),
           Result = as.character(Result)) %>%
    bind_rows(results)
  
  if(length(depthseries_results) > 0) {
    depthseries_results <- paste0("No core ID in core table: ", 
                           paste(depthseries_results, collapse = ", "))
  } else depthseries_results <- "Passed"
  
  setNames(data.frame(matrix(c(table_type, depthseries_results), 
                                        nrow=1, ncol=2)), c("Test", "Result")) %>%
    mutate(Test = as.character("Valid core ID links in depthseries table"),
           Result = as.character(Result)) %>%
    bind_rows(results)
  
}



# Provide summary statistics of numeric variables 
testNumericVariables <- function(depthseries) {
  
  # Get both controlled and approved uncontrolled variables
  numeric_attributes <- read_csv("./docs/uncontrolled_attributes.csv", col_types = cols()) %>%
    bind_rows(read_csv("./docs/controlled_attributes.csv", col_types = cols())) %>%
    filter(data_type == "numeric") 
  
  # select only numeric columns 
  to_check <- subset(colnames(depthseries), 
                     colnames(depthseries) %in% numeric_attributes$attribute_name)
  testing_data <- depthseries %>%
    ungroup() %>%
    select(to_check) 
  
  ## Skimr package updated with substantial code changes

  # list of functions to run on numeric attributes
  funs <- sfl(
    min = function(x) min(x, na.rm=TRUE), 
    max = function(x) max(x, na.rm=TRUE), 
    median = function(x) median(x, na.rm=TRUE),
    mean = function(x) mean(x, na.rm=TRUE),
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
  # V2 skimr requires assigning skim_with to object
  my_skim <- skim_with(numeric = funs, append = TRUE)
  
  # Organize into a wide form table and return
  # testing_data %>%
  #   skim() %>%
  #   select(variable, type, stat, formatted) %>%
  #   spread(stat, formatted) %>%
  #   select(variable, type, n, min, max, median, mean, sd, missing, na_count, NaN_count)
  
  testing_data %>%
    my_skim()
  
}

## Check lat/long uniqueness ########
# Cores in synthesis should likely have unique lat/long values
testUniqueCoordinates <- function(cores) {
  # First, get a list of all unique coordinate combinations
  # And filter out all that only are associated with one core ID
  core_list <- cores %>%
    mutate(lat_long = paste(core_latitude,core_longitude, sep=", ")) %>%
    group_by(lat_long) %>%
    summarize(n = n(), 
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1)
  
  # If there are cores associated with the same coordinates
  if(length(core_list$core_ids)>0){
    # Write results to a csv 
    write_csv(core_list, "./data/QA/duplicate_coordinates.csv")
    # Create results dataframe
    results <- setNames(data.frame(x="Test coordinate uniqueness", 
                                   y=paste0(nrow(core_list), 
                                            " sets of coordinates are associated with more than one core. ",
                                            "Check 'data/QA/duplicate_coordinates.csv'")),
                        c("Test", "Result"))
  } else {
    results <- setNames(data.frame(x="Test coordinate uniqueness", y="Passed"), c("Test", "Result"))
  }
  
  # Format and return results
  results %>%
    mutate(Test = as.character(Test),
           Result = as.character(Result)) 
}

# Reorder columns to reflect guidance 
reorderColumns <- function(tables, ccrcn_synthesis) {
  
  # Remove "studycitations" from tables as it is independent of this test
  tables <- tables[!tables %in% "studycitations"]
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) 
   
  for(table_type in tables){
    # Get controlled attributes for the current table
    table_structure <- database_structure %>%
      filter(table == table_type) %>%
      filter(attribute_name %in% colnames(ccrcn_synthesis[[table_type]]))
    
    # Controlled attributes selected first, then all approved uncontrolled attributes
    ccrcn_synthesis[[table_type]] <- ccrcn_synthesis[[table_type]] %>%
      select(table_structure$attribute_name, everything())
  } 
  
  # Return synthesis
  ccrcn_synthesis
}
