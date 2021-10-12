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
  
  # setNames(data.frame(matrix(c("Core ID uniqueness", result),nrow=1,ncol=2)), c("Test", "Result")) %>%
  #   mutate(Test = as.character(Test),
  #          Result = as.character(Result))

  qa_results <<- qa_results %>%
    add_row(test = as.character("Core ID uniqueness"),
            result = as.character(result))
  
}

# Ensure column names match attributes in the database structure 
testAttributeNames <- function(tables, ccrcn_synthesis) {
  # Remove "studycitations" from tables as it is independent of this test
  tables <- tables[!tables %in% "studycitations"]
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) %>%
    # bind_rows(read_csv("docs/uncontrolled_attributes.csv", col_types = cols())) %>%
    # Only need column names in the tables of the synthesis
    filter(table %in% tables)
   
  # Create empty dataframe for test results
  # results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))

  
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
    
    # results <- setNames(data.frame(matrix(c(table_type, table_result), 
    #                                                 nrow=1, ncol=2)), c("Test", "Result")) %>%
    #   mutate(Test = as.character(Test),
    #          Result = as.character(Result)) %>%
    #   bind_rows(results)
      
    qa_results <<- qa_results %>%
      add_row(test = as.character(paste0("Validity of column names in ", table_type, " table")),
              result = as.character(table_result))
    
  }
  
  # Format Test column and return results
  # mutate(results, Test = paste0("Validity of column names in ", Test, " table"))
  
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
  # results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))
  
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
      
      # results <- setNames(data.frame(matrix(c(table_type, table_result), 
      #                                       nrow=1, ncol=2)), c("Test", "Result")) %>%
      #   mutate(Test = as.character(Test),
      #          Result = as.character(Result)) %>%
      #   bind_rows(results)
      
      qa_results <<- qa_results %>%
        add_row(test = as.character(paste0("Validity of variable names in ", table_type, " table")),
                result = as.character(table_result))
      
      
    }
  }
  
  # Format Test column and return results
  # mutate(results, Test = paste0("Validity of variable names in ", Test, " table"))
  
}

testCoreRelationships <- function(ccrcn_synthesis) {
  # this will return any core_ids that do not have a match in the core-level and depthseries data
  core_results <- (anti_join(ccrcn_synthesis$cores, ccrcn_synthesis$depthseries, by="core_id"))$core_id
  depthseries_results <- (anti_join(ccrcn_synthesis$depthseries, ccrcn_synthesis$cores, by="core_id"))$core_id
  
  # Create empty dataframe for test results
  results <- setNames(data.frame(matrix(ncol=2, nrow=0)), c("Test", "Result"))
  
  if(length(core_results) > 0){
    core_results <- paste0("No core ID in depthseries table: ", 
                          paste(unique(core_results), collapse = ", "))
  } else core_results <- "Passed"
  
  # results <- setNames(data.frame(matrix(c(table_type, core_results), 
  #                                       nrow=1, ncol=2)), c("Test", "Result")) %>%
  #   mutate(Test = as.character("Valid core ID links in core table"),
  #          Result = as.character(Result)) %>%
  #   bind_rows(results)
  
  qa_results <<- qa_results %>%
    add_row(test = as.character("Valid core ID links in core table"),
            result = as.character(core_results))
  

  if(length(depthseries_results) > 0) {
    depthseries_results <- paste0("No core ID in core table: ", 
                           paste(unique(depthseries_results), collapse = ", "))
  } else depthseries_results <- "Passed"
  
  # setNames(data.frame(matrix(c(table_type, depthseries_results), 
  #                                       nrow=1, ncol=2)), c("Test", "Result")) %>%
  #   mutate(Test = as.character("Valid core ID links in depthseries table"),
  #          Result = as.character(Result)) %>%
  #   bind_rows(results)
  
  qa_results <<- qa_results %>%
    add_row(test = as.character("Valid core ID links in depthseries table"),
            result = as.character(depthseries_results))
  
}



# Provide summary statistics of numeric variables 
testNumericVariables <- function(depthseries) {
  
  # Get both controlled and approved uncontrolled variables
  numeric_attributes <- read_csv("./docs/controlled_attributes.csv", col_types = cols()) %>%
    # bind_rows(read_csv("./docs/uncontrolled_attributes.csv", col_types = cols())) %>%
    filter(data_type == "numeric") 
  
  # select only numeric columns 
  to_check <- subset(colnames(depthseries), 
                     colnames(depthseries) %in% numeric_attributes$attribute_name)
  testing_data <- depthseries %>%
    ungroup() %>%
    select(to_check) %>%
    mutate_all(as.numeric)
  
  ## Skimr package updated with substantial code changes

  # list of functions to run on numeric attributes
  funs <- skimr::sfl(
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
  my_skim <- skimr::skim_with(numeric = funs, append = TRUE)
  
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
    mutate(lat_long = paste(latitude, longitude, sep=", ")) %>%
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
    # results <- setNames(data.frame(x="Test coordinate uniqueness", 
    #                                y=paste0(nrow(core_list), 
    #                                         " sets of coordinates are associated with more than one core. ",
    #                                         "Check 'data/QA/duplicate_coordinates.csv'")),
    #                     c("Test", "Result"))
    
    qa_results <<- qa_results %>%
      add_row(test = as.character("Test coordinate uniqueness"),
              result = paste0(nrow(core_list), 
                              " sets of coordinates are associated with more than one core. ",
                              "Check 'data/QA/duplicate_coordinates.csv'"))
    
    
  } else {
    # results <- setNames(data.frame(x="Test coordinate uniqueness", y="Passed"), c("Test", "Result"))
    qa_results <<- qa_results %>%
      add_row(test = as.character("Test coordinate uniqueness"),
              result = as.character("Passed"))
    
  }
  
  # Format and return results
  # results %>%
  #   mutate(Test = as.character(Test),
  #          Result = as.character(Result)) 
  
}

# Reorder columns to reflect guidance 
reorderColumns <- function(tables, ccrcn_synthesis) {
  
  # Remove "studycitations" from tables as it is independent of this test
  # tables <- tables[!tables %in% "study_citations"]
  
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


## Check for required attributes ####

# compile a table of studies and the required attributes they are missing

testRequired <- function(){
  # read in database guidance
  guidance <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) 
  # isolate required attributes
  required <- guidance %>% filter(required == "required") %>%
    filter(attribute_name != "study_id") %>%
    select(table, attribute_name, required)
  
  # read in CCRCN synthesis tables
  methods <- read_csv("data/CCRCN_synthesis/original/CCRCN_methods.csv", col_types = cols())
  cores <- read_csv("data/CCRCN_synthesis/original/CCRCN_cores.csv", guess_max = 10000, col_types = cols())
  depthseries <- read_csv("data/CCRCN_synthesis/original/CCRCN_depthseries.csv", guess_max = 100000, col_types = cols())
  study_citations <- read_csv("data/CCRCN_synthesis/original/CCRCN_study_citations.csv", col_types = cols())
  # site, impact, and species tables are optional
  # but if they are included, there are required attributes
  
  # create list from all the tables with required attributes
  tables <- list(cores = cores, 
                 depthseries = depthseries,
                 methods = methods,
                 study_citations = study_citations)
  
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
    
    # condense study attributes into a list
    study_attributes <- table_cols %>% group_by(study_id) %>%
      # study id has to be added manually (which defeats the purpose a bit)
      summarize(attribute_list = list(attribute_name))
    
    # loop through studies 
    for(i in 1:nrow(study_attributes)){
      # identify missing attributes in each study
      missing_cols <- required_for_table[which(!(required_for_table %in% study_attributes$attribute_list[[i]]))]
      
      if(length(missing_cols) > 0){
        # create table to identify studies lacking required attributes in their tables 
        missing_from_study <- data.frame(study_id = study_attributes$study_id[i],
                                         table = table_name,
                                         missing_attributes = missing_cols)
        
        # compile into missing required table
        # output table should have cols: study_id, table, missing_attributes
        missing_required <- bind_rows(missing_required, missing_from_study)
      }
    }
  }
  
  if(!is_empty(missing_required)){
    return(missing_required)
    write_csv(missing_required, "data/QA/missing_required_attributes.csv")
    print("Missing attributes table output to data/QA/missing_required_attributes.csv")
  } else {
    print("All required attributes are present!")
  }
}

## Check for conditional attributes ####

# compile a table of studies and the required attributes they are missing

# testConditional <- function(){
#   # read in database guidance
#   guidance <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) 
#   # isolate required attributes
#   conditional <- guidance %>% filter(required == "conditional") %>%
#     # filter(attribute_name != "study_id") %>%
#     select(table, attribute_name, required, contains("conditional"))
#   
#   # read in CCRCN synthesis tables
#   methods <- read_csv("data/CCRCN_synthesis/original/CCRCN_methods.csv", col_types = cols())
#   cores <- read_csv("data/CCRCN_synthesis/original/CCRCN_cores.csv", guess_max = 10000, col_types = cols())
#   depthseries <- read_csv("data/CCRCN_synthesis/original/CCRCN_depthseries.csv", guess_max = 100000, col_types = cols())
#   species <- read_csv("data/CCRCN_synthesis/original/CCRCN_species.csv", col_types = cols())
#   # site, impact, and species tables are optional
#   # but if they are included, there are required attributes
#   
#   # create list from all the tables with required attributes
#   tables <- list(cores = cores, 
#                  depthseries = depthseries,
#                  methods = methods,
#                  species = species)
#   
#   # create df to store results
#   missing_conditional <- data.frame()
#   
#   # if an attribute is present, 
#   # check to see if there is a conditional variable
#   # if the variable is a certain value
#   # check for presence of conditional attribute(s)
#   
#   # loop through all the tables
#   for(table in 1:length(tables)){
#     # store table name
#     table_name <- names(tables)[table]
#     # subset the required cols for given table
#     conditional_for_table <- conditional %>% filter(table == table_name) %>% pull(conditional_on)
#     
#     # create table of attributes present in each table for each study
#     table_cols <- tables[[table_name]] %>%
#       mutate_all(as.character) %>%
#       pivot_longer(cols = -study_id, names_to = "attribute_name", values_to = "value") %>%
#       drop_na(value) %>%
#       select(-value) %>% distinct() %>%
#       # filter for required attributes
#       filter(attribute_name %in% required_for_table)
#     
#     # condense study attributes into a list
#     study_attributes <- table_cols %>% group_by(study_id) %>%
#       # study id has to be added manually (which defeats the purpose a bit)
#       summarize(attribute_list = list(attribute_name))
#     
#     # loop through studies 
#     for(i in 1:nrow(study_attributes)){
#       # identify missing attributes in each study
#       missing_cols <- required_for_table[which(!(required_for_table %in% study_attributes$attribute_list[[i]]))]
#       
#       if(length(missing_cols) > 0){
#         # create table to identify studies lacking required attributes in their tables 
#         missing_from_study <- data.frame(study_id = study_attributes$study_id[i],
#                                          table = table_name,
#                                          missing_attributes = missing_cols)
#         
#         # compile into missing required table
#         # output table should have cols: study_id, table, missing_attributes
#         missing_required <- bind_rows(missing_required, missing_from_study)
#       }
#     }
#   }
#   
#   if(!is_empty(missing_required)){
#     return(missing_required)
#     write_csv(missing_required, "data/QA/missing_required_attributes.csv")
#     print("Missing attributes table output to data/QA/missing_required_attributes.csv")
#   } else {
#     print("All required attributes are present!")
#   }
# }
