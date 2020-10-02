## CCRCN Soils Working Group QA/QC scripts
# contact: Michael Lonneman, lonnemanM@si.edu 
#          Jaxine Wolfe, wolfejax@si.edu
# J. Holmquist made edits

## Compile a list from given datasets ########
create_object_list <- function(table_names){
  # get objects from the environment using provided name
  table_list <- mget(table_names, envir = .GlobalEnv)
  
  return(table_list)
}

## Test column names function ###########
# Make sure column names match CCRCN guidelines
test_colnames <- function(datasets, database_structure_doc = "scripts/3_post_processing/tables/input_files/converting_v1p2_to_v2.csv") {
  
  database_structure <- readr::read_csv(database_structure_doc, col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$heriarchy)
  
  controlledlist <- list()
  
  for (i in 1:length(datasets)) {
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$heriarchy == FALSE) {
      # Warn user they have not supplied a valid table categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    # Gather column names from dataset and pull out columns with variables that are defined in our database structure
    column_names <- names(datasets[[i]])
    valid_columns <- filter(database_structure, heriarchy == category)$attribute_name
    non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
    matching_columns <- subset(column_names, (column_names %in% valid_columns))
    
    # subset table of controlled attributes for each group
    controlled <- database_structure %>%
      filter(heriarchy == category & attribute_name %in% matching_columns) %>%
      select(heriarchy, attribute_name, data_type, units)
    
    controlledlist[[i]] <- controlled
    
    if(length(non_matching_columns)==0) {
      print("Looks good! All column names match CCRCN standards")
    } else {
      print(paste(c("Non-matching column headers for", category, ":", non_matching_columns), collapse=" "))
    }
  }
  # concatenate list of controlled attributes 
  controlled_all <- do.call(rbind, controlledlist)
  return(controlled_all)
}

## Need a new QA function that iterates through the guidence, makes sure that every required attribute,
# and any conditional are there.
test_requirements <- function(datasets, database_structure_doc = "scripts/3_post_processing/tables/input_files/converting_v1p2_to_v2.csv") {
  database_structure <- readr::read_csv(database_structure_doc, col_types = cols())
  
  requirement_warnings <- c()
  # Create a vector of all the table names
  tables <- unique(database_structure$heriarchy)
  
  for (i in 1:length(datasets)) {
    
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$heriarchy == FALSE) {
      # Warn user they have not supplied a valid table categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    this_table = tables[i]
    required <- database_structure %>% 
      dplyr::filter(heriarchy == this_table,
                    required == "required")
    
    if (all(required$attribute_name %in% names(datasets[[i]]))) {
      requirement_warnings <- c(requirement_warnings, 
                                paste(this_table, ": all required attributes present.", sep="")
      )
    } else {
      missing_attributes <- required$attribute_name[! (required$attribute_name %in% names(datasets[[i]]))]
      requirement_warnings <- c(requirement_warnings, 
                                paste(this_table, " (required) : ",
                                      paste(missing_attributes, sep="", collapse = ", "),
                                      " missing.", sep="")
      )
      
      # TODO: Create a separate workflow for conditional attributes getting turned on or off by certain settings 
      # Iterate through all of the attributes that require factors
      # Isolate them in a file
      # Iterate one by one
      # Check to see if the setting that triggers the conditionality is somewhere in the data
      # If so add it to a list of conditional attributes
      
      # conditional attributes
      # TODO: Exclude factors
      conditional_attributes <- database_structure %>%
        dplyr::filter(heriarchy == this_table,
                      required == "conditional",
                      ! is.na(conditional_on)) %>% 
        separate_rows(conditional_on, sep="; ") %>% 
        filter(conditional_on %in% names(datasets[[i]]))
      # TODO: Include any of the conditional attributes that were triggered by certain factors
      
      if (all(conditional_attributes$attribute_name %in% names(datasets[[i]]))) {
        requirement_warnings <- c(requirement_warnings, 
                                  paste(this_table, " (conditional): all conditional attributes present.", sep="")
        )
      } else {
        missing_attributes <- conditional_attributes$attribute_name[! (conditional_attributes$attribute_name %in% names(datasets[[i]]))]
        
        requirement_warnings <- c(requirement_warnings, 
                                  paste(this_table, " (conditional): ",
                                        paste(missing_attributes, sep="", collapse = ", "),
                                        " missing.", sep=""))
      }
    }
  }
  
  print(requirement_warnings)
}

## Test variable names function ########
# input: a list object of the compiled datasets using create_object_list()
test_variables <- function(datasets, database_structure_doc = "scripts/3_post_processing/tables/input_files/converting_v1p2_to_v2.csv") {
  
  missing_variables <- c()
  
  database_structure <- readr::read_csv(database_structure_doc, col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$heriarchy)
  
  for (i in 1:length(datasets)) {
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$heriarchy == FALSE) {
      # Warn user they have not supplied a valid table categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    # isolate the attributes in the database that employ controlled vars 
    all_variables <- database_structure %>%
      filter(data_type == "factor",
             heriarchy == category) %>%
      separate_rows(values, sep="; ") %>%
      separate(values, into = c("variable", "variable_definition"), sep = " = ")
    
    # determine which columns in the current dataset contain controlled vars
    factors_present <- names(datasets[[i]])[names(datasets[[i]]) %in% unique(all_variables$attribute_name)]
    
    if (length(factors_present) > 0) {
      for (j in 1:length(factors_present)) {
        check_this_attribute <- factors_present[j] 
        check_these_variables <- unique(
          datasets[[i]] %>% select(check_this_attribute) %>% 
            filter(complete.cases(.))
        )
        target_list <- all_variables %>% filter(attribute_name == check_this_attribute)
        if (any(! check_these_variables[[1]] %in% target_list$variable)) {
          doesnt_match <- check_these_variables[[! check_these_variables %in% target_list$variable]]
          error_text <- paste(check_this_attribute, doesnt_match, sep=": ")
          missing_variables <- c(missing_variables,
                                 error_text)
        }
      } 
    }
  }
  if (length(missing_variables) == 0) {
    print("All variables match guidence.")
  } else {
    variable_error_statement <- paste("These variables don't match approved vocab: ",
                                      paste(missing_variables, collapse = ", ", sep=""),
                                      ".", sep="")
    print(variable_error_statement)
  }
  
}