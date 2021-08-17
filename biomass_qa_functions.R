## CCN QA/QC scripts
# contact: Jaxine Wolfe, wolfejax@si.edu

## Test column names in table list ###########

# Make sure column names match CCRCN guidelines
testTableCols <- function(datasets) {
  # create a list of datasets from the provided table names
  # datasets <- mget(table_names, envir = .GlobalEnv)
  
  # load in database guidance
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$heirarchy)
  
  # controlled_list <- list()
  for (i in 1:length(datasets)) {
    # define the category to filter the database by
    category <- names(datasets[i])
    
    if(category %in% database_structure$heirarchy == FALSE) {
      # Warn user they have not supplied a valid heirarchy categories and provide the list 
      print(paste(category,"is not a valid categories. Please use one of these options:"))
      print(tables)
      return()
    }
    
    # Gather column names from dataset and pull out columns with variables that are defined in our database structure
    column_names <- names(datasets[[i]])
    valid_columns <- filter(database_structure, heirarchy == category)$attribute_name
    non_matching_columns <- subset(column_names, !(column_names %in% valid_columns))
    
    # logical statement returns results
    if(length(non_matching_columns)==0) {
      print("Looks good! All column names match CCRCN standards")
    } else {
      print(paste(c("Non-matching attributes in", category, ":", non_matching_columns), collapse=" "))
    }
  }
}



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


