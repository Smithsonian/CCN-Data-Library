## CCRCN Data Library QA/QC scripts
# contact: Jaxine Wolfe <wolfejax@si.edu>

## Check core_id uniqueness ########
# All cores in synthesis should have unique id

testUniqueCores <- function(data) {
  # First, get a list of all unique core_ids
  core_list <- data %>%
    group_by(core_id) %>%
    summarize(n = n()) %>%
    filter(n > 1)
  
  if(length(core_list$core_id) > 0){
    # warning("Check the following core_ids in the core-level data:")
    return(core_list)
  } else {
    # print("All core IDs are unique.")
    return("Passed")
  }
  # return(core_list)
}

## Check lat/long uniqueness ########
# Cores in synthesis should likely have unique lat/long values

testUniqueCoords <- function(data) {
  # First, get a list of all unique core_ids
  core_list <- data %>%
    mutate(lat_long = paste(latitude, longitude, sep=",")) %>%
    group_by(lat_long) %>%
    summarize(n = n(), 
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1)

  if(length(core_list$core_ids) > 0){
    # warning("Some cores in the core-level data have duplicate coordinates. Check 'data/QA/duplicate_cores.csv' for the list.")
    return(core_list)
  } else {
    # print("All core coordinates are unique.")
    return("Passed")
  }
}

## Ensure fractions are not percentages #################

# This function reviews all attributes that ought to be a fraction and determines
#   whether they are in their proper format (e.g. less than 1)
fractionNotPercent <- function(df) {
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # define attributes with fractional values in the guidance
  fraction_cols <- database_structure %>% filter(data_type == "numeric") %>% 
    select(attribute_name, format_unit_codes) %>% 
    filter(grepl("fraction", attribute_name)) %>%
    distinct() %>% pull(attribute_name)
  
  # subset the dataframe 
  match_cols <- df[,which(names(df) %in% fraction_cols)] %>% 
    # make sure all cols are numeric type
    mutate_all(as.numeric)
  
  # check if there are any fraction-containing columns in the data
  if(plyr::empty(match_cols)) {
    # print("No fraction columns present in the dataset, safe to continue.")
    return("No fraction columns")
    
    # stop function and prevent an error message from displaying
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
    
    # check for non-numeric data types
  } else if (any(match_cols > 1, na.rm = T)){
    non_fraction <- match_cols %>%
      filter_all(any_vars(. > 1))
    
    return(paste(colnames(non_fraction), collapse = ', '))
    # stop(paste0("At least one of the attributes intended to be expressed as a
    #             fraction is expressed as a percent (values > 1). Please review all
    #             fraction columns: ", paste(colnames(non_fraction), collapse = ', ')))
    stop()
  } else {
    # print("All fractions are expressed as fractions, safe to continue.")
    return("Passed")
  }
} 


## Test numeric columns for a given data table (UPDATED)
testNumericCols <- function(df) {

  numeric_attributes <- read_csv("./docs/ccrcn_database_structure.csv", col_types = cols()) %>%
    filter(data_type == "numeric") %>% 
    rename(unit = format_unit_codes) %>% 
    distinct(attribute_name, unit) %>% 
    mutate(unit = recode(unit, "becquerelsPerKilogram" = "units vary")) # dating units will vary
  
  # select only numeric columns 
  to_check <- names(df)[names(df) %in% unique(numeric_attributes$attribute_name)]
  testing_data <- df[, to_check]
  
  # library(skimr)
  
  # list of functions to run on numeric attributes
  funs <- skimr::sfl(
    min = function(x) min(x, na.rm=TRUE), 
    max = function(x) max(x, na.rm=TRUE), 
    median = function(x) median(x, na.rm=TRUE),
    na_count = function(x) sum(is.na(x)),
    # NaN_count = function(x) sum(is.nan(x)),
    p0 = NULL, 
    p25 = NULL, 
    p50 = NULL, 
    p75 = NULL, 
    p100 = NULL,
    hist = NULL
  )
  
  # Set skimr to run with our custom list of functions
  my_skim <- skimr::skim_with(numeric = funs, append = TRUE)
  
  # run skim and format results
  results <- testing_data %>% my_skim() %>% 
    rename(attribute_name = skim_variable,
           missing_values = n_missing, median = numeric.median,
           min = numeric.min, max = numeric.max, mean = numeric.mean) %>% 
    left_join(numeric_attributes) %>% 
    select(attribute_name, unit, min, max, mean, median, missing_values, complete_rate)
  
  return(results)
}

# Check the data types of all variables 
# The function attempts to convert all attributes that should be factors but are character type
# Other type conversions are not conducted since unexpected results could occur. Users are encouraged to 
# conduct all other data type conversions elsewhere in the hook script and check for any underlying 
# issues in the data that could be leading toward an incorrect data type classification (commas in numeric columns, etc.)
testDataTypes <- function(df) {
  require(lubridate)
  
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
reorderColumns <- function(table_type, df) {
  
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  if(table_type %in% database_structure$table == FALSE) {
    # Warn user they have not supplied a valid table table_type and provide the list 
    warning(paste(table_type,"is not a valid table Please use one of the above listed options."))
    print(tables)
    return()
  }
  
  # Get controlled attributes for the current table
  table_structure <- database_structure %>%
    filter(table == table_type) %>%
    filter(attribute_name %in% colnames(df))
  
  # Controlled attributes selected first, then all approved uncontrolled attributes
  df %>% select(table_structure$attribute_name, everything())
  
}

## Test column names in tables ####

# Make sure column names match CCRCN guidelines
testTableCols <- function(table_names) {
  # create a list of datasets from the provided table names
  datasets <- mget(table_names, envir = .GlobalEnv)
  
  # load guidance
  database_structure <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())
  
  # Create a vector of all the table names
  tables <- unique(database_structure$table)
  
  results <- data.frame()
  
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
    
    if(length(non_matching_columns)==0) {
      # print("Looks good! All column names match CCRCN standards")
      results <- bind_rows(results, data.frame(table = category, result = "Passed"))
    } else {
      # print(paste(c("Non-matching attributes in", category, ":", non_matching_columns), collapse=" "))
      results <- bind_rows(results, data.frame(table = category, result = paste0("Uncontrolled attributes: ", non_matching_columns)))
    }
  }
  return(results)
}

## Test variables in tables ########

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
  # If there are no invalid variables indicated test was passed
  # Otherwise pass along the table of uncontrolled vars
  if(nrow(invalid_df) == 0) {
    # print("Looks good! All variable names match CCRCN standards")
    return("Passed")
  } else {
    # print("View resulting invalid variable names:")
    return(invalid_df)
  }
}

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
                                       result = paste0("Missing: ", missing_cols))
      
      # compile into missing required table
      # output table should have cols: table, missing_attributes
      missing_required <- bind_rows(missing_required, missing_from_study)
    } else{
      missing_required <- bind_rows(missing_required, data.frame(table = table_name,
                                                                 result = "Passed"))
    }
  }
  return(missing_required)
  # if(!is_empty(missing_required)){
  #   return(missing_required)
  # } else {
  #   print("All required attributes are present!")
  # }
}

## Check for conditionally required attributes ####

## Need a new QA function that iterates through the guidance, makes sure that every required attribute,
# and any conditional are there. (repurposed from the SWG, written by James Holmquist)
testConditional <- function(table_names, database_structure_doc = "docs/ccrcn_database_structure.csv") {
  # original guidance in the SWG
  # biomass/agb_biomass_inundation/docs/CCN Biomass and Elevation Data Synthesis Guidence Draft 200925.csv
  
  database_structure <- readr::read_csv(database_structure_doc, col_types = cols())
  datasets <- mget(table_names, envir = .GlobalEnv)
  
  # requirement_warnings <- c()
  results <- data.frame()
  
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
    
    # conditional attributes
    conditional_attributes <- database_structure %>%
      dplyr::filter(table == category,
                    required == "conditional",
                    ! is.na(conditional_on)) %>% 
      separate_rows(conditional_on, sep="; ") %>% 
      filter(conditional_on %in% names(datasets[[i]]))
    
    if (all(conditional_attributes$attribute_name %in% names(datasets[[i]]))) {
      # requirement_warnings <- c(requirement_warnings, 
      #                           paste(category, " (conditional): all conditional attributes present.", sep=""))
      results <- bind_rows(results, data.frame(table = category, result = "Passed"))
    } else {
      missing_attributes <- unique(conditional_attributes$attribute_name[! (conditional_attributes$attribute_name %in% names(datasets[[i]]))])
      # requirement_warnings <- c(requirement_warnings, 
      #                           paste(category, " (conditional): ",
      #                                 paste(missing_attributes, sep="", collapse = ", "),
      #                                 " missing.", sep=""))
      results <- bind_rows(results, data.frame(table = category, result = paste0("Missing: ", missing_attributes)))
    }
  }
  return(results)
}

## Create data visualization report ####
writeDataVizReport <- function(study_id){
  
  # generate data contributor report
  rmarkdown::render(input = "./scripts/1_data_formatting/data_visualization_report.Rmd",
                    # output_format = "html_document",
                    output_file = paste0(study_id, "_dataviz_report"),
                    output_dir = "./docs/dataviz_reports/")
}

## Create QA Report ####

writeQualityReport <- function(study_id){
  rmarkdown::render(input = "./scripts/1_data_formatting/qaqc_report.Rmd",
                    # output_format = "html_document",
                    output_file = paste0(study_id, "_qa_report"),
                    output_dir = "./docs/qa_reports/")
}

## Check for misspelled taxa ####

## function to resolve taxa names using GNR 
# uses taxonomic authorities to resolve spelling rather than recoding everything by hand
testTaxa <- function(table_names) {
  
  if("species" %in% table_names){
    # create unique list of species codes to save time
    taxa <- unique(sort(species$species_code))
    
    resolved <- data.frame()
    unresolved <- vector()
    
    for (i in 1:length(taxa)){
      # gnr_sources <- taxize::gnr_datasources()
      
      # store resolved results
      gnr_result <- taxize::gnr_resolve(sci = as.vector(taxa[i]), 
                                        preferred_data_sources = c(150, 9, 4, 3), # default NULL = no preference
                                        canonical = TRUE) %>%
        # gnr_datasources()
        # preferred_data_sources = c(150, 9, 4, 3)
        
        slice(1) # pick the first result
      
      if (!plyr::empty(gnr_result)) {
        # compile list of resolved taxa
        resolved <- rbind(resolved, gnr_result)
        
      } else {
        # save unresolved taxa
        unresolved <- rbind(unresolved, taxa[i])
        # skip unresolved taxa
        i <- i + 1
        next
      }
    }
    # report any unresolved taxa
    if (length(unresolved) > 0) {
      print("The following taxa could not be resolved:")
      print(unresolved)
    }
    # report potentially misspelled taxa
    if(length(which(resolved$user_supplied_name != resolved$matched_name2)) > 0){
      misspelled <- resolved$user_supplied_name[resolved$user_supplied_name != resolved$matched_name2]
      
      print("Check the spelling of the following species: ")
      print(misspelled)
    } else {
      print("Everything looks good!")
    }
    # in the event that no species table is present in the supplied table names
  } else {
    print("No species table present.")
  }
}

## Check core/site relationships ####

# Combine core and site ID testing functions
# specify which ID value to test
testIDs <- function(table1, table2, by) {
  # perform anti joins for the specified tables and id types
  switch(by,
         "core" = {
           # this will return any site IDs that do not have a match in the two tables
           results <- (anti_join(table1, table2, by="core_id"))$core_id
           results2 <- (anti_join(table2, table1, by="core_id"))$core_id
         },
         "site" = {
           # this will return any site IDs that do not have a match in the two tables
           results <- (anti_join(table1, table2, by="site_id"))$site_id
           results2 <- (anti_join(table2, table1, by="site_id"))$site_id
         }
  )
  
  # return results
  if(length(results) == 0 & length(results2) == 0) {
    print("Passed! All IDs match.")
  }
  if(length(results) > 0){
    print("The following IDs are absent from the second table:")
    print(results)
  }
  if(length(results2) > 0){
    print("The following IDs are absent from the first table:")
    print(results2)
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

#### ARCHIVED FUNCTIONS ####

## Check for core_id relationships
# All entries in core_data files should have relationships to relevant depth series and biomass datasets

test_core_relationships <- function(core_data, depth_data) {
  warning("This function has been superseded, please use testIDs() instead.")
  
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

## Test numeric columns
test_numeric_vars <- function(input_data) {
  
  warning("This function has been superseded, please use testNumericCols() instead.")
  
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

## Test column names function
# Make sure column names match CCRCN guidelines
test_colnames <- function(category, dataset) {
  
  warning("This function has been superseded, please use testTableCols() instead.")
  
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

## Test variable names
test_varnames <- function(input_data) {
  warning("This function has been superseded, please use testTableVars() instead.")
  
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

## Select only controlled attributes, re-order according to database structure
select_and_reorder_columns <- function(datalevel_table, # A string corresponding
                                       # to the name of the data level table
                                       data, # The dataset you seek to re-order 
                                       uncontrolled_file_path # File path to where
                                       # the uncontrolled attributes should be saved
) {
  
  warning("This function has been superseded, please use reorderColumns() instead.")
  
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

fraction_not_percent <- function(dataset) {
  
  warning("This function has been superseded, please use fractionNotPercent() instead.")
  
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

## Check core_id uniqueness
# All cores in synthesis should have unique id

test_unique_cores <- function(data) {
  warning("This function has been superseded, please use testUniqueCores() instead.")
  
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

## Check lat/long uniqueness
# Cores in synthesis should likely have unique lat/long values

test_unique_coords <- function(data) {
  warning("This function has been superseded, please use testUniqueCoords() instead.")
  
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
