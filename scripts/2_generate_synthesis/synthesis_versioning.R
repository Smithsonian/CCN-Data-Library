# Coastal Carbon Research Coordination Network ####
# Database Versioning Script

# This versioning script performs the following:
# 1. synthesizes data curated under old database guidance 
# 2. updates the synthesis to V2 guidance
# 3. outputs the synthesis to a folder where it will be added to the grande-synthesis

## 1. Join Primary Studies from Database Version 1.2 ####


# Load necessary libraries
library(tidyverse)
library(lubridate)
# library(skimr)
# library(knitr)
# library(markdown)
# library(rmarkdown)
# library(DT)
# library(gridExtra)

# source scripts
source("./scripts/2_generate_synthesis/qa_synthesis_functions.R", local = T)


# join_status gets changed to FALSE if there is an error. 
# the synthesis continues but is deposited in another folder and the error is recorded
join_status <- TRUE

## ... Scan directory and assemble file paths to data ##########

# ARCHIVED WORKFLOW
# dataset_dirs <- list.dirs("data/primary_studies")
# final_dirs <- dataset_dirs[grepl("derivative", dataset_dirs)]
# write a csv of filepaths to ignore for the grande-synthesis
# write_csv(data.frame(file_paths = final_dirs), "docs/versioning/v1_data_paths.csv")
# ARCHIVED WORKFLOW

# read in list of file paths to data curated under old guidance
final_dirs <- read_csv("docs/versioning/v1_data_paths.csv") %>% pull(file_paths)

# Index of table names
tables <- c("sites", "cores", "depthseries", "species", "impacts", "methods", "study_citations")
# Other objects that we will need to track
trackers <- c(
  # CSV files that do not follow established naming conventions go here 
  "unknown_csv",
  # Non .csv files go here
  "unknown_filetypes")
# ignoring bib files for now

# Empty lists to fill with file paths
file_paths <- vector("list", length(c(tables, trackers)))
names(file_paths) <- c(tables, trackers)

for(folderName in final_dirs){
  # final_directory <- paste0(directory, folderName, "/final")
  
  # Case: if the final folder contains files
  if(length(dir(folderName)) != 0) {
    
    # extract and assign to file paths to the correct table type
    for(fileName in dir(folderName)) {
      # Create the full file path 
      file_path <- paste0(folderName, "/", fileName)
      
      # First check if the file extension is csv
      if(grepl(".csv", fileName)){
        
        # Second determine the table type and add to the appropriate category
        if(any(str_detect(fileName, tables))){
          # isolate the table type for categorization
          table_type <- tables[which(str_detect(fileName, tables))]
          # add the dataset file path to the corresponding list
          file_paths[[table_type]][length(file_paths[[table_type]]) + 1] <- file_path
          
          # Track csv files that do not get added to table-list
          # these are likely curated datasets that where not named in the standardized way
        } else {
          file_paths$unknown_csv[length(file_paths$unknown_csv) + 1] <- file_path
        }
        
        # Record what non-.csv files are in the final folder
      } else {
        file_paths$unknown_filetypes[length(file_paths$unknown_filetypes) + 1] <- file_path
        
      }
    }
  }
}

# stop and take a look at the files in each category
file_paths

## ... Import data and place into proper synthesis ##########

# Create list that will contain all synthesis data frames
data_synthesis <- vector("list", length(tables))
names(data_synthesis) <- tables

# Create object to save errors to 
synthesis_errors <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("error_message", "file_path"))

# Initiate empty data frames
for(i in seq_along(data_synthesis)){
  data_synthesis[[i]] <- data.frame()
}

# Create synthesis
# Convert all factors to characters to avoid multitude of warnings
for(i in seq_along(tables)){
  for(j in seq_along(file_paths[[tables[i]]])){
    # Use tryCatch to keep loop running if there's an error and record
    tryCatch(
      #data_synthesis[[i]] <- as.data.frame(read.csv(file_paths[[tables[i]]][j])) %>%
      data_synthesis[[i]] <- as.data.frame(read_csv(file_paths[[tables[i]]][j], col_types = cols(.default = "c"))) %>%
        mutate_if(is.factor, as.character) %>%
        bind_rows(data_synthesis[[i]]),
      # Record any errors
      error = function(e){
        synthesis_errors[nrow(synthesis_errors) + 1,] <<- c(unlist(e[1]), file_paths[[tables[i]]][j])
      }
    )
  }
}

# If an error was thrown, the synthesis failed
if(nrow(synthesis_errors) > 0) {
  join_status <- FALSE
}

## 2. Update to current database guidance ####

# guidance for data table updates
versioning_guidance <- read_csv("docs/versioning/converting_v1p2_to_v2.csv")
database_guidance <- read_csv("docs/ccrcn_database_structure.csv")

# load lookup tables for targeted fixes
# Species table fixes 
species_fixes <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv")

# Depth code fixes
core_depth_fixes <- read_csv("docs/versioning/studies_revisited.csv") %>% 
  select(study_id, core_length_flag) %>% 
  rename(core_length_flag_correct = core_length_flag)

## ... Update Synthesis Tables ####

# create function to loop through the column headers and rename them
renameColumns <- function(x, key) {
  
  for (i in 1:length(x)) {
    # store column name for potential renaming
    colname <- names(x)[i]
    
    # if the column name matches one in the key, rename it
    if (!is.na(match(colname, key$attribute_name_v1p2)) == TRUE) {
      #  replace the old column name with the new
      colnames(x)[colnames(x) == colname] <- key$attribute_name[match(colname, key$attribute_name_v1p2)]
    }
  }
  return(x)
}


# test out the renaming function
# x <- list_of_tables[[3]]
to_rename <- versioning_guidance %>% filter(action == "rename")
# result <- renameColumns(x = x, key = to_rename)
to_delete <- versioning_guidance %>% filter(action == "deletion" & heirarchy != "species_definitions")

# create list of tables that will become the updated synthesis
tables_to_update <- data_synthesis

# Iterate through list of tables
for (i in 1:length(tables_to_update)) {
  # Store table category
  category <- names(tables_to_update[i])
  
  # ... Table Fixes ####
  # Apply fixes that should occur to specific tables before column renaming and targeted deletions
  
  # Depthseries table fixes
  if(category == "depthseries"){
    print("Fixing depthseries table.")
    # Fix the two different 210Pb in the depth series
    depthseries_fixed <- tables_to_update[[i]] %>% 
      mutate_at(vars(contains("pb214_activity")), as.numeric) %>%
      mutate(pb214_activity = ifelse(is.na(pb214_activity),
                                     (pb214_activity_352keV + pb214_activity_295keV)/2,
                                     pb214_activity),
             # Assume errors are 100% correlated
             pb214_activity_se = ifelse(is.na(pb214_activity_se),
                                        (pb214_activity_se_352keV + pb214_activity_se_295keV)/2,
                                        pb214_activity_se)) %>%
      # these columns need to be coalesced 
      # assumes that coalesced columns won't have non-NA values in the same row
      # this should be vectorized so it's not manual
      mutate(age_se = coalesce(age_se, age_sd),
             cs137_activity_se = coalesce(cs137_activity_se, cs137_activity_sd),
             total_pb210_activity_se = coalesce(total_pb210_activity_se, total_pb210_activity_sd),
             ra226_activity_se = coalesce(ra226_activity_se, ra226_activity_sd),
             excess_pb210_activity_se = coalesce(excess_pb210_activity_se, excess_pb210_activity_sd),
             marker_date = coalesce(marker_date, marker_age),
             marker_date_se = coalesce(marker_date_sd, marker_age_sd),
             depth_interval_notes = coalesce(depth_interval_notes, dating_interval_notes),
             pb214_activity_se = coalesce(pb214_activity_se, pb214_activity_sd),
             bi214_activity_se = coalesce(bi214_activity_se, bi214_activity_sd)) %>%
      select(-c(age_sd, cs137_activity_sd, total_pb210_activity_sd, ra226_activity_sd,
                excess_pb210_activity_sd, marker_age, marker_date_sd, marker_age_sd,
                dating_interval_notes, pb214_activity_sd, bi214_activity_sd))
    # test
    length(unique(names(depthseries_fixed))) == length(depthseries_fixed)
    
    # store updated table
    tables_to_update[[i]] <- depthseries_fixed
  }

  # Core table fixes
  if(category == "cores"){
    print("Fixing cores table.")
    # Fix dates
    cores_fixed <- tables_to_update[[i]] %>% 
      # correct Breithaupt core date
      mutate(core_date = ifelse(study_id == "Breithaupt_et_al_2020", 
                                # transfer this to the hook script at some point
                                as.character(strptime(core_date, format = "%m/%d/%Y")), core_date)) %>%
      # separate out year month day
      mutate(year = year(ymd(core_date)), 
             month = month(ymd(core_date)),
             day = day(ymd(core_date))) %>%
      # resolve cases where core date is NA but core year month or day are provided
      mutate(year = ifelse(is.na(year) & !is.na(core_year), yes = core_year, no = year),
             month = ifelse(is.na(month) & !is.na(core_month), yes = core_month, no = month),
             day = ifelse(is.na(day) & !is.na(core_day), yes = core_day, no = day)) %>%
      select(-c(core_date, core_year, core_month, core_day)) %>% # get rid of cols used to generate split date cols
      rename(core_year = year) %>%
      # Fix depths
      left_join(core_depth_fixes) %>% # join with fixed core depths
      mutate(core_length_flag = ifelse(is.na(core_length_flag),
                                       core_length_flag_correct,
                                       core_length_flag)) %>% 
      select(-core_length_flag_correct) 
    # store updated table
    tables_to_update[[i]] <- cores_fixed
  }

  # Species table fixes
  if(category == "species"){
    print("Fixing species table.")
    # Fix species
    species_fixed <- tables_to_update[[i]] %>%
      left_join(species_fixes) %>% 
      mutate(species_code = ifelse(!is.na(recode_as), 
                                   recode_as, 
                                   species_code)) %>% 
      select(-c(recode_as, notes))
    # store updated table
    tables_to_update[[i]] <- species_fixed
  }  


  # ... Rename, Delete, and Reorder Columns ####
  
  # Rename columns
  # If any colnames are in the to_rename table, rename these columns 
  if(any(names(tables_to_update[[i]]) %in% to_rename$attribute_name_v1p2)){
    print(paste0("Renaming columns for ", category))
    # apply renaming function
    tables_to_update[[i]] <- renameColumns(x = tables_to_update[[i]], key = to_rename)
  }
  # this step will cause duplication of colnames if the new colnames already exist in the df
  
  # Delete (specified) columns
  # If any colnames are in the to_delete table, deselect these columns 
  if(any(names(tables_to_update[[i]]) %in% to_delete$attribute_name_v1p2)){
    print(paste0("Deleting targeted columns for ", category))
    tables_to_update[[i]] <- tables_to_update[[i]] %>% select(-any_of(to_delete$attribute_name_v1p2))
  }
  
  # table_guidance <- filter(database_guidance, table == category)
  # guidance_subset_actions <- filter(guidance_subset, !is.na(action))
  #
  # # If the guidance involves a rename, rename it
  # if (nrow(guidance_subset_actions) > 0) {
  #   for (j in 1:nrow(guidance_subset_actions)) {
  #     if (guidance_subset_actions$action[j] == "rename") {
  #       
  #       # Old names
  #       if (guidance_subset_actions$attribute_name_v1p2[j] %in% names(tables_to_update[[i]])) {
  #         # isolate the old column 
  #         old_column <- tables_to_update[[i]][,guidance_subset_actions$attribute_name_v1p2[j]]
  #         # [[1]] not sure what this was for
  #         
  #         # For any not NA entries in the old category, 
  #         tables_to_update[[i]][!is.na(old_column), guidance_subset_actions$attribute_name[j]] <- 
  #           old_column[!is.na(old_column)] # write them over to a column with a new category
  #       }
  #     }
  #   }
  # }
  # 
  # # Classes to include
  # include_these <- unique(guidance_subset$attribute_name[guidance_subset$attribute_name %in% names(tables_to_update[[i]])])
  # 
  # # Remove attributes not in guidance 2
  # # ... and order columns as in guidance v2
  # renamed_and_reordered <- tables_to_update[[i]][, include_these]
  # 
  # tables_to_update[[i]] <- renamed_and_reordered
} 

# reorder columns according to guidance
final_synthesis <- reorderColumns(tables = tables, ccrcn_synthesis = tables_to_update)


## ... QA/QC ##########

# create empty table for QA results
qa_results <- tibble(test = NA_character_,
                     result = NA_character_,
                     .rows=0)

# run synthesis QA tests
testAttributeNames(tables, final_synthesis)
testVariableNames(tables, final_synthesis)
testUniqueCores(final_synthesis$cores)
testCoreRelationships(final_synthesis)
# testUniqueCoordinates(data_synthesis) # replace with check for duplicate cores

# Provide summary statistics of numeric variables 
qa_numeric_results <- testNumericVariables(final_synthesis$depthseries)
# some warnings generated in this step because of data type

# Record summary of warnings 
# warning_summary <- summary(warnings())
# warning_summary

# save synthesized tables to objects to investigate
# methods <- final_synthesis$methods
#   sites <- final_synthesis$sites
view_cores <- final_synthesis$cores
view_depthseries <- final_synthesis$depthseries
#   impacts <- final_synthesis$impacts
#   species <-final_synthesis$species
#   study_citations <- final_synthesis$study_citations

# Messerschmidt_and_Kirwan_2020 have pb214_activity at different KeV's

## 3. Write Updated CCRCN Synthesis ###########

output_dir <- "data/primary_studies/CCRCN_synthesis_2021/derivative/"

# write updated tables
for (j in 1:length(final_synthesis)) {
  write_csv(final_synthesis[[j]], paste0(output_dir, "CCRCN_", names(final_synthesis)[j], ".csv"))
}
