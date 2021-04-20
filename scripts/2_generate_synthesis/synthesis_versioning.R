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
tables <- c("depthseries", "cores", "sites", "species", "impacts", "methods", "study_citations")
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

# save synthesized tables to objects 
if(join_status == TRUE){
  
  methods <- data_synthesis$methods
  sites <- data_synthesis$sites
  cores <- data_synthesis$cores
  depthseries <- data_synthesis$depthseries
  impacts <- data_synthesis$impacts
  species <-data_synthesis$species
  study_citations <- data_synthesis$study_citations
}

## ... QA/QC ##########

source("./scripts/2_generate_synthesis/qa_synthesis_functions.R", local = T)

# Reorder columns 
data_synthesis <- reorderColumns(tables, data_synthesis)

# create empty table for QA results
qa_results <- tibble(test = NA_character_,
                     result = NA_character_,
                     .rows=0)

# run synthesis QA tests
testAttributeNames(tables, data_synthesis)
testVariableNames(tables, data_synthesis)
testUniqueCores(data_synthesis$cores)
testCoreRelationships(data_synthesis)
# testUniqueCoordinates(data_synthesis)

# Provide summary statistics of numeric variables 
qa_numeric_results <- testNumericVariables(data_synthesis$depthseries)
# some warnings generated in this step because of data type

# Record summary of warnings 
warning_summary <- summary(warnings())
warning_summary

## 2. Update to current database guidance ####

# source("scripts/3_post_processing/scripts/qa_functions.R")

# guidance for data table updates
guidance <- read_csv("docs/versioning/converting_v1p2_to_v2.csv")

# Species table fixes 
species_fixes <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv")

# Depth code fixes
core_depth_fixes <- read_csv("docs/versioning/studies_revisited.csv") %>% 
  select(study_id, core_length_flag) %>% 
  rename(core_length_flag_correct = core_length_flag)

## ... Prepare Tables for Update ####

# Fix the two different 210Pb in the depth series
depthseries_fixed <- depthseries %>% 
  mutate_at(vars(contains("pb214_activity")), as.numeric) %>%
  mutate(pb214_activity = ifelse(is.na(pb214_activity),
                                 (pb214_activity_352keV + pb214_activity_295keV)/2,
                                 pb214_activity),
         # Assume errors are 100% correlated
         pb214_activity_se = ifelse(is.na(pb214_activity_se),
                                    (pb214_activity_se_352keV + pb214_activity_se_295keV)/2,
                                    pb214_activity_se))

# Fix dates
cores_fixed <- cores %>% 
  # correct Breithaupt core date
  mutate(core_date = ifelse(study_id == "Breithaupt_et_al_2020", 
                            as.character(strptime(core_date, format = "%m/%d/%Y")), core_date)) %>%
  # separate out year month day
  mutate(year = year(ymd(core_date)), 
         month = month(ymd(core_date)),
         day = day(ymd(core_date))) %>%
  # resolve cases where core date is NA but core year month or day are provided
  mutate(year = ifelse(is.na(year) & !is.na(core_year), yes = core_year, no = year),
         month = ifelse(is.na(month) & !is.na(core_month), yes = core_month, no = month),
         day = ifelse(is.na(day) & !is.na(core_day), yes = core_day, no = day)) %>%
  select(-core_date) %>% # get rid of date
  # Fix depths
  left_join(core_depth_fixes) %>% # join with fixed core depths
  mutate(core_length_flag = ifelse(is.na(core_length_flag),
                                   core_length_flag_correct,
                                   core_length_flag)) %>% 
  select(-core_length_flag_correct) 
# left_join(habitat2)

# Fix species
species_fixed <- species %>%
  left_join(species_fixes) %>% 
  mutate(species_code = ifelse(!is.na(recode_as), 
                               recode_as, 
                               species_code)) %>% 
  select(-c(recode_as, notes))


## ... Update Synthesis Tables ####

# Rename attributes from guidance v1.2 to 2
list_of_tables <- list(methods = methods,
                       cores = cores_fixed,
                       depthseries = depthseries_fixed,
                       species = species_fixed)

# Iterate through list of tables
for (i in 1:length(list_of_tables)) {
  # Subset guidance by name of the table 
  category <- names(list_of_tables[i])
  
  guidance_subset <- filter(guidance, heirarchy == category)
  guidance_subset_actions <- filter(guidance_subset, !is.na(action))
  
  # If the guidance involves a rename, rename it
  if (nrow(guidance_subset_actions) > 0) {
    for (j in 1:nrow(guidance_subset_actions)) {
      if (guidance_subset_actions$action[j] == "rename") {
        
        # Old names
        if (guidance_subset_actions$attribute_name_v1p2[j] %in% names(list_of_tables[[i]])) {
          old_column <- list_of_tables[[i]][,guidance_subset_actions$attribute_name_v1p2[j]][[1]]
          
          # For any not NA entries in the old category, 
          list_of_tables[[i]][!is.na(old_column),guidance_subset_actions$attribute_name[j]] <- 
            old_column[!is.na(old_column)] # write them over to a column with a new category
        }
      }
    }
  }
  
  # Classes to include
  include_these <- unique(guidance_subset$attribute_name[guidance_subset$attribute_name %in% names(list_of_tables[[i]])])
  
  # Remove attributes not in guidance 2
  # ... and order columns as in guidance v2
  renamed_and_reordered <- list_of_tables[[i]][,include_these]
  
  list_of_tables[[i]] <- renamed_and_reordered
} 

## 3. Write Updated CCRCN Synthesis ###########

output_dir <- "data/primary_studies/CCRCN_synthesis_2021/derivative/"

# write the tables that were updated
for (i in 1:length(list_of_tables)) {
  write_csv(list_of_tables[[i]], paste0(output_dir, "CCRCN_", names(list_of_tables)[i], ".csv"))
}

# write the tables that weren't updated
write_csv(sites, paste0(output_dir, "CCRCN_sites.csv"))
write_csv(impacts, paste0(output_dir, "CCRCN_impacts.csv"))
write_csv(study_citations, paste0(output_dir, "CCRCN_study_citations.csv"))

