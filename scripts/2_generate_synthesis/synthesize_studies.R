# Coastal Carbon Research Coordination Network ####
# Database Versioning Script

# This script synthesizes all data that has been aligned to the current guidance. Includes:
  # - synthesized and updated data from hook scripts that was curated under the old guidance
  # - data from hook scripts that curate using current guidance

## 1. Join All Studies ####

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(DT)
# library(skimr)
# library(knitr)
# library(markdown)
# library(rmarkdown)
# library(gridExtra)

# join_status gets changed to FALSE if there is an error. 
# the synthesis continues but is deposited in another folder and the error is recorded
join_status <- TRUE

# Set this to true if you would like to compare the current synthesis to the archived synthesis
log_changes <- FALSE

## ... Scan directory and assemble file paths to data ##########

dataset_dirs <- list.dirs("data/primary_studies")
all_dirs <- dataset_dirs[grepl("derivative", dataset_dirs)]

# read in list of file paths to data curated under old guidance
v1_dirs <- read_csv("docs/versioning/v1_data_paths.csv") %>% pull(file_paths)

# leave out all the v1 dirs because they were already synthesized and output to CCRCN_2021_synthesis
final_dirs <- all_dirs[!(all_dirs %in% v1_dirs)]

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

# Write new synthesis data if there were no errors
if(join_status == TRUE){

  write_csv(data_synthesis$cores, "./data/CCRCN_synthesis/original/CCRCN_cores.csv")
  write_csv(data_synthesis$depthseries, "./data/CCRCN_synthesis/original/CCRCN_depthseries.csv")
  write_csv(data_synthesis$sites, "./data/CCRCN_synthesis/original/CCRCN_sites.csv")
  write_csv(data_synthesis$impacts, "./data/CCRCN_synthesis/original/CCRCN_impacts.csv")
  write_csv(data_synthesis$methods, "./data/CCRCN_synthesis/original/CCRCN_methods.csv")
  write_csv(data_synthesis$species, "./data/CCRCN_synthesis/original/CCRCN_species.csv")
  write_csv(data_synthesis$study_citations, "./data/CCRCN_synthesis/original/CCRCN_study_citations.csv")
}

## ... QA/QC ##########

# These QAQC functions need to be updated/modified for the new guidance

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


## 7. Write RMarkdown report #########
# get date to paste to file name
url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")

rmarkdown::render(input = "./scripts/2_generate_synthesis/synthesis_report.Rmd",
                  output_format = "html_document",
                  output_file = paste0("synthesis_report_", url_date, ".html"),
                  output_dir = "./docs/synthesis_reports")
