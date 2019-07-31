# Script automates the creation of the CCRCN synthesis
# Scans data/primary_studies directory and reads in curated data
# Contact: lonnemanM@si.edu

## 1. Synthesis background and description ###############
# The CCRCN clearinghouse synthesis combines data and metadata for each curated study
# The synthesis is made up of the following linked tables 
# 1. Depthseries: Raw, unaggregated soil data from across a soil core's profile 
# 2. Core-level metadata, biomass data, and/or aggregated data from the depthseries
# 3. Site-level metadata
# 4. Species table: organized by core- and/or site-level 
# 5. Impacts table: organized by core- and/or site-level 
# 6. Materials and methods metadata
# 7. Study citation links - Each study is associated with one or more citations 

# join_status is changed to F if there is an error. 
# the synthesis continues but is deposited in another folder and the error is recorded
join_status <- TRUE

## 2. Scan directory and assemble study lists #########
# The data/primary_studies folder will be scanned 
# If a filename meets the necessary format (study_id_XXXX_table_type.csv), 
# it will be appended to the table-specific list
library(tidyverse)
library(skimr)

directory <- "./data/primary_studies/"
# Index of table names
tables <- c("depthseries", "cores", "sites", "species", "impacts", "methods", "studycitations")
# Other objects that we will need to track
trackers <- c(
            # .bib file paths stored in a list 
            "bibs",
            # CSV files that do not follow established naming conventions go here 
            "unknown_csv",
            # Non .bib or .csv files go here
            "unknown_filetypes")

# Empty lists to fill with file paths
file_paths <- vector("list", length(c(tables, trackers)))
names(file_paths) <- c(tables, trackers)

for(folderName in dir(directory)){
  derivative_directory <- paste0(directory,folderName,"/derivative")
  # If the derivative folder exists and has one or more files, the following will not return 0
  if(length(derivative_directory) != 0) {
    for(fileName in dir(derivative_directory)) {
      # Extract the table type and file extension from each file
      file_type <- strsplit(fileName, split = "\\d\\d\\d\\d")[[1]][2]
      
      # Create the full file path 
      file_path <- paste0(derivative_directory,"/",fileName)

      # First check if the file extension is csv
      if(grepl(".csv", file_type)){
        
        # Remove csv and underscore from file type to get table type
        table_type <- gsub("_", "", gsub(".csv", "", file_type))
        
        # Second check the table type and add to the corresponding list of studies 
        if(table_type %in% tables){
          file_paths[[table_type]][length(file_paths[[table_type]]) + 1] <- file_path
          
          # Need to add a method to track csv files that do not get added to table-list  
        } else {
          file_paths$unknown_csv[length(file_paths$unknown_csv) + 1] <- file_path
        }
        # .bib files will the other main file type in derivative files 
      } else if(grepl(".bib", file_type)){
        file_paths$bibs[length(file_paths$bibs) + 1] <- file_path
        
        # Record what non-.bib or -.csv files are in the derivative folder 
      } else {
        file_paths$unknown_filetypes[length(file_paths$unknown_filetypes) + 1] <- file_path
        
      }
    }
  }
}

## 3. Import data and place into proper synthesis ###############
# Create list that will contain all synthesis data frames
ccrcn_synthesis <- vector("list", length(tables))
names(ccrcn_synthesis) <- tables

# Create object to save errors to 
synthesis_errors <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("error_message", "file_path"))

# Initiate empty data frames
for(i in seq_along(ccrcn_synthesis)){
  ccrcn_synthesis[[i]] <- data.frame()
}

# Create synthesis
# Convert all factors to characters to avoid multitude of warnings
for(i in seq_along(tables)){
  for(j in seq_along(file_paths[[tables[i]]])){
    # Use tryCatch to keep loop running if there's an error and record
    tryCatch(
      ccrcn_synthesis[[i]] <- read_csv(file_paths[[tables[i]]][j]) %>%
        mutate_if(is.factor, as.character) %>%
        bind_rows(ccrcn_synthesis[[i]]),
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

## 4.Read in previous synthesis & compare #################

# List will hold the previous synthesis
archived_synthesis <- vector("list", length(tables))
names(archived_synthesis) <- tables

synthesis_directory <- "./data/CCRCN_synthesis_test/"

# Get file names of previous synthesis
archived_filepaths <- dir(synthesis_directory)

# Read in data 
for(file in archived_filepaths){
  
  # If it's a .csv file: 
  if(grepl(".csv", file)){
    
    # Extract table type from file name
    table_type <- file %>%
      gsub("_", "", .) %>%
      gsub("CCRCN", "", .) %>%
      gsub(".csv", "", .)
    
    archived_synthesis[[table_type]] <- read.csv(paste0(synthesis_directory, file)) 
  }
}

# The forward change log list tracks which values are new to the synthesis
change_log_df <- vector("list", length(tables))
names(change_log_df) <- tables

# These are the keys that make each row in a given table unique
table_keys <- list(
  list("core_id", "depth_min"),
  list("study_id", "core_id"),
  list("study_id", "site_id"),
  list("study_id", "site_id", "core_id"),
  list("study_id", "site_id", "core_id"),
  list("study_id"),
  list("study_id", "bibliography_id")
)
names(table_keys) <- tables

for(table in tables){
  # Skip sites for now - geography assignment script needs to be modified
  if(table == "sites"){next()}
  # Get "forward" changes - New data to the synthesis
  forward <- anti_join(ccrcn_synthesis[[table]], archived_synthesis[[table]], by=unlist(table_keys[[table]])) %>%
    mutate(change_type = "forward") 
  # Get "backward" changes - Data that's been removed from the synthesis
  backward <- anti_join(archived_synthesis[[table]], ccrcn_synthesis[[table]], by=unlist(table_keys[[table]])) %>%
    mutate(change_type = "backward") 
  
  change_log_df[[table]] <- bind_rows(forward, backward)
}

## QA/QC #############


## Write new synthesis ###########
if(join_status == TRUE){
  # Write new synthesis data
  
  # Backup previous synthesis data
}

## Write RMarkdown report #########

# Get summary file of warning messages
#summary(warnings())
