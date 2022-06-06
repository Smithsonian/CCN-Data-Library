# Script automates the creation of the CCRCN synthesis
# Scans data/primary_studies directory and reads in curated data
# Contact: lonnemanM@si.edu

# RUN SCRIPT WITH A CLEAN R SESSION #
# if you experience an error, restart Rstudio and try again # 

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

## 2. Scan directory and assemble study lists #########
# The data/primary_studies folder will be scanned 
# If a filename meets the necessary format (study_id_XXXX_table_type.csv), 
# it will be appended to the table-specific list
library(tidyverse)
library(RefManageR)
library(skimr)
# library(knitr)
# library(markdown)
# library(rmarkdown)
library(DT)

# join_status is changed to F if there is an error. 
# the synthesis continues but is deposited in another folder and the error is recorded
join_status <- TRUE

# designate directory to pull data from
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
      #ccrcn_synthesis[[i]] <- as.data.frame(read.csv(file_paths[[tables[i]]][j])) %>%
      ccrcn_synthesis[[i]] <- as.data.frame(read_csv(file_paths[[tables[i]]][j], col_types = cols(.default = "c"))) %>%
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

# Prepare .bib file
bib_file <- ccrcn_synthesis$studycitations %>%
  select(-study_id, -publication_type) %>%
  distinct() %>%
  column_to_rownames("bibliography_id")

## 4.Read in previous synthesis & compare #################

# List will hold the previous synthesis
archived_synthesis <- vector("list", length(tables))
names(archived_synthesis) <- tables

synthesis_directory <- "./data/CCRCN_synthesis/original/"

# Get file names of previous synthesis
archived_filepaths <- dir("./data/CCRCN_synthesis/archive/")

# Read in data 
for(file in archived_filepaths){
  
  # If it's a .csv file: 
  if(grepl(".csv", file)){
    
    # Extract table type from file name
    table_type <- file %>%
      gsub("_", "", .) %>%
      gsub("CCRCN", "", .) %>%
      gsub(".csv", "", .)
    
    archived_synthesis[[table_type]] <- read_csv(paste0("./data/CCRCN_synthesis/archive/", file),
                                                 col_types = cols(.default = "c")) # %>%
      # type_convert(na = "NA")
  }
}

# The forward change log list tracks which values are new to the synthesis
change_log_df <- vector("list", length(tables))
names(change_log_df) <- tables

# Create object to save errors to 
change_log_errors <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("error_message", "change_type", "table"))

for(table in tables){
  # Skip sites for now - geography assignment script needs to be modified
  if(table == "sites"){next()}
  
  forward <- NULL
  backward <- NULL
  
  # Use tryCatch to keep loop running if there's an error and record
  tryCatch(
    # Get "forward" changes - New data to the synthesis
    forward <- setdiff(ccrcn_synthesis[[table]], archived_synthesis[[table]]) %>%
      mutate(change_type = "forward"), 
    # Record any errors
    error = function(e){
      change_log_errors[nrow(change_log_errors) + 1,] <<- c(unlist(e[1]), "forward", table)
    }
  )
  tryCatch(
    # Get "backward" changes - Data that's been removed from the synthesis
    backward <- setdiff(archived_synthesis[[table]], ccrcn_synthesis[[table]]) %>%
      mutate(change_type = "backward"),
    # Record any errors
    error = function(e){
      change_log_errors[nrow(change_log_errors) + 1,] <<- c(unlist(e[1]), "backward", table)
    }
  )
  
  # If there's an error, no results will exist for that table
  if(!is.null(forward)){
    if(is.null(backward)){
      change_log_df[[table]] <- forward 
    } else change_log_df[[table]] <- bind_rows(forward, backward)
  } else if(is.null(forward) & !is.null(backward)){
    change_log_df[[table]] <- backward
  }
  
}

# 
# forward <- setdiff(ccrcn_synthesis[["depthseries"]], archived_synthesis[["depthseries"]]) %>%
#   mutate(change_type = "forward")


## Format change log results 
#change_log_results <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("table", "change_types", "study_id"))
change_log_results <- tibble(table = NA_character_,
                             change_types = NA_character_,
                             study_id = NA_character_,
                             .rows=0)

for(table_type in names(change_log_df)){
  if(!is.null(change_log_df[[table_type]])){
    change_summary <- change_log_df[[table_type]] %>%
      group_by(study_id) %>%
      summarize(change_types = paste(unique(change_type), collapse = ", "), table = table_type) %>%
      select(table, study_id, change_types)

    change_log_results <- change_log_results %>%
      bind_rows(change_log_results, change_summary)
  }
}

## 5. QA/QC #############
source("./scripts/2_generate_synthesis/qa_synthesis_functions.R", local = T)

# Reorder columns 
ccrcn_synthesis <- reorderColumns(tables, ccrcn_synthesis)

qa_results <- tibble(test = NA_character_,
                     result = NA_character_,
                     .rows=0)

testUniqueCores(ccrcn_synthesis$cores)
testCoreRelationships(ccrcn_synthesis)
testUniqueCoordinates(ccrcn_synthesis$cores)
testAttributeNames(tables, ccrcn_synthesis)
testVariableNames(tables, ccrcn_synthesis)

# qa_results <- 
#   # Ensure each core ID is unique
#   # Currently the map atlas requires this 
#   # Eventually we'll just need a unique study ID - core ID combination
#   testUniqueCores(ccrcn_synthesis$cores) %>%
#   # Test relationships between core_ids at core- and depthseries-levels
#   # the test returns all core-level rows that did not have a match in the depth series data
#   bind_rows(testCoreRelationships(ccrcn_synthesis)) %>%
#   # Test latitude and longitude uniqueness
#   bind_rows(testUniqueCoordinates(ccrcn_synthesis$cores)) %>%
#   # Test column names to make sure they are in database structure
#   # Or are approved uncontrolled attributes 
#   bind_rows(testAttributeNames(tables, ccrcn_synthesis)) %>%
#   # Test variable names to make sure they are in database structure
#   bind_rows(testVariableNames(tables, ccrcn_synthesis)) 

# Provide summary statistics of numeric variables 
qa_numeric_results <- testNumericVariables(ccrcn_synthesis$depthseries)

## 6. Write new synthesis ###########
if(join_status == TRUE){
  
  # Archive previous synthesis
  # write_csv(archived_synthesis$cores, "./data/CCRCN_synthesis/archive/archived_synthesis_cores.csv")
  # write_csv(archived_synthesis$depthseries, "./data/CCRCN_synthesis/archive/archived_synthesis_depthseries.csv")
  # write_csv(archived_synthesis$sites, "./data/CCRCN_synthesis/archive/archived_synthesis_sites.csv")
  # write_csv(archived_synthesis$impacts, "./data/CCRCN_synthesis/archive/archived_synthesis_impacts.csv")
  # write_csv(archived_synthesis$methods, "./data/CCRCN_synthesis/archive/archived_synthesis_methods.csv")
  # write_csv(archived_synthesis$species, "./data/CCRCN_synthesis/archive/archived_synthesis_species.csv")
  # write_csv(archived_synthesis$studycitations, "./data/CCRCN_synthesis/archive/archived_synthesis_study_citations.csv")
  # Copy the previous .bib file
  # file.copy("data/CCRCN_synthesis/CCRCN_bibliography.bib", "data/CCRCN_synthesis/archive")
  
  # Write new synthesis data
  # to original folder
  write_csv(ccrcn_synthesis$cores, "./data/CCRCN_synthesis/original/CCRCN_cores.csv")
  write_csv(ccrcn_synthesis$depthseries, "./data/CCRCN_synthesis/original/CCRCN_depthseries.csv")
  write_csv(ccrcn_synthesis$sites, "./data/CCRCN_synthesis/original/CCRCN_sites.csv")
  write_csv(ccrcn_synthesis$impacts, "./data/CCRCN_synthesis/original/CCRCN_impacts.csv")
  write_csv(ccrcn_synthesis$methods, "./data/CCRCN_synthesis/original/CCRCN_methods.csv")
  write_csv(ccrcn_synthesis$species, "./data/CCRCN_synthesis/original/CCRCN_species.csv")
  write_csv(ccrcn_synthesis$studycitations, "./data/CCRCN_synthesis/original/CCRCN_study_citations.csv")
  
  WriteBib(as.BibEntry(bib_file), "data/CCRCN_synthesis/original/CCRCN_bibliography.bib")
  
  # # to derivative folder for post-processing (this will be served through the Atlas)
  # write_csv(ccrcn_synthesis$cores, "./data/CCRCN_synthesis/derivative/CCRCN_cores.csv")
  # write_csv(ccrcn_synthesis$depthseries, "./data/CCRCN_synthesis/derivative/CCRCN_depthseries.csv")
  # write_csv(ccrcn_synthesis$sites, "./data/CCRCN_synthesis/derivative/CCRCN_sites.csv")
  # write_csv(ccrcn_synthesis$impacts, "./data/CCRCN_synthesis/derivative/CCRCN_impacts.csv")
  # write_csv(ccrcn_synthesis$methods, "./data/CCRCN_synthesis/derivative/CCRCN_methods.csv")
  # write_csv(ccrcn_synthesis$species, "./data/CCRCN_synthesis/derivative/CCRCN_species.csv")
  # write_csv(ccrcn_synthesis$studycitations, "./data/CCRCN_synthesis/derivative/CCRCN_study_citations.csv")
  # 
  # WriteBib(as.BibEntry(bib_file), "data/CCRCN_synthesis/derivative/CCRCN_bibliography.bib")
}

# Record summary of warnings 
warning_summary <- summary(warnings())

## 7. Write RMarkdown report #########
# get date to paste to file name
url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")
  
rmarkdown::render(input = "./scripts/2_generate_synthesis/synthesis_report.Rmd",
                  output_format = "html_document",
                  output_file = paste0("synthesis_report_", url_date, ".html"),
                  output_dir = "./docs/synthesis_reports")
