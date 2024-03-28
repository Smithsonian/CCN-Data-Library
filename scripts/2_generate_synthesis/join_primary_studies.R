# Script automates the creation of the CCRCN synthesis
# Scans data/primary_studies directory and reads in curated data
# Contact: wolfejax@si.edu

# RUN SCRIPT WITH A CLEAN R SESSION #
# if you experience an error, restart Rstudio and try again # 

past_version_code <- "1.1.1"
new_version_code <- "1.2.0"

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

## Prepare workspace

# load necessary libraries 
library(tidyverse)
library(RefManageR)
library(skimr)
# library(knitr)
# library(markdown)
# library(rmarkdown)
library(DT)

# load synthesis functions into the global environment
source("./scripts/2_generate_synthesis/qa_synthesis_functions.R", local = T)

# join_status is changed to F if there is an error. 
# the synthesis continues but is deposited in another folder and the error is recorded
join_status <- TRUE

## 2. Scan directory and assemble study lists #########

# The data/primary_studies folder will be scanned 
# If a filename meets the necessary format (study_id_XXXX_table_type.csv), 
# it will be appended to the table-specific list

# designate directory to pull data from
# directory <- "./data/primary_studies/"

final_dirs <- list.dirs("data/primary_studies")[grepl("derivative|final", list.dirs("data/primary_studies"))]

# remove any directories that shouldn't be drawn into the synthesis
# exclude_dirs <- c("Curtis_et_al_2022", "Howard_and_Fourqurean_2020")
# 
# if(!is_empty(exclude_dirs)){
#   final_dirs <- final_dirs[!grepl(paste0(exclude_dirs, collapse = "|"), final_dirs)]
# }

# Index of table names
tables <- c("depthseries", "cores", "sites", "species", "impacts", "methods", "study_citations")
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

# organize filenames into their designated list
for(folderName in final_dirs){
  
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
        # Record what non-.bib or -.csv files are in the derivative folder
      } else if (grepl(".bib", fileName)){
        file_paths$bibs[length(file_paths$bibs) + 1] <- file_path
        
        # Record what non-.csv files are in the final folder
      } else {
        file_paths$unknown_filetypes[length(file_paths$unknown_filetypes) + 1] <- file_path
      }
    }
  }
}
# stop and take a look at the files in each category
# file_paths

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
bib_file <- ccrcn_synthesis$study_citations %>%
  select(-c(study_id, publication_type, keywords, 
            # issn, `article-number`,
            abstract)) %>%
  distinct_all() %>% drop_na(bibliography_id) %>% distinct_all()

bib_file_redundancy_checks <- bib_file %>% 
  group_by(bibliography_id) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

if (first(bib_file_redundancy_checks$n)>1) {
  stop("There are duplicate bibliography_id's")
}

bib_file <- bib_file %>% 
  column_to_rownames("bibliography_id")

# find and fix encoding issues
# https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
# mutate_at(vars(names(citations)), function(x){iconv(x, "latin1", "UTF-8",sub='')}) %>% 

## 4. QA/QC #############

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

# Provide summary statistics of numeric variables 
qa_numeric_results <- testNumericVariables()

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

## 5. Synthesis Post-processing ###########
if(join_status == TRUE){
  
  # run post-processing on the cores table
  source("scripts/3_post_processing/6_database_citation.R")
  source("scripts/3_post_processing/1_assign_geography.R")
  source("scripts/3_post_processing/2_assign_habitat.R")
  source("scripts/3_post_processing/3_assign_data_tiers.R")
  source("scripts/3_post_processing/4_max_depths.R")
  source("scripts/3_post_processing/5_core_attributes.R")
  source("scripts/3_post_processing/7_resolve_taxonomy.R")
}

## 6. Synthesis Metrics & Change Log ####

# dev branch synthesis will be compared with the version of the synthesis on the main branch
# synthesis_log <- readr::read_csv("docs/synthesis_resources/synthesis_log.csv")
# 
# synth_diff <- anti_join(ccrcn_synthesis$cores %>%
#                           select(study_id, site_id, core_id),
#                         synthesis_log) %>%
#   mutate(version = new_version_code,
#          date = format(Sys.time(), "%Y-%m-%d")) %>%
#   select(date, version, everything())
# 
# # stash results in additive list, documenting version, and date
# synthesis_log_filemame <- paste0("docs/synthesis_resources/synthesis_", 
#                                  str_replace_all(new_version_code, "\\.", "_"), 
#                                  ".csv") 
# write_csv(synth_diff, synthesis_log_filemame)

## 7. Write RMarkdown report #########

# Record summary of warnings 
warning_summary <- summary(warnings())

# read in current guidance
database_structure <- read_csv("docs/ccrcn_database_structure.csv")

# get date to paste to file name
url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")
  
# Needs updating
# REQUIRES: file paths, join status, warning summary, etc
# map cores w habitat?
# save qa_results
rmarkdown::render(input = "./scripts/2_generate_synthesis/synthesis_report.Rmd",
                  # output_format = "html_document",
                  output_file = paste0("synthesis_report_", url_date, ".html"),
                  output_dir = "./docs/synthesis_reports")

# rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "file_paths", "warning_summary", "join_status"))])

## 8. Write new synthesis ###########

if(join_status == TRUE){
  
  # Write new synthesis data
  # to derivative folder (eventually, get rid of the derivative folder, just write to CCRCN_synthesis)
  write_csv(ccrcn_synthesis$cores, "./data/CCN_synthesis/CCN_cores.csv")
  write_csv(ccrcn_synthesis$depthseries, "./data/CCN_synthesis/CCN_depthseries.csv")
  write_csv(ccrcn_synthesis$sites, "./data/CCN_synthesis/CCN_sites.csv")
  write_csv(ccrcn_synthesis$impacts, "./data/CCN_synthesis/CCN_impacts.csv")
  write_csv(ccrcn_synthesis$methods, "./data/CCN_synthesis/CCN_methods.csv")
  write_csv(ccrcn_synthesis$species, "./data/CCN_synthesis/CCN_species.csv")
  write_excel_csv(ccrcn_synthesis$study_citations %>% select(-keywords, -abstract), 
            "./data/CCN_synthesis/CCN_study_citations.csv")
  
  WriteBib(as.BibEntry(bib_file), "data/CCN_synthesis/CCN_bibliography.bib") # some encoding funny business here
}
