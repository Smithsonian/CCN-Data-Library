## CCN Data Library

# Synthesis Change Log
# Script to compare previous synthesis to current and catalog changes

library(tidyverse)

syth_path <- "https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/main/data/CCRCN_synthesis/"

# dev branch synthesis will be compared with the version of the synthesis on the main branch
old_cores <- readr::read_csv(paste0(syth_path, "CCRCN_cores.csv")) %>% 
  select(contains("_id")) %>% 
  mutate(version = "v1.0.0",
         date = "2023-01-12") %>% 
  select(date, version, everything())

write_csv(old_cores, "docs/synthesis_resources/synthesis_v1_0_0.csv")

# Old Script ######

# temporary script to compute and test change log

# archived_filepaths <- list.files("data/CCRCN_synthesis/archive", pattern = ".csv", full.names = T)

# # List will hold the previous synthesis
# archived_synthesis <- vector("list", length(tables))
# names(archived_synthesis) <- tables
# 
# # Read in data 
# for(file in archived_filepaths){
#   
#     # Extract table type from file name
#     table_type <- tables[which(str_detect(file, tables))]
#     
#     archived_synthesis[[table_type]] <- read_csv(file, col_types = cols(.default = "c"))
# }
# 
# # id_vars <- c('study_id', 'site_id', 'core_id', 'method_id')
# 
# # create reference tables for previous version of the synthesis
# prev_cores <- archived_synthesis$cores %>% distinct(study_id, site_id, core_id) %>%  
#   mutate(table = "cores") %>% select(table, everything())
# 
# prev_ds <- archived_synthesis$depthseries %>% distinct(study_id, site_id, core_id)  %>%  # add method_id 
#   mutate(table = "depthseries") %>% select(table, everything()) 
# 
# prev_methods <- archived_synthesis$methods %>% distinct(study_id) %>% # add method_id  
#   mutate(table = "methods") %>% select(table, everything()) 
# 
# prev_sites <- archived_synthesis$sites %>% distinct(study_id, site_id) %>% 
#   mutate(table = "sites") %>% select(table, everything())
# 
# prev_species <- archived_synthesis$species %>% distinct(study_id, site_id, core_id) %>%  
#   mutate(table = "species") %>% select(table, everything())
# 
# prev_impacts <- archived_synthesis$impacts %>% distinct(study_id, site_id, core_id) %>% 
#   mutate(table = "impacts") %>% select(table, everything())
# 
# prev_citations <- archived_synthesis$study_citations %>% distinct(study_id) %>%  
#   mutate(table = "study_citations") %>% select(table, everything())
# 
# # bind into one table
# prev_data <- bind_rows(prev_methods, prev_sites, prev_cores, prev_ds, prev_species, prev_impacts, prev_citations)
# 
# write_csv(prev_data, "docs/synthesis_resources/id_variable_archive.csv")

## Old Change Log ####

# # this needs to be reworked ***
# 
# # List will hold the previous synthesis
# archived_synthesis <- vector("list", length(tables))
# names(archived_synthesis) <- tables
# 
# synthesis_directory <- "./data/CCRCN_synthesis/original/"
# 
# # Get file names of previous synthesis
# archived_filepaths <- list.files("data/CCRCN_synthesis/archive", pattern = ".csv", full.names = T)
# # archived_filepaths <- dir("./data/CCRCN_synthesis/archive/")
# 
# # Read in data 
# for(file in archived_filepaths){
#   
#   # Extract table type from file name
#   table_type <- tables[which(str_detect(file, tables))]
#   
#   # create list of archived data
#   archived_synthesis[[table_type]] <- read_csv(file, col_types = cols(.default = "c"))
# }
# 
# # The forward change log list tracks which values are new to the synthesis
# change_log_df <- vector("list", length(tables))
# names(change_log_df) <- tables
# 
# # Create object to save errors to 
# change_log_errors <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("error_message", "change_type", "table"))
# 
# for(table in tables){
#   # Skip sites for now - geography assignment script needs to be modified
#   if(table == "sites"){next()}
#   
#   forward <- NULL
#   backward <- NULL
#   
#   # Use tryCatch to keep loop running if there's an error and record
#   tryCatch(
#     # Get "forward" changes - New data to the synthesis
#     forward <- setdiff(ccrcn_synthesis[[table]], archived_synthesis[[table]]) %>%
#       mutate(change_type = "forward"), 
#     # Record any errors
#     error = function(e){
#       change_log_errors[nrow(change_log_errors) + 1,] <<- c(unlist(e[1]), "forward", table)
#     }
#   )
#   tryCatch(
#     # Get "backward" changes - Data that's been removed from the synthesis
#     backward <- setdiff(archived_synthesis[[table]], ccrcn_synthesis[[table]]) %>%
#       mutate(change_type = "backward"),
#     # Record any errors
#     error = function(e){
#       change_log_errors[nrow(change_log_errors) + 1,] <<- c(unlist(e[1]), "backward", table)
#     }
#   )
#   
#   # If there's an error, no results will exist for that table
#   if(!is.null(forward)){
#     if(is.null(backward)){
#       change_log_df[[table]] <- forward 
#     } else change_log_df[[table]] <- bind_rows(forward, backward)
#   } else if(is.null(forward) & !is.null(backward)){
#     change_log_df[[table]] <- backward
#   }
#   
# }
# 
# # 
# # forward <- setdiff(ccrcn_synthesis[["depthseries"]], archived_synthesis[["depthseries"]]) %>%
# #   mutate(change_type = "forward")
# 
# ## Format change log results 
# #change_log_results <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("table", "change_types", "study_id"))
# change_log_results <- tibble(table = NA_character_,
#                              change_types = NA_character_,
#                              study_id = NA_character_,
#                              .rows=0)
# 
# for(table_type in names(change_log_df)){
#   if(!is.null(change_log_df[[table_type]])){
#     change_summary <- change_log_df[[table_type]] %>%
#       group_by(study_id) %>%
#       summarize(change_types = paste(unique(change_type), collapse = ", "), table = table_type) %>%
#       select(table, study_id, change_types)
# 
#     change_log_results <- change_log_results %>%
#       bind_rows(change_log_results, change_summary)
#   }
# }
