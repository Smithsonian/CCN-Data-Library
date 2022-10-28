## CCRCN Data Library
## Jaxine Wolfe <wolfejax@si.edu>

## script to join all the information in Marot et al 2020

library(tidyverse)
library(readxl)

## Merge Data Tables ####

study_dir <- "data/primary_studies/Marot_et_al_2020/original"

# function to read and merge  CSV files of a specified data type
readAndMerge <- function(study_dir, keyword, skip){
  
  all_files <- list.files(study_dir, pattern = ".csv", recursive = T, full.names = T)
  # xlsx files are more complicated because multiple sheets
  # the CSVs look to have all the information compiled
  
  selected_files <- all_files[grepl(keyword, all_files)] # select the files by keyword provided
  
  result <- data.frame() # df to store results
  
  # loop through files
  for(k in selected_files){
    # read in and process temperary table
    temptable <- read_csv(k, skip = skip, 
                          na = c("--", "ND", "", "N/A")) %>% 
      mutate(across(everything(), as.character)) %>% 
      select_if(function(x) {!all(is.na(x))})
    
    # iteratively bind results together
    result <- bind_rows(temptable, result)
  }
  return(result)
}

## apply function to data tables

soil_properties <- readAndMerge(study_dir, keyword = "SedimentPhysicalProperties", skip = 1)

alpha_spec <- readAndMerge(study_dir, keyword = "AlphaSpectroscopy", skip = 1) %>% 
  filter_all(any_vars(!is.na(.)))

gamma_spec <- readAndMerge(study_dir, keyword = "GammaSpectroscopy", skip = 1) %>% 
  filter_all(any_vars(!is.na(.)))

# lots of fun variations in the depth col
# unique(gamma_spec$`Depth\n (cm)`)
# dates will need recoding (ex. "2-Jan" = "1-2")

# one of the radiocarbon CSVs is not comma separated
# theres only two tables, lets read in the xls files for this one

# site information has annoying headers, you'd need to skip either 1 or 2 rows
# most of the information is provided in more detail in the field logs so 
# lets just merge this if necessary


## Merge Field Logs ####

# select field log files we want from all xls files
all_xls <- list.files(study_dir, pattern = ".xlsx", recursive = T, full.names = T)
log_files <- all_xls[grepl("FieldLogs|Field_Logs", all_xls)] 

result <- data.frame() # df to store merged table product

for(file in log_files){
  
  tempsheets <- excel_sheets(file) # store sheet names
  tempsheets <- tempsheets[!grepl("READ ME", tempsheets)] # discard readme sheets
  
  # loop through sheets
  for(s in tempsheets){
    log_xls <- read_xlsx(file, sheet = s, col_names = F, na = c("N/A", "NA", ""))
    
    # data wrangling
    templog <- bind_rows(log_xls[1:2], log_xls[3:4],
                         by = c("...1" = "...3", "...2" = "...4")) %>% 
      mutate(site_id = log_xls$...2[1]) %>% 
      slice(-1) %>% drop_na("...2")
    
    # paste rows together
    result <- bind_rows(templog, result) %>% 
      filter(!(...1 == "...3"))
  }
}

# widen result table from long format
wide_result <- result %>% 
  pivot_wider(id_cols = site_id, names_from = ...1, values_from = ...2) %>% 
  mutate(across(where(is.list), sapply, toString))


## Table Clean Up ####



