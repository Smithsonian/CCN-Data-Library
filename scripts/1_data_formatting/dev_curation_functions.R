# This script presents a prototype function to add all ontological changes 
# into a single script and master list of synonyms
# See: https://github.com/Smithsonian/CCRCN-Data-Library/issues/14

# Schile-Beers and Megonigal 2017 core-level data will be used as a test case
library(tidyverse)
library(readxl)
library(lubridate)

FILE_NAME <- "Megonigal_J_Patrick-20170103-Abu_Dhabi_Blue_Carbon_Project_Ecological_Applications.xlsx"
FILE_PATH <- paste0(getwd(), "/data/Schile-Beers_2017/original/" )
data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="plot information")
raw_depthseries_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="soil carbon data")

# The list of core variables in the QA function will be used to initilize the table 
# that controls vocabulary. 
core_var <- c(
  "study_id", "site_id", "core_id", 
  'core_date', "core_notes",
  "core_latitude", "core_longitude", "core_position_accuracy", "core_position_method", "core_position_notes", 
  "core_elevation", "core_elevation_datum", "core_elevation_accuracy", "core_elevation_method", "core_elevation_notes",
  "salinity_class", "salinity_method", "salinity_notes", 
  "vegetation_class", "vegetation_method", "vegetation_notes", 
  "inundation_class", "inundation_method", "inundation_notes", 
  "core_length_flag"
)

# This work flow as I understand it was pitched to me by @ktoddbrown at a dinner at AGU. I think we should try and implement it here since we are thinking we may need to transition a lot of our work eventually from full time coding specialists, to part time general technicians.
# 
# All attribute names and variable names should be stored in a big table that our hook scripts interact with, both adding synonyms for our controlled attribute and variable names names, asking the data entry person to confirm what the function thinks the matches are, then making the conversions to the standard vocabulary.
# 
# For example, imagine a function, matchCcOntologies(listOfNewDataTables, listOfRecognizedVocabulary) { ... code stuff ... }
# 
# listOfNewDataTables would be a list of the data tables we are bringing in to the synthesis.
# 
# listOfRecognizedVocabulary would be a list of all of our controlled attribute and varriable names, as well as synonyms we have encountered from previous inputs.
# 
# For example, listOfRecognizedVocabulary snippet could look like this ...
# controlled_ontology | synonym | firstSeenByUs
# ------------- | ------------- | -------------
# dry_bulk_density | bulk density | Fakeman_et_al_2019
# dry_bulk_density | dbd | Makeup_et_al_2016
# loss_on_ignition | loi | Holmquist_2018
# loss_on_ignition | loi550 | Fakeman_et_al_2019

## 1. Initialize table with controlled vocab ###########
headerTable <- as.data.frame(core_var)

headerTable <- headerTable %>%
  rename(controlled_ontology = core_var) %>%
  mutate(synonym = NA, 
         first_seen_by = "CCRCN_v1",
         controlled_ontology = as.character(controlled_ontology))

# Eventually this should be turned into a function
# "invalid_set" should remove all matching columns in the data frame 

## 2. Test column  names #######
study_id <- "Schile-Beers_and_Megonigal_2017"

# An empty vector that will populated with variables that should be dropped from the data
invalid_set <- c()

# A for loop will run through each column header
for(i in 1:length(colnames(data))){
  
  # get the name of the ith column
  col_name <- colnames(data[i])
  
  # run through each option
  if(col_name %in% headerTable$controlled_ontology) { # if it matches controlled vocab don't change 
    
  } else { 
    # if it doesn't match ask to add new synonym for controlled vocab 
    # or ensure that variable is defined in metadata
    
    # ask if the variable name is a synonym for a variable in the CCRCN guidance
    print(sprintf("Should '%s' be changed to match CCRCN controlled vocabulary?", col_name), quote=FALSE)
    # accept either "y" or "n"
    input <- readline(prompt = "Enter either y or n ")
    
    if(input == "y") {
      
      # Ask for the controlled vocab term
      variable_name <- readline(prompt = sprintf("What controlled vocab should '%s' be changed to? ", col_name))
      # Make sure it's actually a controlled term
      if(variable_name %in% headerTable$controlled_ontology) {
        # Add the new synonym - controlled variable pair to the master list, include the study ID
        headerTable <- bind_rows(headerTable, data.frame(controlled_ontology=variable_name, synonym=col_name, first_seen_by=study_id))
      } else { # warn the user that their input does not match the controlled ontology
        warning(sprintf("WARNING '%s' does not match existing controlled vocabulary", variable_name))
      }
      
      # If it is not a synonym, ask if the variable is defined in the study's metadata
    } else {
      print(sprintf("Is '%s' defined in the study's metadata?", col_name), quote=FALSE)
      input_metadata <- readline(prompt = "Enter either y or n ")
      
      # If it is, move on to the next column, if not warn the user that the column will be dropped
      if(input_metadata == "n") {
        warning(sprintf("WARNING '%s' will be dropped from the dataframe", col_name))
        invalid_set <- append(invalid_set, col_name)
      }
      
    }
    
  }
  
}

