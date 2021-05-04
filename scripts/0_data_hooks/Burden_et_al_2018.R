## CCRCN Data Library ########
## contact: Jaxine Wolfe, wolfejax@si.edu

## Hook script for Burden et al 2018
## Information about the dataset (i.e. title, authors, citation, DOI)

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read in data
raw_data_2010_2017 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2010_2017.csv")
raw_data_2011 <- read_csv("data/primary_studies/Burden_et_al_2018/original/Saltmarsh _Chronosequence_data_2011.csv")
# locations need to be extracted from the .rtf file

# read in database guidance for easy reference
guidance <- read_csv("docs/ccrcn_database_structure.csv")

## 1. Curation ####

id <- "Burden_et_al_2018"

## ... Methods ####

# curate materials and methods
methods <- raw_methods

## ... Core-Level ####

# curate core-level data
core <- raw_cores

## ... Core Depthseries ####

# curate depthseries-level data
depthseries <- raw_depthseries

# The following tables are optional:
## ... Sites ####
## ... Species ####
## ... Impacts ####

## 2. QAQC ####

table_names <- c("methods", "cores", "depthseries")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 3. Study Citations ####

# Use RefManageR package to pull DOI


## 4. Write files ####

# Adjust the filepaths to output to the correct derivative folder
write_csv(cores, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_cores.csv") 
write_csv(depthseries, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_depthseries.csv")
write_csv(methods, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_methods.csv")
# write_csv(sites, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_sites.csv")
# write_csv(species, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_species.csv")
# write_csv(impacts, "data/primary_studies/Author_et_al_####/derivative/Author_et_al_####_impacts.csv")


