# Script automates the creation of the CCRCN synthesis
# Scans data/primary_studies directory and reads in curated data
# Contact: lonnemanM@si.edu
#          klingesD@si.edu

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

directory <- "./data/primary_studies/"

# Create empty vectors for each table
depthseries <- c()
cores <- c()
sites <- c()
species <- c()
impacts <- c()
methods <- c()
study_citations <- c()
biomass <- c()

# Search through each study folder in the directory
for(folderName in dir(directory)){
  derivative_directory <- paste0(directory,folderName,"/derivative")
  # If the derivative folder exists and has one or more files, the following will not return 0
  if(length(derivative_directory) != 0) {
    for(fileName in dir(derivative_directory)) {
      # Extract the table type and file extension from each file 
      table_type <- strsplit(fileName, split = "\\d\\d\\d\\d")[[1]][2]
      
      # First check if the file extension is csv
      if(grepl(".csv", table_type)){
        # Create the full file path 
        file_path <- paste0(derivative_directory,"/",fileName)
        
        # Second check the table type and add to the corresponding list of studies 
        if(grepl("depthseries", table_type) & !grepl("biomass", table_type)){
          depthseries <- append(depthseries, file_path)
          
        } else if (grepl("cores", table_type)) {
          cores <- append(cores, file_path)
          
        } else if (grepl("sites", table_type)) {
          sites <- append(sites, file_path)
          
        } else if (grepl("species", table_type)) {
          species <- append(species, file_path)
          
        } else if (grepl("impacts", table_type)) {
          impacts <- append(impacts, file_path)
          
        } else if (grepl("materials_and_methods", table_type)) {
          methods <- append(methods, file_path)
          
        } else if (grepl("study_citations", table_type)) {
          study_citations <- append(study_citations, file_path)
          
        } else if (grepl("biomass", table_type)) {
          biomass <- append(biomass, file_path)
        
        # Need to add a method to track csv files that do not get added to table-list  
        } else {
          
        }
      # .bib files will the other main file type in derivative files 
      } else if(grepl(".bib", table_type)){
      
      # Need to track what non-.bib or -.csv files are in the derivative folder 
      } else {
        
      }
    }
  }
}

## 3. Import the CCRCN database structure and initative empty data frames #######

database_structure <- read_csv("./docs/ccrcn_database_structure.csv", col_types = cols())

depthseries_attributes <- filter(database_structure, table == "depthseries")$attribute
ccrcn_depthseries <- setNames(data.frame(matrix(ncol = length(depthseries_attributes), nrow = 0)), 
                              depthseries_attributes)

cores_attributes <- filter(database_structure, table == "core_level")$attribute
ccrcn_cores <- setNames(data.frame(matrix(ncol = length(cores_attributes), nrow = 0)), 
                        cores_attributes)

sites_attributes <- filter(database_structure, table == "site_level")$attribute
ccrcn_sites <- setNames(data.frame(matrix(ncol = length(sites_attributes), nrow = 0)), 
                        sites_attributes)

species_attributes <- filter(database_structure, table == "species")$attribute
ccrcn_species <- setNames(data.frame(matrix(ncol = length(species_attributes), nrow = 0)), 
                          species_attributes)

impacts_attributes <- filter(database_structure, table == "impact")$attribute
ccrcn_impacts <- setNames(data.frame(matrix(ncol = length(impacts_attributes), nrow = 0)), 
                          impacts_attributes)

methods_attributes <- filter(database_structure, table == "methods_and_materials")$attribute
ccrcn_methods <- setNames(data.frame(matrix(ncol = length(methods_attributes), nrow = 0)), 
                          methods_attributes)

#study_citations
#biomass

# Import and join each file for a given table 
for(study in depthseries){
  ccrcn_depthseries <- bind_rows(ccrcn_depthseries, read.csv(study))
}

for(study in cores){
  ccrcn_cores <- bind_rows(ccrcn_cores, read.csv(study))
}

for(study in species){
  ccrcn_species <- bind_rows(ccrcn_species, read.csv(study))
}

for(study in impacts){
  ccrcn_impacts <- bind_rows(ccrcn_impacts, read.csv(study))
}

for(study in methods){
  ccrcn_methods <- bind_rows(ccrcn_methods, read.csv(study))
}

for(i in 1:length(study_citations)){
  if(i==1){
    ccrcn_study_citations <- read.csv(study_citations[i])
  } else {
    ccrcn_study_citations <- bind_rows(ccrcn_study_citations, read.csv(study_citations[i]))
  }
}

