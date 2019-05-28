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
directory <- "./data/primary_studies/"

# Create empty vectors for each table
depthseries <- c()
cores <- c()
sites <- c()
species <- c()
impacts <- c()
materials_and_methods <- c()
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
        # Second check the table type and add to the correct list of studies 
        if(grepl("depthseries", table_type)){
          depthseries <- append(depthseries, fileName)
          
        } else if (grepl("cores", table_type)) {
          cores <- append(cores, fileName)
          
        } else if (grepl("sites", table_type)) {
          sites <- append(sites, fileName)
          
        } else if (grepl("species", table_type)) {
          species <- append(species, fileName)
          
        } else if (grepl("impacts", table_type)) {
          impacts <- append(impacts, fileName)
          
        } else if (grepl("materials_and_methods", table_type)) {
          materials_and_methods <- append(materials_and_methods, fileName)
          
        } else if (grepl("study_citations", table_type)) {
          study_citations <- append(study_citations, fileName)
          
        } else if (grepl("biomass", table_type)) {
          biomass <- append(biomass, fileName)
          
        }

      }
      
    }
  }
}

## 3. Import the CCRCN database structure and initative empty data frames #######

database_structure <- read_csv("./docs/ccrcn_database_structure.csv", col_types = cols())

depthseries_attributes <- filter(database_structure, table == "depthseries")$attribute
cores_attributes <- filter(database_structure, table == "core_level")$attribute
sites_attributes <- filter(database_structure, table == "site_level")$attribute
species_attributes <- filter(database_structure, table == "species")$attribute
impacts_attributes <- filter(database_structure, table == "impact")$attribute
materials_and_methods_attributes <- filter(database_structure, table == "methods_and_materials")$attribute
#study_citations <- c()
#biomass <- c()

# Import and join each file for a given table 
for(study in depthseries){
  read.csv()
}

# ccrcn_cores <- 
# ccrcn_sites <- 
# ccrcn_species <- 
# ccrcn_impacts <- 
# ccrcn_materials_and_methods <- 
# ccrcn_study_citations <-
# biomass 