# Utility functions for processing CIFOR data ingestion
# contact: Jaxine Wolfe <wolfejax@si.edu>

## Read Excel Workbook ####

# provide path to excel work book and this function will return a list
# where each list element is one sheet from the workbook
# Note: this fxn currently assumes that the first row of each sheet will be the column headers
readExcelWorkbook <- function(path){
  workbook_sheets <- readxl::excel_sheets(path)
  
  result_list <- list()
  
  for(sheet in workbook_sheets){
    result_list[[sheet]] <- read_xlsx(path, sheet = sheet, guess_max = 10000)
  }
  return(result_list)
}

# result_list[[1]] will return the data table in the first sheet

## Get Table Names from List ####

# used in concert with the output from readExcelWorkbook()
# harvest table and attribute names from each sheet
# provide function with a list object and it will extract the names of each table
# the output table can be used to create a lookup table which aligns the output with CCN data structure
getTableNamesFromList <- function(x){
  result_df <- data.frame()
  
  for(i in 1:length(x)){
    df <- data.frame(table = names(x[i]),
                     attribute_name = names(x[[i]]))
    
    result_df <- bind_rows(result_df, df)
  }
  return(result_df)
}


## synthCIFOR ####
## AKA. the "Alternative" Data Structure
# custom function to synthesize CIFOR data which is formatted 
# in tables with a "Data" or "Tree Data" sheet

# provide this function with a specification of data type and it will synthesize
# all the corresponding files
synthCIFOR <- function(data_type){
  # create a list of all files in the CIFOR original folder
  cifor_files <- list.files("data/primary_studies/CIFOR/original", 
                            pattern = ".xlsx", recursive = T, full.names = T)
  cifor_files <- cifor_files[!grepl("~", cifor_files)] # remove any cached files from the list
  
  # filter these files based on the data type provided
  switch(data_type,
         "soil" = {synth_files <- cifor_files[grepl("soil|Soil", basename(cifor_files))]},
         "vegetation" = {synth_files <- cifor_files[grepl("veg|Veg|Trees",  basename(cifor_files))]},
         "necromass" = {synth_files <- cifor_files[grepl("debris|Debris", basename(cifor_files))]}
  )
  
  # create empty data frame to store results
  result <- data.frame()
  skipped <- c()
  
  # loop through files, reading and combining into one table
  for(file in synth_files){
    
    # only read the file if it has a "Data" or "Tree Data" sheet
    if(any(c("Data", "Tree Data") %in% readxl::excel_sheets(file))){
      
      temp_tbl <- read_xlsx(file, sheet = 2, guess_max = 3000) %>% 
        # add a column with the name of the file, removing the .xlsx
        mutate(filename = gsub(".xlsx", "", basename(file))) %>% 
        # convert all cols to character so they bind
        mutate(across(everything(), as.character)) %>% 
        select(filename, everything())
      
      # add to the final table
      result <- bind_rows(result, temp_tbl)
      
    } else{
      # document skipped files
      skipped <- c(skipped, file)
    }
  }
  print("The following files were skipped:")
  print(skipped)
  
  return(result)
}

# examples
# cifor_soil <- synthCIFOR(data_type = "soil")
# cifor_veg <- synthCIFOR(data_type = "vegetation")
# necromass <- synthCIFOR(data_type = "necromass") # none right now

# unique(cifor_veg$filename)

# # archived script to separate attribute names into separate lookup tables
# # SWAMP
# data_str <- read_csv("data/primary_studies/CIFOR/CIFOR_docs/cifor_data_structure.csv") %>%
#   filter(table != "Data")
# 
# write_csv(data_str, "data/primary_studies/CIFOR/CIFOR_docs/swamp_data_structure.csv")
# 
# # ALTERNATIVE
# veg_str <- data.frame(data_type = "vegetation",
#                       table = "Data",
#                       attribute_name = names(cifor_veg))
# soil_str <- data.frame(data_type = "soil",
#                       table = "Data",
#                       attribute_name = names(cifor_soil))
# 
# data_str_all <- bind_rows(soil_str, veg_str) %>% select(-table)
# write_csv(data_str_all, "data/primary_studies/CIFOR/CIFOR_docs/alt_data_structure.csv")

# Plots

# ggplot(cifor_soil, aes(as.numeric(`Bulk density (g/cm3)`), as.numeric(`Carbon content (%)`))) + geom_point()

# Maps

# library(leaflet)
# 
# cifor_soil %>% 
#   mutate(Latitude = as.numeric(Latitude), 
#          Longitude = as.numeric(Longitude)) %>% 
# leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(radius = 2, label = ~filename)
# 
# cifor_veg %>% 
#   mutate(Latitude = as.numeric(gsub("S00 ", "", Latitude)), 
#          Longitude = as.numeric(gsub("W047 |W046 ", "", Longitude))) %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(radius = 2, label = ~filename)
# # some Brazil cores showing up in Russia...probly missing a negative sign


## synthCIFOR ####
# custom function to synthesize CIFOR data which is formatted 
# in the SWAMP format

# provide this function with a specification of data type and it will synthesize
# all the corresponding files into one big result list (allegedly)

synthSWAMP <- function(data_type){
  # create a list of all files in the CIFOR original folder
  cifor_files <- list.files("data/primary_studies/CIFOR/original", 
                            pattern = ".xlsx", recursive = T, full.names = T)
  cifor_files <- cifor_files[!grepl("~", cifor_files)] # remove any cached files from the list
  
  # filter these files based on the data type provided
  switch(data_type,
         "soil" = {synth_files <- cifor_files[grepl("soil|Soil", basename(cifor_files))]},
         "vegetation" = {synth_files <- cifor_files[grepl("veg|Veg|Trees",  basename(cifor_files))]},
         "necromass" = {synth_files <- cifor_files[grepl("debris|Debris", basename(cifor_files))]}
  )
  
  # create empty list to store results
  list_names <- "Site" # placeholder element name, this will be added to
  result <- sapply(list_names, function(x) NULL)
  
  # loop through files, reading and combining into one table
  for(file in synth_files){

    # only read the file if it has a "Data" or "Tree Data" sheet
    if(any(c("Site", "Plot") %in% readxl::excel_sheets(file))){
      
      templist <- readExcelWorkbook(file)
      
      # store filename and insert as a column in every list element data frame
      filename <- gsub(".xlsx", "", basename(file))
      templist <- mapply(cbind, templist, "filename" = filename, SIMPLIFY = F)
      
      # combine list dataframes based on element names
      keys <- unique(c(names(result), names(templist)))
      # add to result list
      result <- lapply(setNames(keys, keys), function(x) {bind_rows(result[[x]], templist[[x]])})
    } 
  }
  return(result)
}

# example
# swamp_veg <- synthSWAMP(data_type = "vegetation")


