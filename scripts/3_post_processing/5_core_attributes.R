# Coastal Carbon Research Coordination Network
# Database Guidance V2
# Synthesis Post-Processing Script

# Contact: Jaxine Wolfe, wolfejax@si.edu

# The code iterates through the CCN database, which has already been converted to V2 guidance
# It looks at the data available for each core and extracts the attributes names for which data is not entirely missing.
# The final table will have the following columns: study_id, core_id, table, attribute_name

# library(tidyverse)

# read in necessary data
cores <- ccrcn_synthesis$cores
depthseries <- ccrcn_synthesis$depthseries

## Step 1: Get unique ids for study, site, and core ####

if(!(file.exists("docs/core_attributes.csv"))){
  # this runs the script for all cores in the synthesis
  # takes a few minutes
  study_site_core <- cores %>% 
    select(study_id, site_id, core_id)
  
  update_existing_table <- FALSE
  
} else {

  # read in current version of the core attributes table
  current_core_attributes <-  read_csv("docs/core_attributes.csv")
  
  # filter core table for new core ids
  # Get unique ids for study, site, and core
  study_site_core <- cores %>% 
    filter(!(core_id %in% current_core_attributes$core_id)) %>%
    select(study_id, site_id, core_id) %>% 
    filter(study_id != "Copertino_unpublished")
  
  update_existing_table <- TRUE
}

## Step 2: Extract core attributes at the core and depthseries level ####

if(!(plyr::empty(study_site_core))){
  
  # create a table to store the result
  all_core_attributes <- data.frame()
  absent_cores <- data.frame()
  
  # Iterate through study/site/core ID's
  for (i in 1:nrow(study_site_core)) {
    
    # Store the study and core id
    temp_study_id <- study_site_core$study_id[i]
    # temp_site_id <- study_site_core$site_id[i] # unneccessary
    temp_core_id <- study_site_core$core_id[i]
    
    # Extract attribute names one core at a time for the core and depthseries tables
    temp_core <- cores %>% filter(study_id == temp_study_id & core_id == temp_core_id) %>%
      select_if(function(x) {!all(is.na(x))}) %>% # Remove columns that are entirely NA
      mutate_all(as.character) %>%
      # gather attribute names into one column
      pivot_longer(-c(study_id, core_id), names_to = "attribute_name", values_to = "data") %>%
      mutate(table = "core_level") %>%
      select(-data)
    
    if(!(temp_core_id %in% depthseries$core_id)){
      
      # aggregate the attributes for the cores table
      all_core_attributes <- bind_rows(all_core_attributes, temp_core) %>% distinct()
      
      # document cases where a core id is not in the depthseries table
      # Currently, 11 Peck_et_al_2020 cores are not in the depthseries table
      absent_cores <- data.frame(study_id = temp_study_id, core_id = temp_core_id) %>% 
        bind_rows(absent_cores)
      
      print(paste0("Core is not in depthseries table: ", temp_core_id))
      
    } else {
      
      temp_depthseries <- depthseries %>% 
        filter(study_id == temp_study_id & core_id == temp_core_id) %>%
        select_if(function(x) {!all(is.na(x))}) %>% # Remove columns that are entirely NA
        mutate_all(as.character) %>%
        # gather attribute names into one column
        pivot_longer(-c(study_id, core_id), names_to = "attribute_name", values_to = "data") %>%
        mutate(table = "depthseries") %>%
        select(-data)
      
      # create a df with unique attributes for each core 
      core_attributes <- bind_rows(temp_core, temp_depthseries) %>% distinct()
      
      # aggregate the attributes for the cores and depthseries tables
      all_core_attributes <- bind_rows(all_core_attributes, core_attributes)
      
    }
    
  }
  
  if(update_existing_table == TRUE){
    # add new cores to the existant core attribute table
    all_core_attributes <- bind_rows(current_core_attributes, all_core_attributes)
  }
  
  # write data to CCRCN_V2 folder
  write_csv(all_core_attributes, "docs/core_attributes.csv")
  
} else {
  
  print("There are no new cores to process.")
}

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
