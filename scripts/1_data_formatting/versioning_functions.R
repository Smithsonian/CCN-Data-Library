
# Probably merge this function with updateTables()
# create function to loop through the column headers and rename them
renameColumns <- function(x, key) {
  
  for (i in 1:length(x)) {
    # store column name for potential renaming
    colname <- names(x)[i]
    
    # if the column name matches one in the key, rename it
    if (!is.na(match(colname, key$attribute_name_v1p2)) == TRUE) {
      #  replace the old column name with the new
      colnames(x)[colnames(x) == colname] <- key$attribute_name[match(colname, key$attribute_name_v1p2)]
    }
  }
  return(x)
}

# # Synthesis exploration
# synthesis_cores <- read_csv("data/CCRCN_synthesis/original/CCRCN_cores.csv")
# synthesis_depthseries <- read_csv("data/CCRCN_synthesis/original/CCRCN_depthseries.csv", guess_max = 50000)
# 
# lead_studies <- synthesis_depthseries %>% select(study_id, contains("pb214_activity")) %>% 
#   drop_na(pb214_activity)
# unique(lead_studies$study_id)
# "Poppe_and_Rybczyk_2018"        "Poppe_and_Rybczyk_2019"        "Poppe_et_al_2019"             
# "Peck_et_al_2020"               "Messerschmidt_and_Kirwan_2020" "Giblin_and_Forbrich_2018"  

# CHANGE THE FOLLOWING CODE IN EACH HOOK SCRIPT ####

# # Depthseries table fixes
# if(category == "depthseries"){
#   print("Fixing depthseries table.")
#   
#   # Fix the two different 210Pb in the depth series
#   depthseries_fixed <- tables_to_update[[i]] %>% 
#     mutate_at(vars(contains("pb214_activity")), as.numeric) %>%
#     mutate(pb214_activity = ifelse(is.na(pb214_activity),
#                                    (pb214_activity_352keV + pb214_activity_295keV)/2,
#                                    pb214_activity),
#            # Assume errors are 100% correlated
#            pb214_activity_se = ifelse(is.na(pb214_activity_se),
#                                       (pb214_activity_se_352keV + pb214_activity_se_295keV)/2,
#                                       pb214_activity_se))
#   # test
#   length(unique(names(depthseries_fixed))) == length(depthseries_fixed)
#   
#   # store updated table
#   tables_to_update[[i]] <- depthseries_fixed
# }

# # CHANGE IN HOOK SCRIPTS INSTEAD?
# # Core table fixes
# if(category == "cores"){
#   print("Fixing cores table.")
#   
#   # Depth code fixes
#   core_depth_fixes <- read_csv("docs/versioning/studies_revisited.csv", col_types = cols()) %>% 
#     select(study_id, core_length_flag) %>% 
#     rename(core_length_flag_correct = core_length_flag) %>%
#     drop_na(core_length_flag_correct)
#   
#   # Fix dates
#   cores_fixed <- tables_to_update[[i]] %>% 
#     # some recoding
#     mutate(core_position_method = recode(core_position_method, "RTK-GPS" = "RTK"),
#            core_elevation_method = recode(core_elevation_method, "RTK-GPS" = "RTK")) %>%
#     # separate out year month day
#     mutate(year = year(ymd(core_date)), 
#            month = month(ymd(core_date)),
#            day = day(ymd(core_date))) %>%
#     # resolve cases where core date is NA but core year month or day are provided
#     mutate(year = ifelse(is.na(year) & !is.na(core_year), yes = core_year, no = year),
#            month = ifelse(is.na(month) & !is.na(core_month), yes = core_month, no = month),
#            day = ifelse(is.na(day) & !is.na(core_day), yes = core_day, no = day)) %>%
#     select(-c(core_date, core_year, core_month, core_day)) %>% # get rid of cols used to generate split date cols
#     rename(core_year = year)
#   
#   # Fix depths 
#   # (CHANGE IN HOOK SCRIPTS INSTEAD)
#   if(unique(cores_fixed$study_id) %in% core_depth_fixes$study_id){
#     cores_fixed <- cores_fixed %>%   
#       left_join(core_depth_fixes) %>% # join with fixed core depths
#       mutate(core_length_flag = ifelse(is.na(core_length_flag),
#                                        core_length_flag_correct,
#                                        core_length_flag)) %>% 
#       select(-core_length_flag_correct) 
#   }
#   
#   # store updated table
#   tables_to_update[[i]] <- cores_fixed
# }

updateTables <- function(table_names){
  # create a list of datasets from the provided table names
  datasets <- mget(table_names, envir = .GlobalEnv)
  # store the list of tables that will become the updated synthesis
  tables_to_update <- datasets
  
  # Read in Guidance for Update
  versioning_guidance <- read_csv("docs/versioning/converting_v1p2_to_v2.csv", col_types = cols())
  # database_guidance <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols())

  # isolate cases of renaming and deletion 
  to_rename <- versioning_guidance %>% filter(action == "rename")
  to_delete <- versioning_guidance %>% filter(action == "deletion" & heirarchy != "species_definitions")
  
  # Iterate through list of tables
  for (i in 1:length(tables_to_update)) {
    # Store table category
    category <- names(tables_to_update[i])
    
    # ... Table Fixes ####
    # Apply fixes that should occur to specific tables before column renaming and targeted deletions
    
    # Species table fixes
    if(category == "species"){
      print("Fixing species table.")
      
      # load lookup tables for targeted fixes
      # Species table fixes 
      species_fixes <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv", col_types = cols())
      
      # Fix species
      species_fixed <- tables_to_update[[i]] %>%
        left_join(species_fixes) %>% 
        mutate(species_code = ifelse(!is.na(recode_as), 
                                     recode_as, 
                                     species_code)) %>% 
        select(-c(recode_as, notes))
      # store updated table
      tables_to_update[[i]] <- species_fixed
    }  
    
    
    # ... Rename and Delete Necessary Columns ####
    
    # Rename columns
    # If any colnames are in the to_rename table, rename these columns 
    if(any(names(tables_to_update[[i]]) %in% to_rename$attribute_name_v1p2)){
      print(paste0("Renaming columns for ", category))
      # apply renaming function
      tables_to_update[[i]] <- renameColumns(x = tables_to_update[[i]], key = to_rename)
      
      if(length(unique(names(tables_to_update[[i]]))) != length(names(tables_to_update[[i]]))){
        warning(paste0("Column names duplicated during renaming: ", category))
      }
    }
    # this step will cause duplication of colnames if the new colnames already exist in the df
    
    # Delete (specified) columns
    # If any colnames are in the to_delete table, deselect these columns 
    if(any(names(tables_to_update[[i]]) %in% to_delete$attribute_name_v1p2)){
      print(paste0("Deleting targeted columns for ", category))
      print(names(tables_to_update[[i]])[which(names(tables_to_update[[i]]) %in% to_delete$attribute_name_v1p2)])
      
      tables_to_update[[i]] <- tables_to_update[[i]] %>% select(-any_of(to_delete$attribute_name_v1p2))
    }
    
    # table_guidance <- filter(database_guidance, table == category)
    # guidance_subset_actions <- filter(guidance_subset, !is.na(action))
    #
    # # If the guidance involves a rename, rename it
    # if (nrow(guidance_subset_actions) > 0) {
    #   for (j in 1:nrow(guidance_subset_actions)) {
    #     if (guidance_subset_actions$action[j] == "rename") {
    #       
    #       # Old names
    #       if (guidance_subset_actions$attribute_name_v1p2[j] %in% names(tables_to_update[[i]])) {
    #         # isolate the old column 
    #         old_column <- tables_to_update[[i]][,guidance_subset_actions$attribute_name_v1p2[j]]
    #         # [[1]] not sure what this was for
    #         
    #         # For any not NA entries in the old category, 
    #         tables_to_update[[i]][!is.na(old_column), guidance_subset_actions$attribute_name[j]] <- 
    #           old_column[!is.na(old_column)] # write them over to a column with a new category
    #       }
    #     }
    #   }
    # }
    # 
    # # Classes to include
    # include_these <- unique(guidance_subset$attribute_name[guidance_subset$attribute_name %in% names(tables_to_update[[i]])])
    # 
    # # Remove attributes not in guidance 2
    # # ... and order columns as in guidance v2
    # renamed_and_reordered <- tables_to_update[[i]][, include_these]
    # 
    # tables_to_update[[i]] <- renamed_and_reordered
  } 
  
  # reorder columns according to guidance
  # final_synthesis <- reorderColumns(tables = tables, ccrcn_synthesis = tables_to_update)
  
  return(tables_to_update)
}
