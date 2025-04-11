## Tidy Biomass Synthesis

# Remove cols that don't align with database structure
plots <- ccrcn_synthesis$plots %>% 
  mutate(ecotype = tolower(coalesce(ecotype, ecosystem_type)),
         ecosystem_condition = tolower(ecosystem_condition)) %>% 
  select(c(
    "study_id", 
    "site_id", 
    "plot_id", 
    "year",
    "month",
    "day",                       
    "latitude",
    "longitude",
    "position_method",
    "habitat",   
    "ecotype",
    # "ecosystem_type",
    # "ecosystem_health",              
    "plot_area",
    "land_use_class",
    "disturbance_class",
    "disturbance_notes",
    "ecosystem_condition",
    # "land_use_status",
    # "plot_basal_density",
    "debris_carbon",     
    "understory_carbon",
    "aboveground_plant_carbon",  
    "belowground_plant_carbon",

    # "basal_area"   
    "plot_plant_carbon"
  )) 


plants <- ccrcn_synthesis$plants %>% 
  mutate(species = coalesce(species, species_code),
         plant_id = coalesce(tree_id, plant_id)) %>% 
  select(-c(
    "study_id",
    "site_id",
    "plot_id",
    "plot_radius",
    "species",
    "alive_or_dead",
    "decay_class",
    "plot_density",                 
    "height",
    "canopy_width",
    # [11] "basal_width", "biomass_decay_corrected", "plant_aboveground_mass"        
    "plant_aboveground_carbon",
    "plant_belowground_carbon",
    # [15] "plant_belowground_mass"        "plant_organic_matter_total"   
    "plot_area", 
    
    "diameter_flag",
    "diameter",
    # [21] "aboveground_carbon_conversion" "belowground_carbon_conversion"
     "diameter_crown",
    "basal_area",               
    "basal_density",
    "plant_notes"               
    # [27] "tree_id"                       "family"                       
    # [29] "wood_density"                  "plant_AGB_kg"                 
    # [31] "plant_AGBK_kg"                 "plant_BGB_kg"                 
    "plant_AGB_MgHa",
    # "plant_AGBK_MgHa"              
    "plant_BGB_MgHa",
    "plant_mass_total_MgHa",
    # [37] "plant_mass_totalK_MgHa"        "plant_aboveground_carbon_K"   
    "plant_total_carbon",
    # "plant_total_carbon_K"         
    "decay_reduction_factor",
    "carbon_conversion_factor",     
    "plant_id",
    "basal_diameter",
    # [45] "height_deadbreak"              
    "wood_density_source"          
    # [47] "species_code"                  "code_type"   
  ))

# Remove cols that don't align with database structure
  



# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
