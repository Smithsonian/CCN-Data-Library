library(tidyverse)

cores <- read.csv("./data/CCRCN_synthesis/CCRCN_cores.csv")
depthseries <- read.csv("./data/CCRCN_synthesis/CCRCN_depthseries.csv")

guidance <- read.csv("./docs/ccrcn_database_structure.csv")
uncontrolled <- read.csv("./docs/uncontrolled_attributes.csv")

## Inventory data #####

## ... inventory core-level data ###########
# Check that no studies are coded "RTK" and not "RTK-GPS"
unique(cores$core_elevation_method)

# There is so recode that until fixed at the source 
cores <- cores %>%
  mutate(core_elevation_method = recode(core_elevation_method, "RTK" = "RTK-GPS"))

core_inventory_non_RTK <- cores %>%
  filter(core_elevation_method != "RTK-GPS" | is.na(core_elevation_method)) %>%
  group_by(study_id) %>%
  summarize(n_cores_no_RTK = n(),
            non_RTK_elevation_methods = paste(unique(core_elevation_method), collapse=", "))

core_inventory_RTK <- cores %>%
  filter(core_elevation_method == "RTK-GPS") %>%
  group_by(study_id) %>%
  summarize(n_cores_RTK = n())

core_inventory <- merge(core_inventory_RTK, core_inventory_non_RTK, by="study_id", all.x=TRUE, all.y=TRUE) %>%
  mutate(n_cores_no_RTK = ifelse(is.na(n_cores_no_RTK), 0, n_cores_no_RTK),
         n_cores_RTK = ifelse(is.na(n_cores_RTK), 0, n_cores_RTK))

## ... inventory depthseries-level data #####
age_depth <- guidance %>%
  bind_rows(uncontrolled) %>%
  filter(parent_data_category == "age_depth" & table == "depthseries" & !is.na(quality_class) & attribute_name %in% colnames(depthseries)) %>%
  select(attribute_name, data_category, quality_class) 

age_depth_types <- unique(age_depth$data_category)

# We will only summarize variables that are actually present in our dataset
summarize_vars <- unique(as.character(age_depth$attribute_name))[unique(as.character(age_depth$attribute_name)) %in% colnames(depthseries)]
group_var <- c("study_id", "core_id")

inventory_table <- depthseries %>%
  group_by_at(group_var) %>%
  summarize()

for (v in summarize_vars) {
  variable_summary <- depthseries %>%
    group_by_at(group_var) %>%
    # !! is bang bang operator - unquotes a single argument in a function call
    # function tests if any values for the current variable are not NA
    summarise(!!v := any(!is.na(!!sym(v)))) 
  
  inventory_table <- full_join(inventory_table, variable_summary)
  
}

# Classify quality level of each type of age depth data
age_depth_inventory <- select(inventory_table, group_var)

# For each age depth category... 
for (t in age_depth_types){
  type_subset <- filter(age_depth, data_category == t)
  max_quality <- max(type_subset$quality_class)
  
  age_depth_inventory[t] <- NA
  
  for(i in max_quality:1){
    attributes <- as.character(filter(type_subset, quality_class == i)$attribute_name)
    print(attributes)
    # If data exists for a current quality level, assign it to the study id in question
    # Because the loop index move from highest to lowest, lower levels will be replaced if higher quality data is found
    for(row in 1:nrow(inventory_table)) {
      if(TRUE %in% inventory_table[row, attributes]){
        age_depth_inventory[row, t] <- i
      }
    }
  }
}

