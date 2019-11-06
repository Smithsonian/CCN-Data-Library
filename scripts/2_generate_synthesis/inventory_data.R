library(tidyverse)

cores <- read.csv("./data/CCRCN_synthesis/CCRCN_cores.csv")
depthseries <- read.csv("./data/CCRCN_synthesis/CCRCN_depthseries.csv")

guidance <- read.csv("./docs/ccrcn_database_structure.csv")

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
  filter(parent_data_category == "age_depth" & table == "depthseries" & !is.na(quality_class)) %>%
  select(attribute_name, data_category, quality_class)

age_depth_types <- unique(age_depth$data_category)
df <- setNames(data.frame(matrix(ncol = length(age_depth_types) + 1, nrow = 0)), c("study_id", as.character(age_depth_types)))

for(id in unique(depthseries$study_id)){
  results <- id
  for(type in age_depth_types){
    attribute_levels <- filter(age_depth, data_category == type)
    
    for(quality_level in attribute_levels$attribute_name){
        
    }
  }
}

test <- depthseries %>%
  group_by(study_id, core_id) %>%
  summarize(quality_level = any(!is.na(quality_level)))

summarise_vars <- list(list(c('study_id', 'core_id'), 'cs137_activity_sd'), 
                       list(c('study_id', 'core_id'), 'total_pb210_activity_sd'))

for (v in summarise_vars) {
  group_var <- unlist(v[1])   # group by this variable
  print(v[1])
  
  summ <- paste0('any(!is.na(', v[2], '))')  # construct summary method, e.g. mean(mpg) any(!is.na(total_pb210_activity_sd))
  summ_name <- paste0('mean_', v[2])  # construct summary variable name, e.g. mean_mpg
  
  print(paste('grouping by', group_var, 'and summarising', summ))
  
  df_summ <- depthseries %>%
    group_by_(.dots = group_var) %>%
    summarise_(.dots = setNames(summ, summ_name))
  
  print(df_summ)
}

depthseries_inventory <- depthseries %>%
  group_by(study_id, core_id) %>%
  summarize(cs137_measured = any(!is.na(cs137_activity)),
            cs137_sd_measured = any(!is.na(cs137_activity_sd)),
            cs137_peak_measured = any(cs137_peak_present, na.rm=TRUE),
            total_pb210_measured =  any(!is.na(total_pb210_activity)),
            total_pb210_sd_measured = any(!is.na(total_pb210_activity_sd)),
            excess_pb210_measured =  any(!is.na(excess_pb210_activity)),
            excess_pb210_sd_measured = any(!is.na(excess_pb210_activity_sd)),
            ra226_measured =  any(!is.na(ra226_activity)),
            ra226_sd_measured = any(!is.na(ra226_activity_sd)),
            c14_measured =  any(!is.na(c14_age)),
            c14_sd_measured = any(!is.na(c14_age_sd)),
            delta_c13_measured =  any(!is.na(delta_c13)),
            be7_measured =  any(!is.na(be7_activity)),
            be7_sd_measured = any(!is.na(be7_activity_sd)),
            am241_measured =  any(!is.na(am241_activity)),
            am241_sd_measured = any(!is.na(am241_activity_sd)),
            marker_date_measured = any(!is.na(marker_date)),
            marker_date_sd_measured = any(!is.na(marker_date_sd)),
            pb214_measured =  any(!is.na(pb214_activity)),
            pb214_sd_measured = any(!is.na(pb214_activity_sd)),
            th234_measured =  any(!is.na(th234_activity)),
            th234_sd_measured = any(!is.na(th234_activity_sd)),
            bi214_measured =  any(!is.na(bi214_activity)),
            bi214_sd_measured = any(!is.na(bi214_activity_sd)),
            #age_measured = any(!is.na(age)), 
            #age_sd_measured = any(!is.na(age_sd)), 
            age_min_measured = any(!is.na(age_min)), 
            age_max_measured = any(!is.na(age_max)), 
            pb212_measured =  any(!is.na(pb212_activity)),
            pb212_sd_measured = any(!is.na(pb212_activity_sd))
  )
