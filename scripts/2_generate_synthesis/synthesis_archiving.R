

# temporary script to compute and test change log

archived_filepaths <- list.files("data/CCRCN_synthesis/archive", pattern = ".csv", full.names = T)

# List will hold the previous synthesis
archived_synthesis <- vector("list", length(tables))
names(archived_synthesis) <- tables

# Read in data 
for(file in archived_filepaths){
  
    # Extract table type from file name
    table_type <- tables[which(str_detect(file, tables))]
    
    archived_synthesis[[table_type]] <- read_csv(file, col_types = cols(.default = "c"))
}

# id_vars <- c('study_id', 'site_id', 'core_id', 'method_id')

# create reference tables for previous version of the synthesis
prev_cores <- archived_synthesis$cores %>% distinct(study_id, site_id, core_id) %>%  
  mutate(table = "cores") %>% select(table, everything())

prev_ds <- archived_synthesis$depthseries %>% distinct(study_id, site_id, core_id)  %>%  # add method_id 
  mutate(table = "depthseries") %>% select(table, everything()) 

prev_methods <- archived_synthesis$methods %>% distinct(study_id) %>% # add method_id  
  mutate(table = "methods") %>% select(table, everything()) 

prev_sites <- archived_synthesis$sites %>% distinct(study_id, site_id) %>% 
  mutate(table = "sites") %>% select(table, everything())

prev_species <- archived_synthesis$species %>% distinct(study_id, site_id, core_id) %>%  
  mutate(table = "species") %>% select(table, everything())

prev_impacts <- archived_synthesis$impacts %>% distinct(study_id, site_id, core_id) %>% 
  mutate(table = "impacts") %>% select(table, everything())

prev_citations <- archived_synthesis$study_citations %>% distinct(study_id) %>%  
  mutate(table = "study_citations") %>% select(table, everything())

# bind into one table
prev_data <- bind_rows(prev_methods, prev_sites, prev_cores, prev_ds, prev_species, prev_impacts, prev_citations)

write_csv(prev_data, "docs/synthesis_resources/id_variable_archive.csv")

