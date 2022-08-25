## Write function to check fo duplicate cores in database

# create objects to test
cores <- ccrcn_synthesis$cores
ds <- ccrcn_synthesis$depthseries
sites <- ccrcn_synthesis$sites
methods <- ccrcn_synthesis$methods
species <- ccrcn_synthesis$species
citations <- ccrcn_synthesis$studycitations

# Comparisons within the core table first
# 1) look for the same coring locations
# sampling year? 
  # Doesn't work for piazza cores, in the holmquist synth, there is no sampling year 
  # assume that IDs might not match..perhaps a partial match could be made though

core_list <- cores %>%
  mutate(lat_long = paste(latitude, longitude, sep=", ")) %>%
  group_by(lat_long) %>%
  summarize(n = n(), 
            core_ids = paste(unique(core_id), collapse=", "),
            study_ids = paste(unique(study_id), collapse=", "),
            num_studies = length(unique(study_id))) %>%
  filter(n > 1) %>% 
  filter(num_studies > 1) %>% 
  filter(lat_long != "NA, NA")

# id_groups <- core_list %>% distinct(core_ids) %>% pull(core_ids)

# Comparison in the depthseries table next

  ds_list <- ds %>% 
    group_by(dry_bulk_density, fraction_carbon, fraction_organic_matter, depth_min, depth_max) %>%
    summarize(n = n(), 
              core_ids = paste(unique(core_id), collapse=", "),
              study_ids = paste(unique(study_id), collapse=", "),
              num_studies = length(unique(study_id))) %>%
    filter(n > 1) %>% filter(num_studies > 1) %>% 
    drop_na(fraction_carbon, fraction_organic_matter)
  
  # target values of dry bulk density, fraction organic matter, and fraction carbon
  # see if the core IDs compare to those identified in the cores table





