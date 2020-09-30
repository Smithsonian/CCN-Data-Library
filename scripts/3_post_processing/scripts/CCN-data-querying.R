# Analysing the CCN Data
library(tidyverse)

#  c = character, i = integer, n = number, d = double, l = logical, f = factor, 
# D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
cores <- read_csv("CCRCN/CCRCN_cores.csv",
                  col_types="cccnnnDcnncccncnccccccccccccccc")
depths <- read_csv("CCRCN/CCRCN_depthseries.csv") %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, 
         dry_bulk_density, fraction_organic_matter, fraction_carbon)

species <- read_csv("CCRCN/CCRCN_species.csv")

max_depths <- depths %>% 
  filter(complete.cases(depth_max)) %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(max_depth = max(depth_max, na.rm=T))

max_depths_gt_1m <- max_depths %>% filter(max_depth > 100)

# Do a pass on core length notes, all NA's
studies_to_revisit <- cores %>% 
  group_by(study_id, core_length_flag) %>% 
  summarise(n=n()) %>% 
  group_by(study_id) %>% 
  filter(n == max(n)) %>% 
  filter(is.na(core_length_flag))

# Some of these I know
write_csv(studies_to_revisit, "studies_to_revisit.csv")

studies_revisited <- read_csv("studies_revisited.csv") %>% 
  summarise(core_length_flag == "core depth represents deposit depth")

# Core depth represents deposit depth OR max core depth > 1 m
cores_included <- cores %>% 
  filter(core_id %in% depths$core_id & ( # needs to actually be in the dataset
         core_length_flag == "core depth represents deposit depth" |
           study_id %in% studies_revisited$study_id |
           core_id %in% max_depths_gt_1m$core_id)) %>% 
  # Filter out Palustrine from Nhalik and Fennesey 
  filter(study_id == "Nahlik_and_Fennessy_2016" & salinity_class != "palustrine")

  
    
# Merge species-based and vegetation-type based classifications
three_class <- species
  
  

# Visualize

# Map

# OM vs BD

# 