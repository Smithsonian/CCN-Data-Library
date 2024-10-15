## Coastal Wetland Soil Carbon Stocks ####
## wolfejax@si.edu

## Prepare Workspace ####
# library(tidyverse)

source("scripts/3_post_processing/scripts/utility_funs.R")

## Step 1. Load current core and ds tables ####

cores <- ccrcn_synthesis$cores 
ds <- ccrcn_synthesis$depthseries

## Step 2. Prepare the tables for stock calculation ####

# apply to remove cores where the total sampled interval is more than the max depth
# leave_out <- c("M1694", "M1710",               "M1178",               "M1174",               "M1145",
#                "Coyote_Creek_2",      "Coyote_Creek_4",      "Mud_Slough_1",        "Mud_Slough_2",        "Mud_Slough_3",
#                "Plummer_Slough_1",    "Ravenswood_Slough_2", "Triangle_Marsh_2")

tc_seagrass <- c("Danovaro_1996",
                 "Fourqurean_and_Kendrick_unpublished",
                 "Fourqurean_unpublished",
                 "Kamp-Nielsen_et_al_2002",
                 "Koch_and_Madden_2001",
                 "Orem_et_al_1999",
                 "Rigollet_et_al_2004",
                 "Fourqurean_et_al_2010",
                 "Erftemeijer_and_Middelburg_1993",
                 "Alongi_et_al_2008")

# take a look at duplicate intervals
# dup_intervals <- ds %>%
#   left_join(cores_with_typology %>%  select(study_id, site_id, core_id, habitat, country)) %>%
#   filter_at(vars(fraction_organic_matter, fraction_carbon, dry_bulk_density), all_vars(!is.na(.))) %>%
#   add_count(study_id, site_id, core_id, method_id, habitat, country, depth_min, depth_max) %>%
#   filter(n > 1) %>% arrange(study_id, core_id, depth_min) %>%
#   distinct(study_id, site_id, core_id, method_id, n)
# take the mean of these duplicated intervals? Depends - are they replicates or from separate cores?

## Mangrove typologies has to be joined here
## Read in the table Annie sent and incorporate this consideration into the gapfilling step
typologies <- read_csv("docs/post_processing/core_typology_lookup.csv")
  # select(study_id, site_id, core_id, Class, Sedimentar) %>% drop_na(Sedimentar)
# apply the following from Breithaupt et al.
# Carbonate 0.002 (±0.000) × SOM2 + 0.326 (± 0.019) × SOM + 1.8 (±0.4)%
# Terrigenous 0.004 (±0.000) × SOM2 + 0.217 (± 0.022) × SOM + 0.3 (±0.3)%

# Prepare core and depthseries tables

cores_with_typology <- cores %>% left_join(typologies)
  # the following belongs in the geography assignment script
  # mutate(country = case_when(country == "Netherlands" ~ "Bonaire",
  #                            country == "France" & study_id != "Lallier-Verges_et_al_1998" ~ "French Guiana",
  #                            study_id == "Lallier-Verges_et_al_1998" ~ "Guadeloupe",
  #                            country == "Democratic Republic of the Congo" ~ "Congo DRC",
  #                            T ~ country)
  #        # core_id = case_when(study_id == "Kemp_et_al_2024" ~ paste(site_id, core_id, sep = "_"),
  #        #                     # study_id == "Schieder_and_Kirwan_2019" ~ paste(core_id, method_id, sep = " "), # no method ID in the cores table..
  #        #                     T ~ core_id)
  #        )

ds_clean <- ds %>%
  mutate(core_id = case_when(
    # study_id == "Kemp_et_al_2024" ~ paste(site_id, core_id, sep = "_"),
                             core_id %in% c("G12", "G13") ~ paste(core_id, method_id, sep = " "),
                             T ~ core_id)) %>%
  select(study_id:compaction_notes, depth_interval_notes) %>%
  left_join(cores_with_typology %>% select(study_id, site_id, core_id, habitat, country), multiple = "all") %>%
  mutate(habitat = case_when(study_id == "Schieder_and_Kirwan_2019" ~ "marsh", T ~ habitat)) %>%
  # convert some cols to numeric as needed 
  mutate_at(vars(representative_depth_min, representative_depth_max, depth_min, depth_max, 
                 fraction_carbon, dry_bulk_density, fraction_organic_matter), as.numeric) %>% 
  # transfer representative depths to actual depths (need to adjust this later)
  # need to double check the handling of representative depths in these calculations
  # sometimes both actual and representative depths are present
  mutate(depth_min = ifelse(is.na(depth_min) & !is.na(representative_depth_min),
                            representative_depth_min, depth_min),
         depth_max = ifelse(is.na(depth_max) & !is.na(representative_depth_max),
                            representative_depth_max, depth_max)) %>%
  # we need to take a closer look at depth intervals in the synthesis...
  mutate(         # spot correction for depths (needs correcting in their hook scripts)
    depth_min = case_when(
      # study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 15 ~ 0,
      #                     study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 30 ~ 15,
      #                     study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 50 ~ 30,
      #                     study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 100 ~ 50,
      #                     study_id == "Kauffman_et_al_2020_Brazil" & depth_max == 300 ~ 100,
                          study_id == "Trettin_et_al_2017" & depth_max == 75 ~ 70,
                          core_id == "Ghurab_NN_seagrass_2" & depth_max == 29.5 ~ 26.5,
                          study_id == "S._Gress_Unpublished,_Gress_et_al_2016" & depth_max == 200 ~ 150,
                          core_id == "MOU_Mounde_4_1" & depth_max == 130 ~ 130, # now the depth max needs to be set to 180
                          core_id == "M1145" & depth_max == 100 ~ 50,
                          core_id == "M1710" & depth_max == 100 ~ 41,
                          T ~ depth_min),
    depth_max = case_when(core_id == "MOU_Mounde_4_1" & depth_min == 130 ~ 180,
                          core_id == "M1694" & depth_min == 0 ~ 20,
                          core_id == "M1111" & depth_min == 45 ~ 51,
                          T ~ depth_max)) %>%
  filter(!is.na(depth_max)) %>%
  # some negative fraction carbon values...assume no carbon
  # Asked H. Morrissette and it was explained as methodological variance (EA-measured inorganic C > EA-measured total C)
  mutate(fraction_carbon = case_when(fraction_carbon < 0 ~ 0,
                                     # remove C for seagrass studies where it was reported as TC
                                     habitat == "seagrass" & study_id %in% tc_seagrass ~ NA,
                                     T ~ fraction_carbon)) %>%

  # remove intervals with only one observation and that represent less than 50% of 1m
  # add_count(study_id, site_id, core_id) %>%
  # mutate(to_remove = case_when(n == 1 & depth_max - depth_min < 50 ~ TRUE, T ~ F)) %>%
  # filter(to_remove == F) %>%
  # select(-n) %>%
  # average duplicate intervals from the same core
  dplyr::group_by(study_id, site_id, core_id, method_id, habitat, country, depth_min, depth_max) %>%
  # Aggregate by horizon intervals
  dplyr::summarise(dry_bulk_density = mean(dry_bulk_density, na.rm = T),
                   fraction_organic_matter = mean(fraction_organic_matter, na.rm = T),
                   fraction_carbon = mean(fraction_carbon, na.rm = T)) %>%
  ungroup() %>%
  left_join(typologies, multiple = "all")

# test calculations with random or purposefully selected cores

testing <- FALSE

# chosen_cores <- leave_out
# chosen_cores <- sample(cores_with_typology$core_id, 7)
# chosen_cores <- typologies$core_id
chosen_cores <- c("Channel Caye_1_1_mangrove", "MPW1", "M0006", "Humboldt Bay 684", "Burns_and_Swart_1992_1",
                  "ClamShack", "CPF_2", "MT06")

# create test dataset
test_ds <- ds_clean %>% filter(core_id %in% chosen_cores)
# test_ds <- ds_clean %>% filter(habitat %in% c("mangrove", "seagrass"))

# plot sampling intervals
# ggplot(test_ds) +
#   geom_crossbar(aes(x = site_id, y = depth_min,
#                     ymin = depth_min, ymax = depth_max,
#                     group = site_id, col = habitat),
#                 # eliminate the "y" line
#                 fatten = 0) +
#   scale_y_reverse() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   ylab("Depth Intervals (cm)") + xlab("Core ID")
# ggsave("figures/depth_interval_example.jpg", height = 8, width = 6)

## Step 3. Gapfill Carbon values ####

if(testing == TRUE){
  ds_gf <- gapfillCarbon(test_ds)
} else {
  ds_gf <- gapfillCarbon(ds_clean)
}

# plot carbon depthseries for subset
# test_ds_gf %>% drop_na(fraction_carbon) %>%
#   ggplot(aes(x = depth_min, y = fraction_carbon * 100, col = habitat)) +
#   geom_point() +
#   geom_line() +
#   scale_x_reverse() +
#   coord_flip() +
#   facet_wrap(~core_id, scales = "free_y") +
#   ylab("Organic Carbon (%)")

# look at coverage after gapfilling
nrow(ds_gf %>% drop_na(fraction_carbon))/nrow(ds_gf)
# 86% coverage - gets better with more habitats assigned
# Nahlic and Fennessey marsh outlier DBD sample_id NWCA11-3583-1-11-MD-019-016-1

ds_gf %>%
  drop_na(fraction_organic_matter, fraction_carbon) %>%
  # filter(fraction_carbon < 1) %>%
  ggplot(aes(fraction_organic_matter, fraction_carbon, col = OM_flag)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~habitat)

## Step 4. Assign representative depths ####

# determine representative depths
# this step takes the longest
ds_complete_depths <- ds_gf %>%
  # isolate intervals that contain both DBD and OC
  drop_na(dry_bulk_density, fraction_carbon) %>%
  # if fraction carbon is less than 0, treat as 0
  mutate(fraction_carbon = case_when(fraction_carbon < 0 ~ 0, T ~ fraction_carbon)) %>%
  arrange(study_id, site_id, core_id, depth_min) %>%
  # filter(core_id %in% sus_cores) %>%

  group_by(study_id, site_id, core_id) %>%
  mutate(depth_between = lead(depth_min) - depth_max) %>%
  mutate(
         new_depth_max = case_when(depth_between == 0 | is.na(depth_between) ~ depth_max,
                                   depth_between < 0 ~ depth_max,
                                   T ~ depth_max + (depth_between/2)),
         new_depth_min = case_when(depth_min == 0 ~ depth_min,
                                   # depth_between == 0 ~ depth_min,
                                   depth_between < 0 ~ depth_min,
                                   T ~ depth_min - (lag(depth_between)/2)),
         # Correct cases where the core does not start at depth_min = 0
         new_depth_min = case_when(is.na(new_depth_min) ~ 0, T ~ new_depth_min)
         ) %>%
  select(depth_min, depth_max, depth_between,
         # new_depth_min, new_depth_max,
         everything()) %>%
  ungroup()

# What to do with the negative cases for depth_between?
# Indicates overlap of measured intervals, which would inflate the C sum
# -1 to 0 is negligible
# Going to spot correct in the ds cleaning step earlier to address bigger gaps
sus_cores <- ds_complete_depths %>% filter(depth_between < -1) %>% distinct(core_id) %>% pull(core_id)
View(ds_complete_depths %>% filter(core_id %in% sus_cores))

ds_depths_final <- ds_complete_depths %>%
  mutate(new_depth_max = case_when(depth_between < 0 ~ lead(new_depth_min), T ~ new_depth_max)) %>% 
  select(new_depth_min, new_depth_max, everything())
  # filter(core_id %in% sus_cores)

# plot intervals to check
# ds_complete_depths %>%
#   ggplot() +
#   geom_crossbar(aes(x = core_id, y = new_depth_min,
#                     ymin = new_depth_min, ymax = new_depth_max,
#                     group = core_id, col = habitat),
#                 fatten = 0) + # eliminate the "y" line
#   scale_y_reverse() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Step 5. Calculate soil carbon density and stocks for each interval ####

ds_stocks <- ds_depths_final %>%
  # calculate carbon density and interval stocks
  mutate(carbon_density = dry_bulk_density * fraction_carbon,
         # units: g/cm3 * cm * 10000cm2/m2 => gC m-2
         stock_gCm2 = carbon_density * (new_depth_max - new_depth_min) * 10000, # this will be representative depths
         # convert gC m-2 to MgC ha-1
         stock_MgHa = stock_gCm2 * (10^4/10^6))

# ds_stocks %>% drop_na(fraction_organic_matter, fraction_carbon) %>%
#   filter(habitat == 'mangrove') %>%
#   ggplot(aes(fraction_organic_matter, fraction_carbon)) + geom_point(pch = 1) +
#   facet_wrap(~OM_flag)

## Step 6. Calculate total and standard 1-meter carbon stock for each core ####
corestock_total <-  ds_depths_final %>%
  # calculate carbon density and interval stocks
  mutate(carbon_density = dry_bulk_density * fraction_carbon,
         # units: g/cm3 * cm * 10000cm2/m2 => gC m-2
         stock_gCm2 = carbon_density * (new_depth_max - new_depth_min) * 10000, # this will be representative depths
         # convert gC m-2 to MgC ha-1
         stock_MgHa = stock_gCm2 * (10^4/10^6)) %>%

  # mutate(interval_sampled = depth_max - depth_min) %>%
  dplyr::group_by(study_id, site_id, core_id) %>%
  # dplyr::mutate(total_depth = sum(interval_sampled),
  #               weight = interval_sampled / total_depth) %>%
  dplyr::summarize(stock_MgHa_total = sum(stock_MgHa)) %>%
    # stock_MgHa_total_weighted = sum(stock_MgHa * weight),
  ungroup()

# determine C stock for 1 meter depth
# target_interval <- data.frame(horizon_min = 0, horizon_max = 100)

corestock_1m <- ds_depths_final %>%
  # if interval is provided merge this, otherwise treat the min as 0 and max as max depth
  mutate(horizon_min = 0, horizon_max = 100) %>%
  # merge(target_interval) %>%
  # Keeps intervals between min and max horizon
  # If an interval crosses a horizon, it remains
  dplyr::filter(pmax(new_depth_min, horizon_min) < pmin(new_depth_max, horizon_max)) %>%
  # truncate intervals that go over 1m
  mutate(new_depth_max = case_when(new_depth_max > horizon_max ~ horizon_max, T ~ new_depth_max)) %>%
  # dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max) %>%
  # Calculate weights for each interval
  # if an interval exceeds the max target depth, a weight of less than 1 will be applied
  # mutate(weight = case_when(depth_max > horizon_max ~ (horizon_max - new_depth_min) / (new_depth_max - new_depth_min),
  #                           T ~ 1)) %>%
  # dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
  #                                        (horizon_max-depth_min),
  #                                        (depth_max-horizon_min), na.rm=T)) %>%

  # calculate carbon density and interval stocks
  mutate(carbon_density = dry_bulk_density * fraction_carbon,
         # units: g/cm3 * cm * 10000cm2/m2 => gC m-2
         stock_gCm2 = carbon_density * (new_depth_max - new_depth_min) * 10000, # this will be representative depths
         # convert gC m-2 to MgC ha-1
         stock_MgHa = stock_gCm2 * (10^4/10^6)) %>%

  dplyr::group_by(study_id, site_id, core_id) %>%
  # dplyr::mutate(total_depth = sum(overlapping_depth),
  #               weight = overlapping_depth / total_depth) %>%
  # Aggregate by horizon intervals
  dplyr::summarise(stock_MgHa_1m = sum(stock_MgHa)) %>%
  ungroup()

# create a table with all the core stocks
corestocks_all <- full_join(corestock_total, corestock_1m) %>%
  # mutate(core_id = recode(core_id, "SP_SP_1" = "SP_1")) %>% 
  filter(site_id != "TP" | is.na(site_id)) # Kemp et al 2024, this core calculates a 0 stock
# theres a Thom core with no site ID

corestocks_final <- left_join(cores, corestocks_all)
  # filter(core_id %in% unique(corestocks_all$core_id))
  # select(study_id, site_id, core_id, habitat, latitude, longitude, country, admin_division, max_depth) %>%
# count(study_id, site_id, core_id)
# summary(corestocks_final)

# write to synthesis
ccrcn_synthesis$cores <- corestocks_final

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])


