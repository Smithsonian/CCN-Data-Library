## CCN Data Library ####

## Soil core data curation script for Schieder and Kirwan 2019
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores_raw <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_cores.csv")
methods_raw <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_methods.csv")

depthseries_raw <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_depthseries.csv") %>% 
  rename(cs137_activity = cs137_activity_661keV,
         cs137_activity_se = cs137_activity_se_661keV,
         total_pb210_activity = pb210_activity_46.5keV,
         total_pb210_activity_se = pb210_activity_se_46.5keV) %>% 
  mutate(ra226_activity = (pb214_activity_295keV + pb214_activity_351.9keV + bi214_activity_609keV)/3,
         ra226_activity_se = sqrt(pb214_activity_295keV^2 + pb214_activity_351.9keV^2 + bi214_activity_609keV^2)/3,
         ra226_unit = "disintegrationsPerMinutePerGram") %>% 
    select(study_id, site_id, core_id, method_id, depth_min, depth_max, sample_id, dry_bulk_density, fraction_organic_matter,
           cs137_activity, cs137_activity_se, cs137_unit, total_pb210_activity, total_pb210_activity_se, pb210_unit,
           ra226_activity, ra226_activity_se, ra226_unit)


#RC spotfixes 
# G12 and G13 have a second replicate core with the same core id?

methods <- methods_raw

cores <- cores_raw


depthseries <- depthseries_raw %>% 
  mutate(core_id = case_when(sample_id == "replicate_2" ~ paste0(core_id,"_2"),
                             sample_id == "replicate_1" ~ paste0(core_id, "_1"),
                             TRUE ~ core_id))






study_citations <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_associated_publications.csv") %>% 
  add_row(study_id = "Schieder_and_Kirwan_2019", 
          bibliography_id = "schieder_and_kirwan_2019_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: Sea-level driven acceleration in coastal forest retreat",
          author = "Nathalie Schieder, Matthew Kirwan",
          doi = "10.25573/serc.25259983", 
          url = "https://smithsonian.figshare.com/articles/dataset/Dataset:_Sea-level_driven_acceleration_in_coastal_forest_retreat/10.25573/serc.25259983", 
          year = 2024,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          issue = NA,
          pages = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")

## Write files ####
write_csv(cores, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_cores.csv")
write_csv(depthseries, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_depthseries.csv")
write_csv(methods, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_methods.csv")
write_csv(study_citations, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_study_citations.csv")
