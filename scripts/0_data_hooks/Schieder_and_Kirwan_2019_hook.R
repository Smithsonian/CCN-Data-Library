## CCN Data Library ####

## Soil core data curation script for Schieder and Kirwan 2019
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_cores.csv")
methods <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_methods.csv")

depthseries <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_depthseries.csv") %>% 
  rename(cs137_activity = cs137_activity_661keV,
         cs137_activity_se = cs137_activity_se_661keV,
         total_pb210_activity = pb210_activity_46.5keV,
         total_pb210_activity_se = pb210_activity_se_46.5keV) %>% 
  mutate(ra226_activity = (pb214_activity_295keV + pb214_activity_351.9keV + bi214_activity_609keV)/3,
         ra226_activity_se = sqrt(pb214_activity_295keV^2 + pb214_activity_351.9keV^2 + bi214_activity_609keV^2)/3,
         ra226_unit = "DPM/g") %>% 
    select(study_id, site_id, core_id, method_id, depth_min, depth_max, sample_id, dry_bulk_density, fraction_organic_matter,
           cs137_activity, cs137_activity_se, cs137_unit, total_pb210_activity, total_pb210_activity_se, pb210_unit,
           ra226_activity, ra226_activity_se, ra226_unit)

study_citations <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_associated_publications.csv") %>% 
  add_row(study_id = "Schieder_and_Kirwan_2019", 
          bibliography_id = "schieder_and_kirwan_2019_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: Sea-level driven acceleration in coastal forest retreat",
          author = "Nathalie Schieder, Matthew Kirwan",
          doi = "xxx", 
          url = "xxx", 
          year = 2024,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          issue = NA,
          pages = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")

## Write files ####
write.csv(cores, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_cores.csv")
write.csv(depthseries, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_depthseries.csv")
write.csv(methods, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_methods.csv")
write.csv(study_citations, "./data/primary_studies/Schieder_and_Kirwan_2019/derivative/Schieder_and_Kirwan_2019_study_citations.csv")
