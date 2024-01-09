## CCN Data Library ####

## Soil core data curation script for Schieder and Kirwan 2019
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_cores.csv")
depthseries <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_depthseries.csv")
methods <- read.csv("data/primary_studies/Schieder_and_Kirwan_2019/original/schieder_and_kirwan_2019_methods.csv")
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
