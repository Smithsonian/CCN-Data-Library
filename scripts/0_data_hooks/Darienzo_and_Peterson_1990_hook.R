## CCN Data Library ####

## Soil core data curation script for Darienzo and Peterson 1990
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_cores.csv")
depthseries <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_depthseries.csv")
methods <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_methods.csv")
species <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_species.csv")
impacts <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_impacts.csv")
study_citations <- read.csv("data/primary_studies/Darienzo_and_Peterson_1990/original/darienzo_and_peterson_1990_associated_publications.csv") %>% 
  add_row(study_id = "Darienzo_and_Peterson_1990", 
          bibliography_id = "Darienzo_and_Peterson_1990_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: Episodic Tectonic Subsidence of Late Holocene Salt Marshes, Northern Oregon Central Cascadia Margin",
          author = "Darienzo, Mark E. and Peterson, Curt D.",
          doi = "10.25573/serc.25270099", 
          url = "https://smithsonian.figshare.com/articles/dataset/Dataset:_Episodic_Tectonic_Subsidence_of_Late_Holocene_Salt_Marshes,_Northern_Oregon_Central_Cascadia_Margin/25270099", 
          year = 2024,
          month = NA,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          issue = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")


## Write files ####
write.csv(cores, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_cores.csv")
write.csv(depthseries, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_depthseries.csv")
write.csv(methods, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_methods.csv")
write.csv(study_citations, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_study_citations.csv")
write.csv(impacts, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_impacts.csv")
write.csv(species, "./data/primary_studies/Darienzo_and_Peterson_1990/derivative/Darienzo_and_Peterson_1990_species.csv")
