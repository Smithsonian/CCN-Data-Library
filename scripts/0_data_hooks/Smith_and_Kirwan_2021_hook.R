## CCN Data Library ####

## Soil core data curation script for Smith and Kirwan 2021
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores <- read.csv("data/primary_studies/Smith_and_Kirwan_2021/original/smith_and_kirwan_2021_cores.csv")
depthseries <- read.csv("data/primary_studies/Smith_and_Kirwan_2021/original/smith_and_kirwan_2021_depthseries.csv")
methods <- read.csv("data/primary_studies/Smith_and_Kirwan_2021/original/smith_and_kirwan_2021_methods.csv")
study_citations <- read.csv("data/primary_studies/Smith_and_Kirwan_2021/original/smith_and_kirwan_2021_associated_publications.csv") %>% 
  add_row(study_id = "Smith_and_Kirwan_2021", 
          bibliography_id = "Smith_and_Kirwan_2021_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Sea Level-Driven Marsh Migration Results in Rapid Net Loss of Carbon",
          author = "Alexander Smith, Matthew Kirwan",
          doi = "10.25573/serc.24916407", 
          url = "https://smithsonian.figshare.com/articles/dataset/Sea_Level-Driven_Marsh_Migration_Results_in_Rapid_Net_Loss_of_Carbon/24916407", 
          year = 2023,
          month = NA,
          day = NA,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          issue = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")


## Write files ####
write.csv(cores, "./data/primary_studies/Smith_and_Kirwan_2021/derivative/Smith_and_Kirwan_2021_cores.csv")
write.csv(depthseries, "./data/primary_studies/Smith_and_Kirwan_2021/derivative/Smith_and_Kirwan_2021_depthseries.csv")
write.csv(methods, "./data/primary_studies/Smith_and_Kirwan_2021/derivative/Smith_and_Kirwan_2021_methods.csv")
write.csv(study_citations, "./data/primary_studies/Smith_and_Kirwan_2021/derivative/Smith_and_Kirwan_2021_study_citations.csv")
