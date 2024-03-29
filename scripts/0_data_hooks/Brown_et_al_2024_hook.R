## CCN Data Library ####

## Soil core data curation script for Brown et al 2024
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
cores <- read.csv("data/primary_studies/Brown_et_al_2024/original/Brown_et_al_2024_cores.csv")
depthseries <- read.csv("data/primary_studies/Brown_et_al_2024/original/Brown_et_al_2024_depthseries.csv")
methods <- read.csv("data/primary_studies/Brown_et_al_2024/original/Brown_et_al_2024_methods.csv")
species <- read.csv("data/primary_studies/Brown_et_al_2024/original/Brown_et_al_2024_species.csv")

# Create bibliography
study_citations <- data.frame(study_id = "Brown_et_al_2024", 
                              bibliography_id = "Brown_et_al_2024_data",
                              publication_type = "primary dataset", 
                              bibtype = "misc", 
                              title = "Dataset: Accretion rates and carbon sequestration in Oregon salt marshes",
                              author = "Cheryl A. Brown, T Chris Mochon Collura, and Ted DeWitt",
                              doi = "10.25573/serc.25024448", 
                              url = "https://smithsonian.figshare.com/articles/dataset/Dataset_Accretion_rates_and_carbon_sequestration_in_Oregon_salt_marshes/25024448", 
                              year = 2024,
                              publisher = "Smithsonian Environmental Research Center",
                              copyright = "Creative Commons Attribution 4.0 International")
        

## Write files ####
write_csv(cores, "./data/primary_studies/Brown_et_al_2024/derivative/Brown_et_al_2024_cores.csv")
write_csv(depthseries, "./data/primary_studies/Brown_et_al_2024/derivative/Brown_et_al_2024_depthseries.csv")
write_csv(methods, "./data/primary_studies/Brown_et_al_2024/derivative/Brown_et_al_2024_methods.csv")
write_csv(species, "./data/primary_studies/Brown_et_al_2024/derivative/Brown_et_al_2024_species.csv")
write_csv(study_citations, "./data/primary_studies/Brown_et_al_2024/derivative/Brown_et_al_2024_study_citations.csv")

