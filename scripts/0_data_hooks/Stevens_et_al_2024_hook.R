## CCN Data Library ####

## Soil core data curation script for Stevens et al 2024
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
species <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_species.csv")
impacts <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_impacts.csv")
methods <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_methods.csv")
cores <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_cores.csv")
depthseries <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_depthseries.csv")
sites <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_sites.csv")
associated_publications <- read.csv("data/primary_studies/Stevens_et_al_2024/original/stevens_et_al_2024_associated_publications.csv") 


## Write files ####
write_csv(cores, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_cores.csv")
write_csv(depthseries, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_depthseries.csv")
write_csv(methods, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_methods.csv")
write_csv(impacts, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_impacts.csv")
write_csv(species, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_species.csv")
write_csv(sites, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_sites.csv")
write_csv(associated_publications, "data/primary_studies/Stevens_et_al_2024/derivative/stevens_et_al_2024_associated_publications.csv")

