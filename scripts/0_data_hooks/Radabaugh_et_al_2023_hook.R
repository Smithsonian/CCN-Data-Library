## CCN Data Library ####

## Soil core data curation script for Radabaugh et al 2023
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)


plant <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_plant.csv")
plot_summary <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_plot_summary.csv")
species <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_species.csv")
allometric_eq <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_allometric_eq.csv")
cores <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_cores.csv")
depthseries <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_depthseries.csv")
methods <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_methods.csv")
impacts <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_impacts.csv")
study_citations <- read.csv("data/primary_studies/Radabaugh_et_al_2023/original/radabaugh_et_al_2023_study_citations.csv")


bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


write_csv(plant, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_plant.csv")
write_csv(plot_summary, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_plot_summary.csv")
write_csv(species, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_species.csv")
write_csv(allometric_eq, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_allometric_eq.csv")
write_csv(cores, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_cores.csv")
write_csv(depthseries, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_depthseries.csv")
write_csv(methods, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_methods.csv")
write_csv(impacts, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_impacts.csv")
write_csv(study_citations, "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Radabaugh_et_al_2023/derivative/radabaugh_et_al_2023_study_citations.bib")

