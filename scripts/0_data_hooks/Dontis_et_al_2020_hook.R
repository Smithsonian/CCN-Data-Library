## CCN Data Library ####

## Biomass data curation script for Dontis et al 2020
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)


plant <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_plant.csv")
plot_summary <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_plot_summary.csv")
species <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_species.csv")
allometric_eq <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_allometric_eq.csv")
impacts <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_impacts.csv")
study_citations <- read.csv("data/primary_studies/Dontis_et_al_2020/original/Dontis_et_al_2020_study_citations.csv")


bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


write_csv(plant, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_plant.csv")
write_csv(plot_summary, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_plot_summary.csv")
write_csv(species, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_species.csv")
write_csv(allometric_eq, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_allometric_eq.csv")
write_csv(impacts, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_impacts.csv")
write_csv(study_citations, "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Dontis_et_al_2020/derivative/Dontis_et_al_2020_study_citations.bib")

