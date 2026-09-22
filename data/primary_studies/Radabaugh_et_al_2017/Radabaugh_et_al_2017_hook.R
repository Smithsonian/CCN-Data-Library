## CCN Data Library ####

## Biomass data curation script for Radabaugh et al 2017
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)


allometric_eq <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_allometric_eq.csv")
plant_plot_detail <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_plant_plot_detail.csv")
plant <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_plant.csv")
plot_summary <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_plot_summary.csv")
species <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_species.csv")
study_citations <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_study_citations.csv")
uncontrolled_attributes <- read.csv("data/primary_studies/Radabaugh_et_al_2017/original/radabaugh_et_al_2017_uncontrolled_attributes.csv")


bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


write_csv(allometric_eq, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_allometric_eq.csv")
write_csv(plant_plot_detail, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_plant_plot_detail.csv")
write_csv(plant, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_plant.csv")
write_csv(plot_summary, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_plot_summary.csv")
write_csv(species, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_species.csv")
write_csv(study_citations, "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Radabaugh_et_al_2017/derivative/radabaugh_et_al_2017_study_citations.bib")

