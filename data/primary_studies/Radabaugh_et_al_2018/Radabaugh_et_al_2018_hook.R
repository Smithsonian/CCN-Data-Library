## CCN Data Library ####

## Soil core and biomass data curation script for Radabaugh et al 2018
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)


cores <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_cores.csv")
depthseries <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_depthseries.csv")
methods <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_methods.csv")
plot_summary <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_plot_summary.csv")
impacts <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_impacts.csv")
study_citations <- read.csv("data/primary_studies/Radabaugh_et_al_2018/original/radabaugh_et_al_2018_study_citations.csv")


bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")


write_csv(cores, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_cores.csv")
write_csv(depthseries, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_depthseries.csv")
write_csv(methods, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_methods.csv")
write_csv(plot_summary, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_plot_summary.csv")
write_csv(impacts, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_impacts.csv")
write_csv(study_citations, "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Radabaugh_et_al_2018/derivative/radabaugh_et_al_2018_study_citations.bib")

