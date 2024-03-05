## CCN Data Library ####

## Soil core data curation script for Morgan et al 2024
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
methods <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_methods.csv")
depthseries <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_depthseries.csv")
site <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_site.csv")
cores <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_cores.csv")
plot_summary <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_plot_summary.csv")
study_citations <- read.csv("data/primary_studies/Morgan_et_al_2024/original/morgan_et_al_2024_associated_publications.csv") %>% 
  add_row(study_id = "Morgan_et_al_2024", 
          bibliography_id = "Morgan_et_al_2024_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: Soil organic matter in fringing and meadow salt marshes in Great Bay, New Hampshire and southern Maine",
          author = "Morgan, P.A., Burdick, D.M. & Short, F.T.",
          doi = "10.25573/serc.25222124", 
          url = "https://smithsonian.figshare.com/articles/dataset/Dataset:_Soil_organic_matter_in_fringing_and_meadow_salt_marshes_in_Great_Bay,_New_Hampshire_and_southern_Maine/25222124", 
          year = 2024,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          pages = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")


## Write files ####
write_csv(methods, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_methods.csv")
write_csv(depthseries, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_depthseries.csv")
write_csv(site, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_sites.csv")
write_csv(cores, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_cores.csv")
write_csv(study_citations, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_study_citations.csv")
write_csv(plot_summary, "data/primary_studies/Morgan_et_al_2024/derivative/morgan_et_al_2024_plot_summary.csv")
