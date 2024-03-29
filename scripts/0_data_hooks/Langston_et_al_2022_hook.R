## CCN Data Library ####

## Soil core data curation script for Langston et al 2022
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)

## Read files ####
species <- read.csv("data/primary_studies/Langston_et_al_2022/original/langston_et_al_2022_species.csv")
cores <- read.csv("data/primary_studies/Langston_et_al_2022/original/langston_et_al_2022_cores.csv")
depthseries <- read.csv("data/primary_studies/Langston_et_al_2022/original/langston_et_al_2022_depthseries.csv") %>% 
  mutate(cs137_unit = "disintegrationsPerMinutePerGram",
         pb210_unit = "disintegrationsPerMinutePerGram",
         pb214_unit = "disintegrationsPerMinutePerGram",
         bi214_unit = "disintegrationsPerMinutePerGram") %>% 
  select(!fraction_carbon)
methods <- read.csv("data/primary_studies/Langston_et_al_2022/original/langston_et_al_2022_methods.csv")
study_citations <- read.csv("data/primary_studies/Langston_et_al_2022/original/langston_et_al_2022_associated_publications.csv") %>% 
  add_row(study_id = "Langston_et_al_2022", 
          bibliography_id = "Langston_et_al_2022_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: The Effect of Marsh Age on Ecosystem Function in a Rapidly Transgressing Marsh",
          author = "Langston, Amy K., Coleman, Daniel J., Jung, Nathalie W., Shawler,  Justin L., Smith, Alexander J., Williams, Bethany L., Wittyngham, Serina S., Chambers, Randolph M., Perry, James E., Kirwan, Matthew L.",
          doi = "10.25573/serc.24913215", 
          url = "https://smithsonian.figshare.com/articles/dataset/Dataset_The_Effect_of_Marsh_Age_on_Ecosystem_Function_in_a_Rapidly_Transgressing_Marsh/24913215", 
          year = 2023,
          month = NA,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          pages = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")


bib_file <- study_citations %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")



## Write files ####
write_csv(cores, "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_cores.csv")
write_csv(depthseries, "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_depthseries.csv")
write_csv(methods, "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_methods.csv")
write_csv(species, "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_species.csv")
write_csv(study_citations, "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_study_citations.csv")
WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Langston_et_al_2022/derivative/langston_et_al_2022_study_citations.bib")
