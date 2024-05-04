## CCN Data Library ####

## Soil core data curation script for Adotey et al 2024
## contact: Henry Betts, BettsH@si.edu

library(tidyverse)
library(RefManageR)


## Read files ####
cores <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_cores.csv")
depthseries <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_depthseries.csv")
methods <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_methods.csv")
plot_summary <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_plot_summary.csv")
plant <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_plant.csv")
study_citations <- read.csv("data/primary_studies/Adotey_et_al_2024/original/adotey_et_al_2024_study_citations.csv") %>% 
  add_row(study_id = "Adotey_et_al_2024", 
          bibliography_id = "Adotey_et_al_2024_data",
          publication_type = "primary dataset", 
          bibtype = "misc", 
          title = "Dataset: Carbon Stock Assessment in the Kakum and Amanzule Estuary Mangrove Forests, Ghana",
          author = "Adotey J, Acheampong E, Aheto DW, Blay J.",
          doi = "", 
          url = "", 
          year = 2024,
          month = NA,
          publisher = "Smithsonian Environmental Research Center",
          volume = NA,
          issue = NA,
          journal = NA,
          copyright = "Creative Commons Attribution 4.0 International")

## Add allometric equations table
allometric_eq <- data.frame(study_id = "Adotey_et_al_2024",
                            location_description = "Ghana",
                            allometric_eq_id = paste("Adotey_et_al_2024", "equation", c(1:8), sep = "_"),
                            allometric_eq_formula = rep(c("Ac = W * f",
                                                      "Bc = W * f",
                                                      "Wtop = .251 * p * DBH^2.46",
                                                      "Wr = .199 * p^.899 * DBH^2.22",
                                                      "Oc = 100 * (A * Nfas * 0.003) / Dw",
                                                      "Sc = B * T * Oc",
                                                      "Tree Density = No. of trees of a species / 0.01 (ha)",
                                                      "TBA = Sum of the basal area for all tree species / Area of sampling plot (m2)"), 3),
                            genus = c(rep("Rhizophora", 8), rep("Avicennia", 8), rep("Langucularia", 8)),
                            species = c(rep("mangle", 8), rep("germinans", 8), rep("racemosa", 8)),
                            alive_or_dead = "alive",
                            above_or_belowground = c("above", "below", "above", "below", "below", "below", NA, NA),
                            height_min = .6,
                            height_max = 18,
                            source_citation = c("Kauffman, J.; Donato, D. Protocols for the measurement, monitoring and reporting of structure, biomass and carbon stocks in mangrove forests. In Center for International Forestry; CIFOR: Bogor, Indonesia, 2012; Available online: http://www.amazonico.org/speclab/SiteAssets/SitePages/Methods/Mangrove-biomass-CIFOR.pdf (accessed on 27 May 2018)",
                                                "Kauffman, J.; Donato, D. Protocols for the measurement, monitoring and reporting of structure, biomass and carbon stocks in mangrove forests. In Center for International Forestry; CIFOR: Bogor, Indonesia, 2012; Available online: http://www.amazonico.org/speclab/SiteAssets/SitePages/Methods/Mangrove-biomass-CIFOR.pdf (accessed on 27 May 2018)",
                                                "Komiyama, A.; Poungparn, S.; Kato, S. Common allometric equations for estimating the tree weight of mangroves. J. Trop. Ecol. 2005, 21, 471–477",
                                                "Komiyama, A.; Poungparn, S.; Kato, S. Common allometric equations for estimating the tree weight of mangroves. J. Trop. Ecol. 2005, 21, 471–477",
                                                "Nelson, D.W.; Sommers, L.E. A rapid and accurate method for estimating organic carbon in soil. Proc. Indiana Acad. Sci. 1975, 84, 456–562.",
                                                "Adotey J, Acheampong E, Aheto DW, Blay J. Carbon Stocks Assessment in a Disturbed and Undisturbed Mangrove Forest in Ghana. Sustainability. 2022; 14(19):12782. https://doi.org/10.3390/su141912782",
                                                "Adotey J, Acheampong E, Aheto DW, Blay J. Carbon Stocks Assessment in a Disturbed and Undisturbed Mangrove Forest in Ghana. Sustainability. 2022; 14(19):12782. https://doi.org/10.3390/su141912782",
                                                "Adotey J, Acheampong E, Aheto DW, Blay J. Carbon Stocks Assessment in a Disturbed and Undisturbed Mangrove Forest in Ghana. Sustainability. 2022; 14(19):12782. https://doi.org/10.3390/su141912782"))


## Write files ####
write_csv(cores, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_cores.csv")
write_csv(allometric_eq, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_allometric_eq.csv")
write_csv(depthseries, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_depthseries.csv")
write_csv(methods, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_methods.csv")
write_csv(plot_summary, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_plot_summary.csv")
write_csv(plant, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_plant.csv")
write_csv(study_citations, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_study_citations.csv")

