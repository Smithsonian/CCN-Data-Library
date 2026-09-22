## CCN Data Library ####

## Soil core data curation script for Adotey et al 2024
## contact: Rose Cheney, cheneyr@si.edu

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


#assign study id
id <- "Adotey_et_al_2024"

## Read files ####

methods_raw <- read.csv("data/primary_studies/Adotey_et_al_2024/original/Adotey_et_al_2024_methods.csv")
depthseries_raw <- read.csv("data/primary_studies/Adotey_et_al_2024/original/Adotey_et_al_2024_depthseries.csv")

plot_summary_raw <- read.csv("data/primary_studies/Adotey_et_al_2024/original/Adotey_et_al_2024_plots.csv")
plant_raw <- read.csv("data/primary_studies/Adotey_et_al_2024/original/Adotey_et_al_2024_plants.csv")

## ... Curate methods ####
methods <- methods_raw
methods <- reorderColumns("methods", methods)


## ... Curate depthseries ####
depthseries <- depthseries_raw %>% 
  select(-salinity, -pH, -percent_sand, -percent_silt, -percent_clay, -plot_id) 

depthseries <- reorderColumns("depthseries", depthseries)



## ... Curate cores ####
#pull plot-level locations
coords <- plot_summary_raw %>% select(site_id, plot_id, plot_center_latitude, plot_center_longitude)

#curate cores
cores <- depthseries %>% 
  select(study_id, site_id, core_id) %>% distinct() %>% 
  mutate(plot_id = str_sub(core_id, end = -3),
         habitat = "mangrove",
         salinity_class = "mesohaline",
         salinity_method = "measurement",
         position_method = "other low resolution",
         position_notes = "position at plot-level",
         vegetation_class = "forested", #mangroves
         vegetation_method = "field observation") %>% 
  full_join(coords) %>% 
  rename(latitude = plot_center_latitude,
         longitude = plot_center_longitude) %>% 
  select(-plot_id)

cores <- reorderColumns("cores", cores)


#notes - include impacts table?
# Kakum forest - unprotected, fuel wood and other degradation
# Amanzule forest - protected lands 


## ... Curate impacts ####
impacts <- cores %>% 
  select(study_id, site_id, core_id) %>% 
  mutate(impact_class = ifelse(site_id == "Amanzule","natural", "firewood extraction"))


## ... Curate plots ####
plots <- plot_summary_raw %>% 
  mutate(plot_shape = "rectangular",
         plot_area = "5000", # 125m x 40m 
         field_or_manipulation_code = "field",
         soil_core_present = "Yes")

  

## ... Curate plants ####
plants <- plant_raw %>% 
  mutate(n_plants = 1, #each observation represents a single plant 
         height_unit = "meter")
  

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
write_csv(depthseries, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_depthseries.csv")
write_csv(methods, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_methods.csv")
write_csv(impacts, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_impacts.csv")

#write_csv(plots, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_plot_summary.csv")
#write_csv(plants, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_plant.csv")
#write_csv(allometric_eq, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_allometric_eq.csv")


## 4. Bibliography ####

# There are three ways to approach this:
# 1) download the article citation directly to the study's folder
# 2) create the study citation in the curation script and output it to the data release folder
# 3) create a study_citation table in an intermediate folder, read it in and output bib file to derivative folder

# example study citation creation:
# study_citation <- data.frame(bibliography_id = "Spera_et_al_2020",
#                              title = "Spatial and temporal changes to a hydrologically-reconnected coastal wetland: Implications for restoration",
#                              author = "Alina C. Spera and John R. White and Ron Corstanje",
#                              bibtype = "Article",
#                              doi = "10.1016/j.ecss.2020.106728",
#                              url = "https://doi.org/10.1016/j.ecss.2020.106728", 
#                              journal = "Estuarine, Coastal and Shelf Science",
#                              year = "2020") %>% 
#     column_to_rownames("bibliography_id")
# 
# WriteBib(as.BibEntry(study_citation), "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_associated_publications.bib")

study_citation_data <- data.frame(study_id = id, 
                             bibliography_id = "Adotey_et_al_2024_data",
                             publication_type = "primary dataset",
                             bibtype = "Misc", 
                             title = "Dataset: Carbon Stock Assessment in the Kakum and Amanzule Estuary Mangrove Forests, Ghana",
                             author = "Joshua Adotey, Denis Worlanyo Aheto, John Blay, Emmanuel Acheampong",
                             doi = "10.25573/serc.25148561",
                             url = "https://doi.org/10.25573/serc.25148561.v1",
                             year = "2024")

study_citation_paper <- data.frame(study_id = id, 
                                   bibliography_id = "Adotey_et_al_2022",
                                   publication_type = "article",
                                   bibtype = "Misc", 
                                   title = "Carbon Stock Assessment in the Kakum and Amanzule Estuary Mangrove Forests, Ghana",
                                   author = "Joshua Adotey,Emmanuel Acheampong, Denis Worlanyo Aheto, and John Blay",
                                   doi = " https://doi.org/10.3390/su141912782",
                                   url = "https://www.mdpi.com/2071-1050/14/19/12782",
                                   journal = "Sustainability",
                                   year = "2022")

study_citations <- full_join(study_citation_data, study_citation_paper)

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_study_citations.csv")
write_csv(study_citations, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/






