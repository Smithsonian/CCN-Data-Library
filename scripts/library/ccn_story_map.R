## NOAA-CCN Story Map Feature 
#contact: Rose Cheney, cheneyr@si.edu

#script to pull coordinates and make a table of relevant studies to the NOAA BCI project 

library(dplyr)
library(readr)
library(leaflet)


#read in data, current synthesis, pre BCI cores
synthesis_version <- read_csv("docs/synthesis_resources/synthesis_history.csv")
preproject_cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/3d4d2e65dbcb61a88cb2999f229bcaaf4a0612a3/data/CCRCN_synthesis/original/CCRCN_cores.csv")
current_cores <- read_csv("data/CCN_synthesis/CCN_cores.csv")


#create list of NOAA target countries
targets <- c("Bangladesh","Brazil","Cambodia","Cameroon","Colombia",
             "Costa Rica","Dominican Republic","East Timor","Ecuador", 
             "Federated States of Micronesia","Fiji","Ghana","India", 
             "Indonesia","Kiribati","Malaysia","Maldives","Marshall Islands",
             "Mexico","Nauru","Palau","Papua New Guinea","Philippines","Samoa", 
             "Senegal","Solomon Islands","South Africa","Sri Lanka","Thailand",
             "Tonga","Tuvalu","Uganda","Vanuatu","Vietnam")


####.... filter current and pre-project cores ####


#filter pre-project cores (2021 V 0.6.0)
#no country labels, use maps package to get country from coordinates
# NO admin division in V 0.6.0
library(maps)

#get country labels, filter by targets 
preBCI <- preproject_cores %>% 
  mutate(country = map.where(database = "world", longitude,latitude)) %>% 
  filter(country %in% targets) %>% 
  select(study_id, site_id, core_id, latitude, longitude, year, habitat, country)


#filter current cores (2024 V3.0.0)
current <- current_cores %>% filter(current_cores$country %in% targets) %>% 
  select(study_id, site_id, core_id, latitude, longitude, year, habitat, country, admin_division)
  

#get list of studies -- 186 in current version 
studies_new <- current %>% select(study_id) %>% distinct()
studies_old <- preBCI %>% select(study_id) %>% distinct()



#get bib lists to pull primary associated publications
bibs_new <- read_csv("data/CCN_synthesis/CCN_study_citations.csv")
#bibs_old <- read_csv("") is there an accessible bibliography list from pre-1.0.0?

bib_list_new <- bibs_new %>% semi_join(studies_new) %>% 
  filter(grepl("primary", publication_type))


#clean up tables, remove underscores from study_id 
currentBCI <- current %>% 
  mutate(study_id = gsub("_", " ", study_id))

preBCI <- preBCI %>% 
  mutate(study_id = gsub("_", " ", study_id))



####.... get cores added per country #### 
#include core count, n studies, anything else? 

#anti join current core list and preBCI core list 
cores_added <- anti_join(currentBCI, preBCI, by = "core_id")

new_core_count <- cores_added %>% count(country) %>% 
  rename(new_cores_added = n)


#From V 0.6.0 to V 3.0.0 there were 4,028 cores added from 20 NOAA target countries






####... write tables to csv ####


#current relevant cores and citations
write_csv(currentBCI, "")
#write_csv(bib_list_current, "")


#pre-project relevant cores and citations
write_csv(preBCI,"")
#write_csv(,"")


#cores added per country (throughout BCI project)
write_csv(new_core_count,"")









