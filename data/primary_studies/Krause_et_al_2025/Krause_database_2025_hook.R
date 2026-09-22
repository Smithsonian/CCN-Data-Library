## CCN Data Library ####

## Soil core data curation script for Krause seagrass synthesis 2025
## contact: Jaxine Wolfe

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(sf)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

## Read files ####

coredat <- read_xlsx("data/primary_studies/Krause_et_al_2025/original/Seagrass_C_stocks_database.xlsx", 
                     sheet = "Core data", guess_max = 19595)

stocks_data <- read_xlsx("data/primary_studies/Krause_et_al_2025/original/Seagrass_C_stocks_database.xlsx",
                         sheet = "Carbon stocks", guess_max = 19595) %>% 
  rename(`Core ID` = `Additional identification`, `Site name` = `Study site`)

## Identify Duplicates

# Previous Fourqurean synthesis hook
four_cores <- read_csv("data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_cores.csv")
four_bib <- read_csv("data/primary_studies/Fourqurean_2012/derivative/Fourqurean_2012_study_citations.csv") %>% 
  filter(bibliography_id != "Fourqurean_et_al_2012_article")

# nrow(coredat %>% filter(`Article ID` == "CC_012"))
cc_012 <- stocks_data %>% filter(`Article ID` == "CC_012") %>% 
  drop_na(`Core ID`) %>% 
  mutate(article_year = str_extract(`Secondary reference`, "(1|2)\\d{3}"),
         reference = ifelse(!is.na(article_year), gsub(".{2}$", "", `Secondary reference`), `Secondary reference`)) %>%
  distinct(reference, article_year, `Core ID`) %>% 
  count(reference, article_year) %>% 
  arrange(reference) %>% 
  mutate(unpublished = ifelse(grepl("unpublish|Unpublish|submitted|Press|press", reference), T, F),
         study_id = case_when(unpublished == F ~ paste(gsub(",", "", word(reference)), "et_al", article_year, sep = "_"), 
                              T ~ reference))

four_compare <- full_join(cc_012, four_bib) %>% arrange(study_id) # LEFT OFF HERE
# I think they're all the same though (some unvegetated records in the original though)
# have to decide which version to keep
# If we drop the old and bring in this version, we'll have to recreate the study IDs and bibliography linkeages...
# Will have to do this already for the new data IDK

# Via bibliography
ccn_bib <- read_csv("data/CCN_synthesis/CCN_study_citations.csv", guess_max = 1200)

krause_studies <- coredat %>% 
  mutate(reference = coalesce(`Secondary reference`, `Primary reference`)) %>% 
  distinct(Country, `Publication year`, reference) %>% 
  rename(country = Country)

unique(krause_studies$`Primary reference`)


# Via coords

ccn_cores <- read_csv("data/CCN_synthesis/CCN_cores.csv", guess_max = 17000) %>% drop_na(latitude)
  # filter(country %in% c("United States", "Canada", "Mexico")) %>% 
  # filter(longitude < -88) %>% 
  # select(-c(position_notes:pb210_cic_r2))

# ccn_bib <- read_csv("data/CCN_synthesis/CCN_study_citations.csv", guess_max = 1200) %>% 
#   filter(study_id %in% unique(ccn_cores$study_id))

# try a spatial join to isolate duplicate samples
ccn_cores_sf <- ccn_cores %>% 
  filter(habitat == "seagrass") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

krause_cores_sf <- coredat %>% 
  mutate(reference = coalesce(`Secondary reference`, `Primary reference`)) %>% 

    mutate(longitude = as.numeric(gsub("˚E", "", Longitude)), 
           latitude = as.numeric(gsub("˚N", "", Latitude))) %>% 
  drop_na(latitude) %>% 
  distinct(`Article ID`, `Site name`, Country, `Core ID`, `Primary reference`, `Secondary reference`, latitude, longitude) %>% 
  # select(-c(Latitude, Longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

nearest <- st_nearest_feature(krause_cores_sf, ccn_cores_sf)

distances <- krause_cores_sf %>% # need to add source information and plot the resulting pairs to see if they actually align
  # select(`Article ID`, reference, `Publication year`, `Site name`, Country, `Core ID`) %>% 
  mutate(sample_dist = as.vector(st_distance(krause_cores_sf, ccn_cores_sf[nearest,], by_element = T))) %>% 
  bind_cols(ccn_cores_sf[nearest,])

# isolate duplicates
# filter coords pairs that are less than X meter off and pull IDs
duplicates <- distances %>% 
  filter(sample_dist < 4) %>%
  distinct(study_id, core_id, site_id, `Article ID`, `Site name`, `Core ID`, `Primary reference`, `Secondary reference`, sample_dist) 

# isolate new data
new_data <- distances %>% 
  filter(sample_dist > 4) %>%
  distinct(`Article ID`, Country, `Site name`, `Core ID`, `Primary reference`, `Secondary reference`) %>% 
  filter(`Article ID` != "CC_012") # Fourqurean synth needs to be resolved carefully - leave alone for now (keep prev version)
  # pull(StudyID)

# join to stocks table
table_join <- left_join(new_data %>% select(`Article ID`, `Primary reference`, `Secondary reference`, `Site name`, `Core ID`),
                        stocks_data)

# write_csv(new_data, "temp/tamalavage/Krause_new_data_lookup.csv")
# write_csv(table_join, "temp/tamalavage/Krause_new_data_stocks.csv")
# write_csv(duplicates, "temp/tamalavage/Krause_duplicates_lookup.csv")

## Clean new data

novel_coredat <- coredat %>% drop_na(Latitude) %>% # theres some obs with no coordinates
  inner_join(new_data) %>% 
  rename(study_id = `Article ID`,
         site_id = `Site name`,
         core_id = `Core ID`,
         latitude = Latitude,
         longitude = Longitude,
         depth_min = `Top of section`, 
         depth_max = `Bottom of section`, 
         dry_bulk_density = `Dry Bulk Density`) %>% 
  mutate(fraction_organic_matter = `OM content`/100,
         fraction_carbon = case_when(`Primary reference` == "Silva, Copertino, Lanari, 2022 unpublished" ~ NA,
                                     T ~ as.numeric(Corg)/100),
         reference = gsub(".{2}$", "", `Secondary reference`), `Secondary reference`) %>% 
  mutate(longitude = as.numeric(gsub("˚E", "", longitude)), 
         latitude = as.numeric(gsub("˚N", "", latitude))) 
  
## Core-level
cores <- novel_coredat %>% 
  distinct(`Primary reference`, reference, study_id, site_id, core_id, `Publication year`, Country, latitude, longitude, Species, 
           `Depth of core`)

# new_krause_final <- new_krause %>% 
#   select(study_id, site_id, core_id, latitude, longitude, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, fraction_carbon) 

# write_csv(new_krause_final, "temp/tamalavage/Krauss_CCN_format.csv")

# Map the cores with the CCN ones
leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(data = ccn_cores_sf,
                   radius = 0.5, opacity = 0.5,
                   label = ~paste(study_id, core_id, sep = "; "), 
                   group = "CCN") %>%
  
  addCircleMarkers(data = cores, 
                   lat = ~as.numeric(latitude), lng = ~as.numeric(longitude),
                   color = "red", radius = 0.5, opacity = 0.5,
                   group = "Krause",
                   label = ~study_id,
                   # label = studies$study_id[which(studies$StudyID == dont_keep[5])]
  ) %>% 
  addLayersControl(overlayGroups = c("CCN", "Krause"), # "Border"
                   options = layersControlOptions(collapsed = FALSE)
  )


## ... Curate methods ####
# methods <- methods_raw
# methods <- reorderColumns("methods", methods)


## ... Curate depthseries ####
# depthseries <- depthseries_raw %>% 
#   select(-salinity, -pH, -percent_sand, -percent_silt, -percent_clay, -plot_id) 
# 
# depthseries <- reorderColumns("depthseries", depthseries)

new_krause %>% 
  drop_na(fraction_organic_matter, fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter, fraction_carbon, color = study_id)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~`Primary reference`)

new_krause_final %>% 
  drop_na(fraction_organic_matter) %>% 
  ggplot(aes(fraction_organic_matter, group = `Primary reference`)) + 
  geom_density() +
  facet_wrap(~`Primary reference`, scales = "free")

coredat %>% 
  drop_na(Cinorg, Corg) %>% 
  ggplot(aes(Cinorg, as.numeric(Corg))) + 
  geom_point(alpha = 0.5)

## ... Curate cores ####
#pull plot-level locations
# coords <- plot_summary_raw %>% select(site_id, plot_id, plot_center_latitude, plot_center_longitude)
# 
# #curate cores
# cores <- depthseries %>% 
#   select(study_id, site_id, core_id) %>% distinct() %>% 
#   mutate(plot_id = str_sub(core_id, end = -3),
#          habitat = "mangrove",
#          salinity_class = "mesohaline",
#          salinity_method = "measurement",
#          position_method = "other low resolution",
#          position_notes = "position at plot-level",
#          vegetation_class = "forested", #mangroves
#          vegetation_method = "field observation") %>% 
#   full_join(coords) %>% 
#   rename(latitude = plot_center_latitude,
#          longitude = plot_center_longitude) %>% 
#   select(-plot_id)

# cores <- reorderColumns("cores", cores)

## ... Curate impacts ####
# impacts <- cores %>% 
#   select(study_id, site_id, core_id) %>% 
#   mutate(impact_class = ifelse(site_id == "Amanzule","natural", "firewood extraction"))


## ... Curate plots ####
# plots <- plot_summary_raw %>% 
#   mutate(plot_shape = "rectangular",
#          plot_area = "5000", # 125m x 40m 
#          field_or_manipulation_code = "field",
#          soil_core_present = "Yes")
# 

## Write files ####
# write_csv(cores, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_cores.csv")
# write_csv(depthseries, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_depthseries.csv")
# write_csv(methods, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_methods.csv")
# write_csv(impacts, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_impacts.csv")

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

# study_citation_data <- data.frame(study_id = id, 
#                                   bibliography_id = "Adotey_et_al_2024_data",
#                                   publication_type = "primary dataset",
#                                   bibtype = "Misc", 
#                                   title = "Dataset: Carbon Stock Assessment in the Kakum and Amanzule Estuary Mangrove Forests, Ghana",
#                                   author = "Joshua Adotey, Denis Worlanyo Aheto, John Blay, Emmanuel Acheampong",
#                                   doi = "10.25573/serc.25148561",
#                                   url = "https://doi.org/10.25573/serc.25148561.v1",
#                                   year = "2024")
# 
# study_citation_paper <- data.frame(study_id = id, 
#                                    bibliography_id = "Adotey_et_al_2022",
#                                    publication_type = "article",
#                                    bibtype = "Misc", 
#                                    title = "Carbon Stock Assessment in the Kakum and Amanzule Estuary Mangrove Forests, Ghana",
#                                    author = "Joshua Adotey,Emmanuel Acheampong, Denis Worlanyo Aheto, and John Blay",
#                                    doi = " https://doi.org/10.3390/su141912782",
#                                    url = "https://www.mdpi.com/2071-1050/14/19/12782",
#                                    journal = "Sustainability",
#                                    year = "2022")
# 
# study_citations <- full_join(study_citation_data, study_citation_paper)
# 
# WriteBib(as.BibEntry(study_citations), "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_study_citations.csv")
# write_csv(study_citations, "data/primary_studies/Adotey_et_al_2024/derivative/Adotey_et_al_2024_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/






