## CCN Data Library ########

## Soil core data curation script for 'High resolution carbon stock and soil data for three salt marshes along the northeastern coast of North America' (dataset),
  ## Lee B. van Ardenne et al 2018
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://doi.org/10.1016/j.dib.2018.07.037
## Associated paper: https://www.sciencedirect.com/science/article/pii/S0016706117317172?via%3Dihub

#######   ~NOTES~
## Vegetation was recorded within an ~0.5 m radius around each site where a core was collected,
# or depth recorded. Location of each sample site was recorded using a differential GPS.

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(sf)
library(proj4)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCCN-Community-Resources/soil_carbon_guidance.html


#load in data 
data_raw <- read_xlsx("data/primary_studies/van_Ardenne_et_al_2018/original/DiB_Marsh_Soils.xlsx")

#load shapefiles for gps points 
wells_spit <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Wells_Int_GPS.shp", stringsAsFactors = FALSE)
wells_int <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Wells_Int_GPS.shp")
point_carron <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Point_Carron_GPS.shp")
grants_beach <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Grants_Beach_GPS.shp")


## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "van_Ardenne_et_al_2018"


## ... Methods ####

methods <- data.frame(method_id = 1, coring_method = "russian corer") 
methods [2,] <- list(2, "gouge auger")

methods <- methods %>% mutate(study_id = id,
                      roots_flag = "roots and rhizomes included",
                      compaction_flag = "corer limits compaction",
                      dry_bulk_density_temperature = "60",
                      dry_bulk_density_flag = "to constant mass",
                      loss_on_ignition_temperature = 550,
                      loss_on_ignition_time = 4,
                      carbon_measured_or_modeled = "modeled",
                      fraction_carbon_method = "Craft regression",
                      fraction_carbon_type = "organic carbon",
                      carbonates_removed = "FALSE",  #organic carbon calculated from loi via craft regression 
                      carbonate_removal_method = "none specified",
                      carbon_profile_notes = "To determine organic matter, samples were combusted at 350°C for 1h followed by 4h at 550°C.")



#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites #### 


## ... Cores ####
cores <- data_raw %>% select(Site, Site_Num, Transect, Flag) %>% 
                      mutate(study_id = id,
                             core_id = paste(Site, Transect, sep = "_"),
                             core_id = paste(core_id, Flag, sep = ""),
                             position_method = "other high resolution",
                             position_notes = "Lecia Viva differential GPS",
                             core_length_flag = "core depth represents deposit depth",
                             salinity_class = "estuarine",
                             salinity_method = "field observation",
                             vegetation_class = "emergent",
                             vegetation_method = "field observation") %>% 
                     rename(site_id = Site) %>% 
                     select(- Site_Num, -Transect, -Flag) %>% distinct()


#transform points into lat long (reference = https://spatialreference.org/)
proj4_CA <- "+proj=sterea +lat_0=46.5 +lon_0=-66.5 +k=0.999912 +x_0=2500000 +y_0=7500000 +ellps=GRS80 +units=m +no_defs"
proj4_wells <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#transform each site to lat long 
lon_lat_gb <- project(grants_beach[,4:5], proj4_CA, inverse = TRUE)
lon_lat_pt <- project(point_carron[,3:4], proj4_CA, inverse = TRUE)
lon_lat_wells_int <- project(wells_int[,3:4], proj4_wells, inverse = TRUE)
lon_lat_wells_spit <- project(wells_spit[,3:4], proj4_wells, inverse = TRUE)

#grants beach cores
grants_beach_coords <- data.frame(longitude = lon_lat_gb$x, latitude = lon_lat_gb$y) %>% cbind(grants_beach)
grants_beach <- grants_beach_coords %>% mutate(core_id = paste("Grant's Beach", Transect, sep = "_"),
                                               core_id = paste(core_id, Flag, sep = "")) %>% 
                                              dplyr::select(longitude, latitude, core_id) 

#point carron cores
point_carron_coords <- data.frame(longitude = lon_lat_pt$x, latitude = lon_lat_pt$y) %>% 
                      cbind(point_carron)
point_carron <- point_carron_coords %>% separate(Core,c('NA','Transect', 'Flag')) %>% 
                                        mutate(core_id = paste("Pt Carron",Transect, sep = "_"),
                                               core_id = paste(core_id, Flag, sep = "")) %>% #Flags/transects do not match to core table 
                dplyr::select(longitude, latitude, core_id) 


#wells cores
wells_int_coords <- data.frame(longitude = lon_lat_wells_int$x, latitude = lon_lat_wells_int$y) %>% 
                    cbind(wells_int) %>% `[`(-c(44,45),)   #remove duplicate A8 and A9, rows 44 and 45 
wells_int <- wells_int_coords %>% mutate(core_id = paste("Wells", Transect, sep = "_"),
                                  core_id = paste(core_id, Flag_Numbe, sep = "")) %>% 
                                  mutate(core_id = recode(core_id,"Wells_A17" = "Wells_A17X-3")) %>% #rename to match 
                                  dplyr::select(longitude, latitude, core_id) 


#wells spit cores
wells_spit_coords <- data.frame(longitude = lon_lat_wells_spit$x, latitude = lon_lat_wells_spit$y) %>% cbind(wells_spit)
wells_spit <- wells_spit_coords %>% mutate(core_id = paste("Wells_spit", Transect, sep = "_"),
                                         core_id = paste(core_id, Flag_Numbe, sep = "")) %>% 
                             dplyr::select(longitude, latitude, core_id) 


#JOIN ALL to cores table 
latlong <- bind_rows(grants_beach, point_carron, wells_int, wells_spit) 
cores <- left_join(cores, latlong) 

#fix core coding issues 
cores <- cores %>% mutate(latitude = case_when(core_id == "Grant's Beach_AO10X2" ~ 46.17300, TRUE ~ latitude),
                          longitude = case_when(core_id == "Grant's Beach_AO10X2" ~ -64.04982,
                                                TRUE ~ longitude)) %>% 
                   recode(cores,"Wells_A17X-3" = "Wells_A17X3")
  
                        
cores <- reorderColumns("cores", cores)

## ... Depthseries #### 

depthseries <- data_raw %>% select(Site, Transect, Flag, Corer, `upper depth (cm)` , `lower depth (cm)`,`Bulk Density (g/cc)`,
                                   `Organic matter proportion`, `Organic Carbon Craft (%)`) %>% 
                            mutate(study_id = id,
                                   method_id = case_when(Corer == "60 mm" ~ 2,
                                                                     Corer == "25 mm" ~ 1, 
                                                                     TRUE ~ 1),
                                   core_id = paste(Site, Transect, sep = "_"),
                                   core_id = paste(core_id, Flag, sep = ""),
                                   fraction_carbon = `Organic Carbon Craft (%)`/100) %>% 
                            rename(site_id = Site,
                                   depth_min = `upper depth (cm)`,
                                   depth_max = `lower depth (cm)`,
                                   dry_bulk_density = `Bulk Density (g/cc)`,
                                   fraction_organic_matter = 'Organic matter proportion') %>% 
                            select(-Transect, -Corer, -Flag, - 'Organic Carbon Craft (%)')
      
#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

#species data from proportion of roots/rhizomes in cores, detailed in paper 
species <- data_raw %>% select(Site, `S. alt.`, `S. pat.`, `J. ger.`, Triglochin, 
                               Plantago, Carex, Salicornia, Phrag, unknown,
                               Distichlis, Limonnium, Transect, Flag, `lower depth (cm)`) %>% 
                        subset(!`lower depth (cm)`> 5) %>%  #subset to the first depth sample in each core 
                        pivot_longer(!c(Site, Transect, Flag, `lower depth (cm)`), names_to = "species_code", values_to = "proportion", values_drop_na = TRUE) %>% 
                        mutate(study_id = id,
                               habitat = "marsh",
                               core_id = paste(Site, Transect, sep = "_"),
                               core_id = paste(core_id, Flag, sep = "")) %>% 
                        rename(site_id = Site) %>% 
                        select(-Transect, -Flag, -`lower depth (cm)`) %>% subset(!proportion < 50) %>% 
                        mutate(species_code = case_when(species_code == "S. pat." ~ "Spartina patens",
                                                        species_code == "J. ger." ~ "Juncus gerardii",
                                                        species_code == "S. alt." ~ "Spartina alterniflora",
                                                        TRUE ~ species_code)) %>% 
                        mutate(code_type = case_when(species_code == "Triglochin" ~ "Genus",
                                                     TRUE ~ "Genus species")) %>% select(-proportion)

species <- reorderColumns("species", species)                      
## ... Impacts ####

# if provided, curation table of anthropogenic impacts

## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
testRequired(table_names)
testConditional(table_names)

# test uniqueness
testUniqueCores(cores)
testUniqueCoords(cores)

# test relational structure of data tables
testIDs(cores, depthseries, by = "site")
testIDs(cores, depthseries, by = "site")

# test numeric attribute ranges
fractionNotPercent(depthseries)
  #testNumericCols(depthseries) function not working 
test_numeric_vars(depthseries) 

## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018_methods.csv")
  #write_csv(sites, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018_cores.csv")
write_csv(depthseries, "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018_depthseries.csv")
write_csv(species, "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018_species.csv")
  #write_csv(impacts, "data/primary_studies/Author_et_al_YYYY/derivative/Author_et_al_YYYY_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "van_Ardenne_et_al_2018_dataset",
                             title = "High resolution carbon stock and soil data for three salt marshes along the northeastern coast of North America",
                             author = "Lee B. van Ardenne, Serge Jolicoeur, Dominique Bérubé, David Burdick, Gail L. Chmura",
                             bibtype = "Misc", 
                             publication_type = "primary dataset",
                             doi = "https://doi.org/10.1016/j.dib.2018.07.037",
                             url = "https://www.sciencedirect.com/science/article/pii/S2352340918308102#ec0010",
                             year = "2018") %>% 
                  column_to_rownames("bibliography_id")

study_citation_article <- data.frame(bibliography_id = "van_Ardenne_et_al_2018_article",
                                     title = "The importance of geomorphic context for estimating the carbon stock of salt marshes",
                                     author = "Lee B. van Ardenne, Serge Jolicoeur, Dominique Bérubé, David Burdick, Gail L. Chmura",
                                     bibtype = "Article",
                                     doi = "https://doi.org/10.1016/j.geoderma.2018.06.003",
                                     url = "https://www.sciencedirect.com/science/article/pii/S0016706117317172",
                                     journal = "Geoderma",
                                     publication_type = "associated source",
                                     year = "2018") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("van_Ardenne_et_al_2018", "van_Ardenne_et_al_2018"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018.bib")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
