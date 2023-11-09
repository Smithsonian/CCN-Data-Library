## CCN Data Library ########

## Soil core data curation script for 'High resolution carbon stock and soil data for three salt marshes along the northeastern coast of North America' (dataset),
  ## Lee B. van Ardenne et al 2018
## contact: Rose Cheney, cheneyr@si.edu 

## Notes about the dataset 
## Dataset: https://doi.org/10.1016/j.dib.2018.07.037
## Associated paper: https://www.sciencedirect.com/science/article/pii/S0016706117317172?via%3Dihub

## NOTES
## Vegetation was recorded within an ~0.5 m radius around each site where a core was collected, or depth recorded. 


# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)
library(sf)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCCN-Community-Resources/soil_carbon_guidance.html


#load in data 
data_raw <- read_xlsx("data/primary_studies/van_Ardenne_et_al_2018/original/DiB_Marsh_Soils.xlsx")

#position data 
wells_spit <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Wells_Spit_GPS.shp")
wells_int <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Wells_Int_GPS.shp")
point_carron <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Point_Carron_GPS.shp")
grants_beach <- st_read("data/primary_studies/van_Ardenne_et_al_2018/original/1-s2.0-S2352340918308102-mmc2/Grants_Beach_GPS.shp")


## 1. Curation ####

# this study ID must match the name of the dataset folder
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
sites <- data_raw %>% select(Site)



## ... Cores ####
cores <- data_raw %>% select(Site, Transect, Flag) %>% 
                      mutate(study_id = id,
                             core_id = paste(Site, Transect, sep = "_"),
                             core_id = paste(core_id, Flag, sep = "_")) %>% 
                     rename(site_id = Site) %>% distinct()


##function for extracting coords from shp files
extractCoords <- function(x){
  # convert coord reference system to WGS 1984 (EPSG:4326)
  df_4326 <- x %>% st_transform(4326)
  
  extract_coords <- x %>% 
    # select(Flag_Numbe, Transect, Flag_Trans) %>% # can't include this since colnames can differ, clean this up after
    st_drop_geometry() %>%  # convert to data frame
    bind_cols(as.data.frame(st_coordinates(df_4326))) # extract transformed coordinates to this table 
  
  return(extract_coords)
}

#extract coords
## "Core names which do not have an analogue in the core data table are sites where only depth was measured."
wells_int_coords <- extractCoords(wells_int)
wells_spit_coords <- extractCoords(wells_spit)
gb_coords <- extractCoords(grants_beach)
pt_coords <- extractCoords(point_carron)

#grants beach 
grants_beach <- gb_coords %>% rename(latitude = Y,
                                     longitude = X) %>% 
                              mutate(site_id = "Grant's Beach",
                                     Flag = case_when(Core == "FLAG-W-30" ~ "W30",
                                                      Core == "FLAG-W-35"~ "W35", TRUE ~ Flag),
                                     Transect = case_when(Flag == "W30" ~ "E", 
                                                          Flag == "W35" ~ "G", TRUE ~ Transect)) %>% 
                              filter(!is.na(Flag)) %>% 
                              select(site_id, Flag, Transect, longitude, latitude) #missing core -- 	C_W7 (only depth/dbd data included)
                                              
#point carron 
pt_stock <- data_raw %>% select(Site, Transect, Flag, `Core Carbon Stock (g/sqm)`) %>% 
                         filter(Site == "Pt Carron") %>% filter(!is.na(`Core Carbon Stock (g/sqm)`)) %>% 
                         rename("stock" = "Core Carbon Stock (g/sqm)","site_id" = "Site") %>% 
                         select(site_id, Transect, Flag, stock)
              #match core c stock to id in shp file  --> missing core B40
pt_coords <- pt_coords %>% rename(latitude = Y...9, longitude = X...8, 
                                     stock = Carbon_Sto) %>% mutate(site_id = "Pt Carron") %>% 
                           separate(Core, c(NA, "Transect", "Flag")) 

point_carron <- left_join(pt_coords, pt_stock) %>% 
                        mutate(Flag = recode(Flag, "05" = "5"),
                              Transect = case_when(Flag == "30"|Flag == "32"|Flag == "33"|Flag == "35"|Flag == "40" ~ "B",
                                                   Flag == "24" & Transect == "P" ~ "A",
                                                   Flag == "11" & Transect == "O" ~ "A",
                                                   Flag == "17" & Transect == "O" ~ "A",
                                                   Flag == "20"|Flag == "21"|Flag == "5" ~ "A")) %>% 
                             filter(!is.na(Transect)) %>% select(site_id, Flag, Transect, longitude, latitude) 


#wells 
wells_int <- wells_int_coords %>% rename(latitude = Y...12,
                                         longitude = X...11,
                                         Flag = Flag_Numbe) %>% 
                                  mutate(site_id = "Wells",
                                         Flag = recode(Flag,"17"="17X-3"),
                                         Flag = case_when(Flag_Trans == "TRANSECT_A_4_CORE_9" ~ "4",
                                                          Flag_Trans == "TRANSECT_A_3_CORE_8" ~ "3",
                                                          Flag_Trans == "TRANSECT_A_7_CORE_10" ~ "7",
                                                          Flag_Trans == "TRANSECT_A_10" ~ "10",
                                                          TRUE ~ Flag)) %>% filter(!is.na(Flag)) %>% 
                                  select(site_id, Flag, Transect, longitude, latitude) # missing cores A1, A2

#wells spit 
wells_spit <- wells_spit_coords %>% rename(latitude = Y...12,
                                    longitude = X...11,
                                    Flag = Flag_Numbe) %>% 
                                    mutate(site_id = "Wells spit",
                                           Flag = case_when(Flag_Trans == "CORE_B_6" ~ "6",
                                                            Flag_Trans == "TRANSECT_B_21_CORE_11" ~ "11",
                                                            TRUE ~ Flag)) %>%   
                                   select(site_id, Transect, Flag, longitude, latitude)

#JOIN ALL LAT/LONG to cores table 
latlong <- bind_rows(grants_beach, point_carron, wells_int, wells_spit) 
cores <- left_join(cores, latlong) %>% distinct()

#add additional variables
cores <- cores %>% mutate(position_method = case_when(is.na(latitude) ~ "other low resolution",
                                                      TRUE ~ "other high resolution"),
                          position_notes = case_when(is.na(latitude) ~ "position at site level",
                                                     TRUE ~ "Lecia Viva differential GPS"),
                          year = "2015",
                          core_length_flag = "core depth represents deposit depth",
                          salinity_class = "estuarine",
                          salinity_method = "field observation",
                          vegetation_class = "emergent",
                          vegetation_method = "field observation",
                          habitat = "marsh",
                          inundation_class = "low",
                          inundation_method = "field observation",
                          latitude = case_when(core_id == "Grant's Beach_A_O10X2" ~ 46.17301, 
                                               site_id == "Wells"& is.na(latitude) ~ 43.3,
                                               site_id == "Grant's Beach"& is.na(latitude) ~ 46.166667,
                                               site_id == "Pt Carron"& is.na(latitude) ~ 47.65,
                                               TRUE ~ latitude),
                          longitude = case_when(core_id == "Grant's Beach_A_O10X2" ~ -64.04982,
                                                site_id == "Wells"& is.na(longitude) ~ 70.566667,
                                                site_id == "Pt Carron"& is.na(longitude) ~ 65.6,
                                                site_id == "Grant's Beach"& is.na(longitude) ~ 64.05,
                                                TRUE ~ longitude)) %>% select(-Transect, -Flag)
                        
cores <- reorderColumns("cores", cores)

## ... Depthseries #### 

depthseries <- data_raw %>% select(Site, Transect, Flag, Corer, `upper depth (cm)` , `lower depth (cm)`,`Bulk Density (g/cc)`,
                                   `Organic matter proportion`, `Organic Carbon Craft (%)`) %>% 
                            mutate(study_id = id,
                                   method_id = case_when(Corer == "60 mm" ~ 2,
                                                                     Corer == "25 mm" ~ 1, 
                                                                     TRUE ~ 1),
                                   core_id = paste(Site, Transect, Flag, sep = "_"),
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

#merge and write bib             
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("van_Ardenne_et_al_2018_dataset", "van_Ardenne_et_al_2018_article"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

# create bib
van_arden_bib <- study_citations %>% select(-study_id, -publication_type) %>%   
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(van_arden_bib), "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018.bib")
write_csv(study_citations, "data/primary_studies/van_Ardenne_et_al_2018/derivative/van_Ardenne_et_al_2018_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
