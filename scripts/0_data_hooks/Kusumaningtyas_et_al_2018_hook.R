## CCN Data Library ########

## Soil core data curation script for Kusumaningtyas, Mariska Astrid et al (2018):Carbon, nitrogen and stable carbon isotopes, and radionuclides 
## in sediment cores from Segara Anakan Lagoon, Berau and Kongsi Island, Indonesia, 2013 and 2016.
## Sediment characteristics of the mangrove forest of Bonaire, Dutch Caribbean. PANGAEA, https://doi.org/10.1594/PANGAEA.910431,
## contact: Rose Cheney, cheneyr@si.edu 


## Dataset: https://doi.pangaea.de/10.1594/PANGAEA.896852
## Associated paper: https://doi.org/10.1016/j.ecss.2018.12.007
# data published through PANGEA 


# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(leaflet)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC


# link to database guidance for easy reference:
# https://smithsonian.github.io/CCCN-Community-Resources/soil_carbon_guidance.html


#load in data 
#separate data sheet for each core (n=18), skipping additional metadata rows 
SAL_E16 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_East-E16_C-N-stable-isotopes.xlsx", skip = 28)
SAL_E40 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_East-E40_C-N-stable-isotopes.xlsx", skip = 40)
SAL_E44 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_East-E44_C-N-stable-isotopes.xlsx", skip = 28)
SAL_E46 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_East-E46_C-N-stable-isotopes.xlsx", skip = 28)
SAL_E47 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_East-E47_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C49 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C49_C-N-stable-isotopes.xlsx",skip = 28)
SAL_C45 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C43_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C43 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C43_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C42 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C42_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C41 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C41_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C26 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C26_C-N-stable-isotopes.xlsx", skip = 28)
SAL_C24 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Segara_Anakan_Center-C24_C-N-stable-isotopes.xlsx", skip = 38)
B1 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Berau-B1_C-N-stable-isotopes.xlsx", skip = 28)
B2 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Berau-B2_C-N-stable-isotopes.xlsx", skip = 38)
B3 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Berau-B3_C-N-stable-isotopes.xlsx", skip = 28)
B4 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Berau-B4_C-N-stable-isotopes.xlsx", skip = 28)
K1 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Kongsi_island-K1_C-N-stable-isotopes.xlsx", skip = 28)
K2 <- read_xlsx("data/primary_studies/Kusumaningtyas_et_al_2018/original/Kongsi_island-K2_C-N-stable-isotopes.xlsx", skip = 28)

## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Kusumaningtyas_et_al_2018"



## ... Methods ####

methods <- data.frame(study_id = id,
                      method_id = "single set of methods",
                      coring_method = "gouge auger",
                      roots_flag = "roots and rhizomes separated",
                      sediment_sieved_flag = "sediment not sieved",
                      compaction_flag = "not specified",
                      dry_bulk_density_temperature = 60,
                      dry_bulk_density_flag = "time approximate",
                      carbon_profile_notes = "dry bulk density time from 48-72 hours",
                      loss_on_ignition_flag = "not specified", 
                      carbon_measured_or_modeled = "measured",
                      carbonates_removed = TRUE,
                      carbonate_removal_method = "direct acid treatment",
                      fraction_carbon_method = "EA",
                      fraction_carbon_type = "organic carbon",
                      cs137_counting_method = "gamma",
                      pb210_counting_method = "gamma",
                      excess_pb210_rate = "cumulative mass",
                      excess_pb210_model = "CFCS") 

#reorder columns 
methods <- reorderColumns("methods", methods)


## ... Sites ####

## ... Cores ####

#function to pull out needed variables for core table
 select_core <- function(df, core, lat, lon){
  df %>% 
    mutate(core_id = core,  #create core id
           latitude = lat,
           longitude = lon) %>% 
    select(core_id, latitude, longitude) %>% distinct() #select variables needed for core table
}

## Berau 
B1_core <- select_core(B1,"B1", lat = 2.423470, lon = 118.000720)
B2_core <- select_core(B2,"B2", lat = 2.398250, lon = 118.013720)
B3_core <- select_core(B3,"B3", lat = 2.300000, lon = 118.089000)
B4_core <- select_core(B4,"B4", lat = 2.191190, lon= 117.98017)

berau <- rbind(B1_core, B2_core, B3_core, B4_core) %>% 
  mutate(site_id = "Berau",
         year = 2013, month = 05, 
         day = case_when(core_id == "B1"| core_id == "B2" ~ 24,
                         core_id == "B3" ~ 25,
                         TRUE ~ 26))


## Kongsi Island 
K1_core <- select_core(K1, "K1", lat = -5.855780, lon = 106.600970)
K2_core <- select_core(K2, "K2", lat = -5.857690, lon = 106.601750)

kongsi <- rbind(K1_core, K2_core) %>% mutate(site_id = "Kongsi_Island",
                                             year = 2016,
                                             month = 12,
                                             day = 26)

## Segara anakan lagoon
C24_core <- select_core(SAL_C24, "C24", lat = -7.658580 , lon = 108.839440) 
C26_core <- select_core(SAL_C26, "C26", lat = -7.677470, lon = 108.841110)
C41_core <- select_core(SAL_C41, "C41", lat = -7.691030, lon = 108.846530)
C42_core <- select_core(SAL_C42, "C42", lat = -7.679640, lon = 108.818890) 
C43_core <- select_core(SAL_C43, "C43", lat = -7.709690, lon = 108.883690)
C45_core <- select_core(SAL_C45, "C45", lat = -7.667310, lon = 108.866860) 
C49_core <- select_core(SAL_C49, "C49", lat = -7.691030, lon = 108.846530) 
E16_core <- select_core(SAL_E16, "E16", lat = -7.729610, lon = 108.986250) 
E40_core <- select_core(SAL_E40, "E40", lat = -7.675500, lon = 109.000500) 
E44_core <- select_core(SAL_E44, "E44", lat = -7.711250, lon = 108.929920) 
E46_core <- select_core(SAL_E46, "E46", lat = -7.696530, lon = 108.951780) 
E47_core <- select_core(SAL_E47, "E47", lat = -7.718310, lon = 108.950580)

SAL <- rbind(C24_core, C26_core, C41_core, C42_core, C43_core, C45_core, C49_core,
             E16_core, E40_core, E44_core, E46_core, E47_core) %>% 
      mutate(site_id = case_when(str_detect(core_id, "C") ~ "Segara_Anakan_Lagoon_Central",
                                 TRUE ~ "Segara_Anakan_Lagoon_East"),
             year = 2016,
             day = c(26, 11, 27, 26, 12, 11,
                     27, 11, 04 ,04 ,03 ,03),
             month = case_when(str_detect(core_id, "E") ~ 12,
                               core_id == "C24"|core_id == "C41"|
                               core_id == "C42"| core_id == "C49" ~ 11, 
                               TRUE ~ 12))

#join tables and reformat 
cores <- rbind(berau, kongsi, SAL) %>% 
            mutate(study_id = id, 
                   habitat = "mangrove",
                   position_method = NA,
                   core_length_flag = "core depth limited by length of corer",
                   vegetation_method = "field observation",
                   salinity_method = "field observation",
                   salinity_class = case_when(site_id == "Kongsi_Island" ~ "saline",
                                              TRUE ~ "estuarine"),
                   vegetation_class = case_when(str_detect(site_id, "Segara") ~ "forested to shrub",
                                                TRUE ~ "forested"))

cores <- reorderColumns("cores", cores)


## ... Depthseries #### 

# format individual cores for depthseries table 
format_depth <- function(df, core) {
  df %>% 
    mutate(core_id = core,  #create core id
           fraction_carbon = `TOC [%]`/100) %>%
    rename(depth_min = `Depth top [m]`,
           depth_max = `Depth bot [m]`,
           dry_bulk_density = `DBD [g/cm**3]`,
           delta_c13 = `Œ¥13C Corg [‚Ä∞ PDB]`) %>% 
    select(-`Depth [m]`, -`C/N`, - `TOC [%]`, - `TN [%]`)
}

# format dated cores for depthseries table 
format_depth_dated <- function(df, core) {     
  df %>% 
    mutate(core_id = core,
           fraction_carbon = `TOC [%] (Element analyser, Eurovector ...)`/100) %>% 
    rename(depth_min = `Depth top [m]`,
           depth_max = `Depth bot [m]`,
           dry_bulk_density = `DBD [g/cm**3]`,
           delta_c13 = `Œ¥13C Corg [‚Ä∞ PDB] (Mass spectrometer Thermo Finn...)`) %>% 
    select(-`Depth [m]`, -`TN [%] (Element analyser, Eurovector ...)`, 
           -`Cum mass [g/cm**2]`, -`C/N (C_org/N, atomic)`, 
           -`TOC [%] (Element analyser, Eurovector ...)`)
}


#berau
B1_depth <- format_depth(B1, "B1")
B3_depth <- format_depth(B3, "B3")
B4_depth <- format_depth(B4, "B4")
B2_depth <- format_depth_dated(B2, "B2")

berau_depth <- rbind(B1_depth, B3_depth, B4_depth) %>% 
                full_join(B2_depth) %>% 
                mutate(site_id = "Berau")

#kongsi
K1_depth <- format_depth(K1, "K1")
K2_depth <- format_depth(K2, "K2")

kongsi_depth <- rbind(K1_depth, K2_depth) %>%
                mutate(site_id = "Kongsi_Island")

#segara anakan lagoon
C24_depth <- format_depth_dated(SAL_C24, "C24")
C26_depth <- format_depth(SAL_C26, "C26")
C41_depth <- format_depth(SAL_C41, "C41")
C42_depth <- format_depth(SAL_C42, "C42")
C43_depth <- format_depth(SAL_C43, "C43")
C45_depth <- format_depth(SAL_C45, "C45")
C49_depth <- format_depth(SAL_C49, "C49")
E16_depth <- format_depth(SAL_E16, "E16")
E40_depth <- format_depth_dated(SAL_E40, "E40")
E44_depth <- format_depth(SAL_E44, "E44")
E46_depth <- format_depth(SAL_E46, "E46")
E47_depth <- format_depth(SAL_E47, "E47")

SAL_depth <- rbind(C26_depth, C41_depth, C42_depth, C43_depth, C45_depth, 
                  C49_depth, E16_depth, E44_depth, E46_depth, E47_depth) %>% 
             full_join(C24_depth) %>% 
             full_join(E40_depth) %>% 
             mutate(site_id = case_when(str_detect(core_id, "C") ~ "Segara_Anakan_Lagoon_Central",
                                                  TRUE ~ "Segara_Anakan_Lagoon_East")) 

#join all 
depthseries <- berau_depth %>% full_join(kongsi_depth) %>% 
                full_join(SAL_depth) %>% 
                mutate(study_id = id,
                       method_id = "single set of methods",
                       depth_min = depth_min * 100, #m to cm 
                       depth_max = depth_max * 100,
                       pb210_unit = "becquerelsPerKilogram", 
                       pb214_unit = "becquerelsPerKilogram",
                       cs137_unit = "becquerelsPerKilogram",
                       delta_c13 = delta_c13/1000000) %>% #convert to ppm
                rename(total_pb210_activity = `210Pb [Bq/kg] (total, High resolution, low b...)`,
                       total_pb210_activity_se = `Uncertainty [¬±] (Pb-210 (total), 1 sigma, High...)`,
                       pb214_activity = `214Pb [Bq/kg] (High resolution, low backgrou...)`,
                       pb214_activity_se = `Uncertainty [¬±] (Pb-214, 1 sigma, High resolut...)`,
                       cs137_activity = `137Cs [Bq/kg] (High resolution, low backgrou...)`,
                       cs137_activity_se = `Uncertainty [¬±] (Cs-137, 1 sigma, High resolut...)`,
                       excess_pb210_activity = `210Pb xs [Bq/kg] (High resolution, low backgrou...)`,
                       excess_pb210_activity_se = `Uncertainty [¬±] (Pb-210 excess, 1 sigma, High ...)`) %>% 
              select(-`241Am d t [Bq/kg] (High resolution, low backgrou...)`, -`137Cs d t [Bq/kg] (High resolution, low backgrou...)`)

#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####


## ... Impacts ####
#from associated paper
impacts <- tibble(study_id = id,
                  site_id = c("Berau", "Kongsi_Island", "Segara_Anakan_Lagoon"),
                  impact_class = c("natural", "natural", "sediment added"))

impacts <- reorderColumns("impacts", impacts)



## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "cores", "depthseries", "impacts") 

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
write_csv(methods, "data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_methods.csv")
  #write_csv(sites, "data/primary_studies/AUTHOR_ET_AL_YYYY/derivative/AUTHOR_ET_AL_YYYY_sites.csv")
write_csv(cores, "data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_cores.csv")
write_csv(depthseries, "data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_depthseries.csv")
 #write_csv(species, "data/primary_studies/AUTHOR_ET_AL_YYYY/derivative/AUTHOR_ET_AL_YYYY_species.csv")
write_csv(impacts,"data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_impacts.csv")

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

study_citation <- data.frame(bibliography_id = "Kusumaningtyas_et_al_2018",
                             title = "Carbon, nitrogen and stable carbon isotopes, and radionuclides in sediment cores from Segara Anakan Lagoon, Berau and Kongsi Island, Indonesia, 2013 and 2016",
                             author = "Kusumaningtyas, Mariska Astrid; Hutahaean, Andreas A; Fischer, Helmut W; Pérez-Mayo, Manuel; Ransby, Daniela; Jennerjahn, Tim C",
                             bibtype = "Misc", 
                             publication_type = "primary dataset",
                             doi = "https://doi.org/10.1594/PANGAEA.896852",
                             url = "https://doi.pangaea.de/10.1594/PANGAEA.896852",
                             year = "2018") %>% 
                  column_to_rownames("bibliography_id")

study_citation_article <- data.frame(bibliography_id = "Kusumaningtyas_et_al_2019",
                                     title = "Variability in the organic carbon stocks, sources, and accumulation rates of Indonesian mangrove ecosystems",
                                     author = "Mariska Astrid Kusumaningtyas, Andreas A. Hutahaean, Helmut W. Fischer, Manuel Pérez-Mayo, Daniela Ransby, Tim C. Jennerjahn",
                                     bibtype = "Article",
                                     doi = "https://doi.org/10.1016/j.ecss.2018.12.007",
                                     url = "https://www.sciencedirect.com/science/article/pii/S0272771418304207?via%3Dihub",
                                     journal = "Estuarine, Coastal and Shelf Science",
                                     publication_type = "associated source",
                                     year = "2019") %>% 
                          column_to_rownames("bibliography_id")

#merge               
study_citations <- bind_rows(study_citation, study_citation_article) %>%
  mutate(study_id = id,
         bibliography_id = c("Kusumaningtyas_et_al_2018", "Kusumaningtyas_et_al_2019"),
         publication_type = c("primary dataset", "associated source")) %>%
  remove_rownames() %>% 
  select(study_id, bibliography_id, publication_type, bibtype, everything())

WriteBib(as.BibEntry(study_citations), "data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018.bib")
write_csv(study_citations, "data/primary_studies/Kusumaningtyas_et_al_2018/derivative/Kusumaningtyas_et_al_2018_study_citations.csv")

# link to bibtex guide
# https://www.bibtex.com/e/entry-types/
