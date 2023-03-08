## CCN Data Library Hook Script########
## contact: Rose Cheney, cheneyr@si.edu

#Dataset- https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-fce.1228.2
#### Organic and inorganic data for soil cores from Brazil and Florida Bay seagrasses to support 
# Howard and Fourqurean 2020, CO2 released by carbonate sediment production in some coastal areas may offset the benefits of seagrass 
# and Blue Carbon storage, Limnology and Oceanography, DOI: 10.1002/lno.10621. 

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RefManageR)
library(skimr)
library(leaflet)
library(data.table)


# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# read in raw data 
brazil_cores <- read_csv("data/primary_studies/Howard_and_Fourqurean_2020/original/knb-lter-fce.1228.2/brazil_cores.csv")
fl_bay_cores <- read_csv("data/primary_studies/Howard_and_Fourqurean_2020/original/knb-lter-fce.1228.2/Fl_bay_cores.csv")
methods_raw <- read_xlsx("data/primary_studies/Howard_and_Fourqurean_2020/intermediate/Howard_and_Fourqurean_2020_materials_and_methods.xlsx", 2)
sites_raw <- read_csv("data/primary_studies/Howard_and_Fourqurean_2020/intermediate/Howard_and_Fourqurean_2020_supplement.csv")


## 1. Curation ####

# this study ID must match the name of the dataset folder
# include this id in a study_id column for every curated table
id <- "Howard_and_Fourqurean_2020"
# if there are only two authors: Author_and_Author_year
# "year" will be exchanged with "unpublished" in some cases

## ... Methods ####

# curate materials and methods table
methods <- methods_raw %>% mutate(method_id = "single set of methods",
                                  roots_flag = "roots and rhizomes separated",
                                  fraction_carbon_method = "EA",
                                  carbon_profile_notes = "organic C was calculated as the difference between inorganic C and total C")

methods <- reorderColumns("methods", methods)

## ... Sites ####
# curate and format site level data from full data table
sites <- sites_raw %>% rename(site_description = notes) %>% 
                       mutate(salinity_class = "saline",
                              salinity_method = "measurement",
                              vegetation_class = "seagrass",
                              inundation_class = "low",
                              inundation_method = "field observation",
                              study_id = id) %>% 
                       mutate(site_id = recode(site_id,"bob" = "Bob Allen Keys","rb" = "Russell Bank",
                                                        "nm" = "Nine Mile Bank", "tc" = "Trout Cove")) %>% 
                       select(-latitude, -longitude, -salinity, -`seagrass present`)



## ... Cores ####
#curate core level data table 

# rename variables to match across FL and Brazil datasets
fl_bay_cores <- fl_bay_cores %>% rename(Latitude = Lat,
                                        Longitude = Long)
# join datasets
full_data <- brazil_cores %>% bind_rows(brazil_cores,fl_bay_cores) %>% 
                              rename(latitude = Latitude,
                                     longitude = Longitude,
                                     site_id = Site) %>% 
                              mutate(core_id = paste(site_id, "_", Rep),
                                     study_id = id,
                                     site_id = recode(site_id, "bob" = "Bob Allen Keys", 
                                                              "rb" = "Russell Bank",
                                                              "nm" = "Nine Mile Bank", 
                                                              "tc" = "Trout Cove"))
  
 
#reformat variables and add needed columns for cores table
cores <- full_data %>% mutate(year= case_when(latitude > -20 ~ "2015",
                       latitude < -20 ~ "2002")) %>% 
                       mutate(vegetation_class ="seagrass",
                              vegetation_method = "field observation",
                              habitat = "seagrass",
                              position_method = "other high resolution",
                              core_length_flag = "core depth limited by length of corer") %>% 
                     select( -depth, -loi, -dbd, -Cinorg, -Corg, -Rep) %>% 
                     distinct() #collapse into unique cores 

#reorder columns 
cores <- reorderColumns("cores", cores)


## ... Depthseries ####

# curate depthseries table        
depthseries <- full_data %>% mutate(fraction_organic_matter = loi/100,
                                    fraction_carbon = Corg/100,
                                    method_id = "single set of methods",
                                    compaction_notes = "corer minimizes compaction",
                                    depth = abs(depth),
                                    depth_1 = abs(shift(depth, n=1, type = "lead")),
                                    depth_2 = abs(shift(depth_1, n=1, type = "lead")), 
                                    depth_min = case_when(depth_1 == 0 ~ depth,
                                                          TRUE ~ ((depth + depth_1)/2)),
                                    depth_max = case_when(depth_1 == 0 ~ depth_min + 9,
                                                          depth_2 == 0 ~ depth_min + 9,
                                                          TRUE ~  ((depth_2 + depth_1)/2))) %>% 
                              rename(dry_bulk_density = dbd) %>% 
                              select(-latitude, -longitude, -loi, -Rep, -Cinorg, -Corg,
                                      -depth, -depth_1, -depth_2)
#FIND NAs
which(is.na(depthseries), arr.ind = TRUE)

#recode missing values to match depth interval
depthseries <- depthseries %>% mutate(depth_min = if_else(is.na(depth_min), 90, depth_min, missing = NULL),
                                      depth_max = case_when(dry_bulk_density == 0.9425000 ~ 90,
                                                            dry_bulk_density == 1.0235000 ~ 95,
                                                            TRUE ~ depth_max))

#reorder columns 
depthseries <- reorderColumns("depthseries", depthseries)


## ... Species ####

# if provided at the site or core-level, curate taxa table
#add and recode relevant variables 
species <- sites_raw %>% select(c(1,5)) %>% 
           rename(species_code = `seagrass present`) %>%
           mutate(study_id = id,
                  code_type = "Genus species",
                  habitat = "seagrass",
                  site_id = recode(site_id, "bob" = "Bob Allen Keys", "rb" = "Russell Bank",
                                            "nm" = "Nine Mile Bank", "tc" = "Trout Cove")) %>%
           separate(species_code, c("species_code", "species2"), sep = " & ") %>% 
           pivot_longer(cols = starts_with("species"), names_to = NULL,
                               values_to = "species_code", values_drop_na = TRUE) %>% 
           mutate(species_code = recode(species_code, "H. wrightii" = "Halodule wrightii", 
                                                      "T. testudinum" = "Thalassia testudinum",
                                                      "H. decipiens" = "Halophilia decipiens"))
           
species <- reorderColumns("species", species)

## ... Impacts ####
## Not provided in dataset 


## 2. QAQC ####

## Mapping
leaflet(cores) %>%
    addTiles() %>% 
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("methods", "sites", "cores", "depthseries", "species")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)

# test required and conditional attributes
### group species code to run qc
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
test_numeric_vars(depthseries)

#visualize fraction organic matter ~ fraction carbon 
ggplot(depthseries) +
  geom_point(aes(fraction_organic_matter, fraction_carbon))


## 3. Write Curated Data ####

# write data to final folder
write_csv(methods, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_methods.csv")
write_csv(sites, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_sites.csv")
write_csv(cores, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_cores.csv")
write_csv(depthseries, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_species.csv")
## Impacts not included, write_csv(impacts, "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_impacts.csv")

## 4. Bibliography ####

####Create bibtex citation for dataset and associated article 

#citation for primary dataset
study_citation_data <- data.frame(bibliography_id = "Howard_and_Fourqurean_2020",
           title = "Organic and inorganic data for soil cores from Brazil and Florida Bay seagrasses to support Howard et al 2018, CO2 released by carbonate sediment production in some coastal areas may offset the benefits of seagrass “Blue Carbon” storage",
           author = "Jason L. Howard and James W. Fourqurean",
           bibtype = "Misc",
           doi = "DOI: 10.1002/lno.10621",
           url = "https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-fce.1228.2", 
           journal = "Limnology and Oceanography",
           publication_type = "primary dataset", ###if article -- "associated source"
           year = "2020") %>% 
  column_to_rownames("bibliography_id")

#citation for associated article
study_citation_article <- data.frame(bibliography_id = "Howard_et_al_2018",
                                    title = "CO2 released by carbonate sediment production in some coastal areas may offset the benefits of seagrass “Blue Carbon” storage",
                                    author = "Howard, J.L., Creed, J.C., Aguiar, M.V.P. and Fourqurean, J.W.",
                                    bibtype = "Article",
                                    doi = "DOI: 10.1002/lno.10621",
                                    url = "https://aslopubs.onlinelibrary.wiley.com/doi/10.1002/lno.10621",
                                    journal = "Limnology and Oceanography",
                                    publication_type = "associated source",
                                    year = "2018") %>% 
                column_to_rownames("bibliography_id")
#merge               
study_citations <- bind_rows(study_citation_data, study_citation_article) %>%
                  mutate(study_id = id,
                  bibliography_id = c("Howard_and_Fourqurean_2020_dataset", "Howard_et_al_2018_paper"),
                         publication_type = c("primary dataset", "associated source")) %>%
                  remove_rownames() %>% 
                  select(study_id, bibliography_id, publication_type, bibtype, everything())
                
#Write to joined .bib file 
bib_file <- study_citations %>%
           select(-study_id, -publication_type) %>%
           column_to_rownames("bibliography_id")                
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020.bib")
write_csv(study_citations, "./data/primary_studies/Howard_and_Fourqurean_2020/derivative/Howard_and_Fourqurean_2020_citations.csv")


                
                
