## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Data and publication citations #########


## 2. Prep workspace and scrape data from web ####################

## ... 2A. Load packages #######################
# Load RCurl, a package used to download files from a URL
# library(rvest)
# library(stringr)
# library(RCurl)
library(tidyverse)
library(lubridate)
library(readxl)
library(RefManageR)

## ... 3B. Download data ###############

# DATA DOWNLOAD WORKFLOW ARCHIVED

# # 1. Designate the target webpage to scrape for data
# #   Paste the url of the target webpage here, wrapped in quotation marks
# 
# URL <- "https://repository.si.edu/handle/10088/31949"
# 
# 
# # 2. Name the file
# #   Add the names for each file into this list, wrapped in quotation marks, IN 
# #   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
# #   extension (.csv, .xlsx, etc.) in the name of the file as well.
# 
# FILE_NAME <- "Megonigal_J_Patrick-20170103-Abu_Dhabi_Blue_Carbon_Project_Ecological_Applications.xlsx"
# 
# # 3. Designate file path of where these data files will go in the CCRCN library
# #   Paste the file path here, wrapped in quotation marks. The getwd() function 
# #   will automatically detect the working directory of the R project (in the case 
# #   of the CCRCN Data library, the location of where this repository is stored on 
# #   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
# #   with whatever you include within the quotation marks.
# 
# FILE_PATH <- paste0(getwd(), "/data/primary_studies/Schile-Beers_2017/original/" )
# 
# # The stem of the url should always be the same
# BASE_URL <- "https://repository.si.edu"
# 
# # Extract and save the url path for each data file embedded on the webpage
#   # if using Chrome, html_node and html_attr data can be obtained 
#   # using an extension such as "SelectorGadget" (html_node) and the "inspect" option 
#   # when right-clicking an element on a webpage (html_attr). 
# page <- read_html(URL)
# page <- page %>%
#   html_nodes('.file-link a') %>% 
#   html_attr("href")
# 
# # Download the data to your file path 
# download.file(paste0(BASE_URL, page), paste0(FILE_PATH, FILE_NAME),mode = "wb")

## 3. Curate data to CCRCN Structure #################

## ... 3A. Read data in ##################

# Paste file name and path in again
FILE_NAME <- "Megonigal_J_Patrick-20170103-Abu_Dhabi_Blue_Carbon_Project_Ecological_Applications.xlsx"
FILE_PATH <- paste0(getwd(), "/data/primary_studies/Schile-Beers_2017/original/" )

plot_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="plot information", na = "n.d.")
raw_depthseries_data <- read_excel(paste0(FILE_PATH, FILE_NAME), sheet="soil carbon data", na = "nd")

# Revision to original data hook script: The cores in the "seagrass" ecosystem were initially collected as part 
# of a separate study. I'll change their study ID to "Campbell_et_al_2015"

## ... 3B. Depth series data ###################

# Issues: 
# 1. Two sites have multi-year entries for cores that either do not have matching depth series data 
# or have depth series data but no clear core-level metadata (and therefore no location data) 
# The specific site_ids are: Eastern Mangrove 10 yr, Eastern Mangrove 7 yr, Eastern Mangrove 3 yr,
# Jubail Is. 10 yr, Jubail Is. 7 yr, Jubail Is. 3 yr, Abu al Abyad 15 yr
# 2. According to the methods in the publication, core depth was to either 3 m (the corer was 1 m long) or until parent material
# There is no clear core_depth_flag code for the former, and I have coded it as "core depth limited by length of corer"

depthseries <- raw_depthseries_data %>%
  rename(site_id = "Site", 
         core_length = "total core length (cm)") %>%
  # I will remove the following cores that have no or conflicting core-level entries: 
  filter(site_id != "Jubail Is. 10 yr", site_id != "Jubail Is. 7 yr", site_id != "Jubail Is. 3 yr") %>%
  
  # there are inconsistencies in the spelling and abbreviation of site names between the core- and depth-level data
  # I need to make them consistent to create core_ids that match 
  mutate(site_id = gsub("Thimiriya", "Thumayriyah", site_id)) %>%
  mutate(site_id = gsub("Is.", "Island", site_id)) %>%
  mutate(site_id = gsub("Al Zorah", "Ajman Al Zorah", site_id)) %>%
  mutate(site_id = gsub(" Al ", " al ", site_id)) %>%
  mutate(site_id = gsub("Khalba", "Kalba", site_id)) %>%
  mutate(study_id = ifelse(Ecosystem == "seagrass", "Campbell_et_al_2015", "Schile-Beers_and_Megonigal_2017")) %>%
  
  # Core IDs are expressed as factor not numeric
  # Paste site, ecosystem and plot values to create a unique core ID 
  mutate(core_id = as.factor(gsub(" ", "_", paste(site_id, paste(Ecosystem, plot, sep="_"), sep="_")))) %>%
  # I will remove the following core that has no core-level entry: 
  filter(core_id != "Al_Dabiya_1_seagrass_2") %>%
  # Fix typo
  mutate(core_id = gsub("Khalba", "Kalba", core_id)) %>%
  
  rename(dry_bulk_density = "dry bulk density (g/cm3)") %>%
  rename(fraction_organic_matter = "% organic carbon (OC)") %>%
  mutate(fraction_organic_matter = as.numeric(fraction_organic_matter) / 100) %>%
  separate(col="depth (cm)", into=c("depth_min", "depth_max"), sep="-") %>%
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, core_length) %>%
  mutate(depth_min = ifelse(is.na(depth_max==TRUE),100,depth_min)) %>%
  mutate(depth_min = as.numeric(depth_min), 
         depth_max = as.numeric(depth_max)) %>%
  
  # The depth (cm) category does not provide depth_max if it's entered as >100. 
  # However, the first entry for each core provides a core length.
  # I'll group by core_id to acquire the core_length for each core and associate it with depth_max
  group_by(core_id) %>%
  mutate(core_length = ifelse(is.na(core_length), 0, core_length)) %>%
  mutate(core_length = sum(core_length)) %>%
  mutate(depth_max = ifelse(is.na(depth_max)==TRUE,core_length, depth_max)) %>%
  select(-core_length) %>%
  ungroup() %>%
  mutate(method_id = "single set of methods")

# The depth (cm) category does not provide depth_max if it's entered as >100. 
# However, the first entry for each core provides a core length. I'll summarize the data to get that information, 
# then join it back 


## ... 3C. Core data ####################

core_data <- plot_data %>%
  rename(site_id = "Site") %>%
  # I will remove the following cores that have no or conflicting depth series-level entries: 
  filter(site_id != "Eastern Mangrove 10 yr", site_id != "Eastern Mangrove 7 yr",
         site_id != "Eastern Mangrove 3 yr", site_id != "Abu al Abyad 15 yr") %>%
  # There is a typo in the ecosystem column: plated mangrove should be planted mangrove
  mutate(Ecosystem = ifelse(Ecosystem=="plated mangrove", "planted mangrove", Ecosystem)) %>%
  # there are inconsistencies in the spelling and abbreviation of site names between the core- and depth-level data
  # I need to make them consistent to create core_ids that match 
  mutate(site_id = gsub("Jaoub", "Janoub", site_id)) %>%
  mutate(site_id = gsub("Bazam", "Basm", site_id)) %>%
  mutate(site_id = gsub("Khalba", "Kalba", site_id)) %>%
  mutate(site_id = gsub(" Al ", " al ", site_id)) %>%
  mutate(study_id = ifelse(Ecosystem == "seagrass", "Campbell_et_al_2015", "Schile-Beers_and_Megonigal_2017")) %>%
  
  # Core IDs are expressed as factor not numeric
  # Paste site, ecosystem, and plot values to create a unique core ID 
  mutate(core_id = as.factor(gsub(" ", "_", paste(site_id, paste(Ecosystem, Plot, sep="_"), sep="_")))) %>%
  # mutate(core_date = as.Date(as.numeric(Date), origin="1899-12-30")) %>%
  mutate(core_year = year(Date), 
         core_month = month(Date),
         core_day = day(Date)) %>%
  rename(vegetation_notes = "Ecosystem") %>%
  rename(XYZ = "XYZ source") %>%
  mutate(core_position_method = ifelse(XYZ == "RTK GPS", "RTK", 
                                ifelse(XYZ == "Garmin GPS", "handheld", NA)),
         core_elevation_datum = ifelse(is.na(elevation), NA, "WGS84")) %>%
  mutate(core_elevation_method = ifelse(XYZ == "RTK GPS", "RTK", NA)) %>%
  rename(core_latitude = "Latitude", core_longitude = "Longitude",
         core_elevation = "elevation",
         core_depth = "core depth (cm)") %>%
  mutate(core_length_flag = ifelse(core_depth<300, "core depth represents deposit depth", 
                                  ifelse(core_depth==300, "core depth limited by length of corer", NA))) %>%
  mutate(salinity_class = ifelse(salinity > 50, "brine", 
                                 ifelse(salinity < 51 & salinity > 29, "saline", "brackish"))) %>%
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_year, core_month, core_day,
        core_position_method, core_elevation, core_elevation_datum, core_elevation_method, vegetation_notes,
        salinity_class, everything())


# Some plot are missing coordinates. From the site description we can estimate
#   the locations and add coordinates in
cores <- core_data %>% 
  select(study_id, site_id, core_id, core_latitude, core_longitude, core_year, core_month, core_day,
         core_position_method, core_elevation, core_elevation_datum, core_elevation_method, vegetation_notes,
         salinity_class, core_length_flag) %>%
  mutate(core_latitude = ifelse(site_id == "Kalba East", 25.007509, 
                          ifelse(site_id == "Kalba South", 24.999839,
                            core_latitude))) %>% 
  mutate(core_longitude = ifelse(site_id == "Kalba East", 56.365198, 
                            ifelse(site_id == "Kalba South", 56.365198,
                              core_longitude))) %>%
  mutate(core_position_method = ifelse(site_id == "Kalba East" | 
                                 site_id == "Kalba South", "other low resolution",
                                  core_position_method)) %>% 
  mutate(core_position_notes = ifelse(core_position_method == "other low resolution",
                                      "location coarsely estimated from Google Maps",
                                       NA)) %>% 
  # jaxine edits
  # need to deal with sabkha and microbial mat later
  mutate(habitat = case_when(grepl("mangrove", vegetation_notes) ~ "mangrove",
                             vegetation_notes == "salt marsh" ~ "marsh",
                             vegetation_notes == "seagrass" ~ "seagrass")) %>% 
  #rose edits, add sabkha and microbial mat 
  mutate(habitat = case_when(grepl("sabkha", vegetation_notes) ~ "sabkha",
                             vegetation_notes == "microbial mat" ~ "microbial mat",
                             TRUE ~ habitat))
  
  

## ... 3D. Site level data #############

# We're going to need to add a few new columns, and aggregate out core level
#   data up to the site level

# Change value of digits so we don't have too many for the next step
options(digits=6)

# Rename and curate
site_data <- cores %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude, core_elevation, vegetation_notes) 
  
# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_data_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_data_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)

# Now aggeregate data to the site level
sites <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id),  
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  filter(is.na(site_longitude_max) == FALSE)

## 4. Create study-citation table ######

if(!file.exists("./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_study_citations.csv")){
  # Citation for segrass cores article: 
  # Campbell, J. E., Lacey, E. A., Decker, R. A., Crooks, S., & Fourqurean, J. W. (2015). 
  # Carbon storage in seagrass beds of Abu Dhabi, United Arab Emirates. Estuaries and Coasts, 38(1), 242-251. 
  study_campbell <- "Campbell_et_al_2015"
  doi_campbell <- "10.1007/s12237-014-9802-9"
  
  # import the CCRCN bibliography 
  # Get bibtex citation from DOI for Campbell
  biblio_campbell <- GetBibEntryWithDOI(doi_campbell)
  biblio_df <- as.data.frame(biblio_campbell)
  
  study_citations_campbell <- biblio_df %>%
    mutate(bibliography_id = "Campbell_et_al_2014_article", 
           study_id = study_campbell,
           publication_type = "synthesis source") %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Data citation: 
  # Schile, Lisa M. and Megonigal, J. Patrick. 2017. [Dataset] 
  # "Abu Dhabi Blue Carbon Demonstration Project." Distributed by Smithsonian Environmental Research Center. https://doi.org/10.5479/data_serc/10088/31949
  study_schile <- "Schile-Beers_and_Megonigal_2017"
  doi_schile <- "10.5479/data_serc/10088/31949"
  
  # The DOI for Schile-Beers and Megonigal does not return the necessary information
  biblio_schile <- BibEntry(bibtype = "Misc", 
                            key = study_schile, 
                            title = "Abu Dhabi Blue Carbon Demonstration Project",
                            author = "Schile, Lisa M. and Megonigal, J. Patrick", 
                            doi = doi_schile,
                            publisher = "Smithsonian Environmental Research Center",
                            year = "2017", 
                            url = "https://repository.si.edu/handle/10088/31949")
  
  # link each study to primary citation and join with synthesis table
  studies <- unique(cores$study_id)
  # expand data citation to include all the studies
  data_citations <- data.frame(study_id = studies,
                               as.data.frame(biblio_schile)) %>%
    mutate(publication_type = "synthesis dataset", 
           bibliography_id = "Schile-Beers_et_al_2017_data")
  
  # bind all citations together
  study_citations <- bind_rows(study_citations_campbell, data_citations) %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017.bib")
  write_csv(study_citations, "./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_study_citations.csv")
  
}

# library(bib2df)
# CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")
# 
# study_data_primary <- CCRCN_bib %>%
#   select(BIBTEXKEY, CATEGORY, DOI) %>%
#   rename(bibliography_id = BIBTEXKEY,
#          study_type = CATEGORY,
#          doi = DOI) %>%
#   filter(bibliography_id %in% studies) %>%
#   mutate(study_id = bibliography_id, 
#          study_type = tolower(study_type)) %>%
#   select(study_id, study_type, bibliography_id, doi) 
# 
# synthesis_study_id <- "Schile-Beers_and_Megonigal_2017"
# synthesis_doi <- "10.5479/data_serc/10088/31949"
#   
# study_data_synthesis <- cores %>%
#   filter(study_id == "Campbell_et_al_2015") %>%
#   group_by(study_id) %>%
#   summarize(study_type = "synthesis",
#             bibliography_id = synthesis_study_id, 
#             doi = synthesis_doi) %>%
#   bind_rows(study_data_primary)

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("cores", "depthseries", "sites")

updated <- updateTables(table_names)

# save listed tables to objects

# methods <- updated$methods
depthseries <- updated$depthseries
sites <- updated$sites
cores <- updated$cores %>% 
  # add core Bu_Tinah_Shamal_mangrove_2 position as a site level replicate of core Bu_Tinah_Shamal_mangrove_1
  mutate(latitude = case_when(core_id == "Bu_Tinah_Shamal_mangrove_2" ~ 24.631528,
                              T ~ latitude),
         longitude = case_when(core_id == "Bu_Tinah_Shamal_mangrove_2" ~ 53.051491,
                               T ~ longitude),
         position_notes = case_when(core_id == "Bu_Tinah_Shamal_mangrove_2" ~ "site level replicate of core Bu_Tinah_Shamal_mangrove_1's position",
                                    T ~ position_notes))



## QA/QC ###############
source("./scripts/1_data_formatting/qa_functions.R")

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)
testConditional(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## 6. Write data ##############

write_csv(sites, "./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_sites.csv")
write_csv(cores, "./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_cores.csv")
write_csv(depthseries, "./data/primary_studies/Schile-Beers_2017/derivative/Schile-Beers_Megonigal_2017_depthseries.csv")
