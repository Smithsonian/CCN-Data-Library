## CCRCN Data Library
# contact: klingesd@si.edu

# Data citation:
# Osland, M.J., Grace, J.B., Stagg, C.L., Day, R.H., Hartley, S.B., Enwright, N.M., Gabler, C.A., 
# 2016, U.S. Gulf of Mexico coast (TX, MS, AL, and FL) Vegetation, soil, and landscape data (2013-2014): 
# U.S. Geological Survey data release, http://dx.doi.org/10.5066/F7J1017G.

# Publication citation: 
# Osland, M.J., Feher, L.C., Griffith, K.T., Cavanaugh, K.C., Enwright, N.M., Day, R.H., Stagg, C.L., 
# Krauss, K.W., Howard, R.J., Grace, J.B., and Rogers, K., 2016, 
# Climatic controls on the global distribution, abundance, and species richness of mangrove forests: 
# Ecological Monographs, Accepted Online, http://dx.doi.org/10.1002/ecm.1248.

## INSTRUCTIONS ####################

# # 1. Designate the target webpage to scrape for data
# #   Paste the url of the target webpage here, wrapped in quotation marks
# 
# # Note: because Osland_2016 has multiple data release DOIs, we'll just run this
# #   script separately for each one. Easier to read than looping through each url
# 
# URL_1 <- "https://www.sciencebase.gov/catalog/item/57b24094e4b00148d3982cce"
# URL_2 <- "https://www.sciencebase.gov/catalog/item/57b240fce4b00148d3982cd0"
# URL_3 <- "https://www.sciencebase.gov/catalog/item/57aa11efe4b05e859be06932"
# 
#                  
# # 2. Name the files
# #   Add the names for each file into this list, wrapped in quotation marks, IN 
# #   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
# #   extension (.csv, .xlsx, etc.) in the name of the file as well.
#   
# FILE_NAMES_1 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Landscape_and_Climate_Data_2013_2014_.xml",
#                    "Dataset_03_macroclimate_landscape_and_climate_data_2_22_2016.xlsx"
# )
# 
# FILE_NAMES_2 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Soil_Data_2013_2014_.xml",
#                      "Dataset_02_macroclimate_soil_data_2_22_2016.xlsx"
# )
# 
# FILE_NAMES_3 <- list("U_S_Gulf_of_Mexico_coast_TX_MS_AL_and_FL_Macroclimate_Vegetation_Data_Section_1_2013_2014_.xml",
#                      "Dataset_01_macroclimate_vegetation_data_all_2_24_2016.xlsx"
# )
# 
# # 3. Designate file path of where these data files will go in the CCRCN library
# #   Paste the file path here, wrapped in quotation marks. The getwd() function 
# #   will automatically detect the working directory of the R project (in the case 
# #   of the CCRCN Data library, the location of where this repository is stored on 
# #   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
# #   with whatever you include within the quotation marks.
#   
# FILE_PATH <- paste0(getwd(), "/data/primary_studies/Osland_2016/original/" )
#   
## Assumptions made about data ###############


## Prep workspace #######################
# Load RCurl, a package used to download files from a URL
# library(rvest)
# library(stringr)
# library(RCurl)
library(tidyverse)
library(lubridate)
library(readxl)
library(sp)

## Download data ########################

# # The stem of the url should always be the same
# BASE_URL <- "https://www.sciencebase.gov"
# 
# # Because Osland 2016 has multiple urls, we'll need to run this loop multiple times
#  
# page <- read_html(URL_3)
#   
# # Extract the url paths for each data file embedded on the webpage, and save
# #   those paths to a list
# url_list <- page %>%
#   html_nodes('.sb-download-link') %>% 
#   html_attr("data-url")
#   
# # For each data file path on the webpage....
# for (i in 1:length(url_list)) {
#   
#   # ...extract and download file
#   download.file(paste0(BASE_URL, url_list[[i]]), paste0(FILE_PATH, FILE_NAMES_3[[i]]),
#                   mode = "wb")
# }

## Curate data to CCRCN Structure #################
FILE_PATH <- paste0(getwd(), "/data/primary_studies/Osland_2016/original/" )


# Read data in
Osland_2016_soil <- read_excel(paste0(FILE_PATH, "Dataset_02_macroclimate_soil_data_2_22_2016.xlsx"))
Osland_2016_land_climate <- read_excel(paste0(FILE_PATH, "Dataset_03_macroclimate_landscape_and_climate_data_2_22_2016.xlsx"))
Osland_2016_veg <- read_excel(paste0(FILE_PATH, "Dataset_01_macroclimate_vegetation_data_all_2_24_2016.xlsx"))

# Remove instructions at the top of sheets
depth_series_data <- Osland_2016_soil %>%
  slice(-1:-9)
# the first row will be the header
colnames(depth_series_data) <- depth_series_data[1, ] 
depth_series_data <- as.data.frame(depth_series_data[-1, ])

Osland_2016_veg <- Osland_2016_veg %>%
  slice(-1:-9)
# the first row will be the header
colnames(Osland_2016_veg) <- Osland_2016_veg[1, ] 
Osland_2016_veg <- Osland_2016_veg[-1, ]

Osland_2016_land_climate <- Osland_2016_land_climate %>%
  slice(-1:-9)
# the first row will be the header
colnames(Osland_2016_land_climate) <- Osland_2016_land_climate[1, ] 
Osland_2016_land_climate <- Osland_2016_land_climate[-1, ]


## Site level data #############

# We're going to need to add a few new columns, and aggregate out core level
#   data up to the site level

# Change value of digits so we don't have too many for the next step
options(digits=6)

# Change all quantitative attributes to numeric (we'll need this later to 
#   aggregate the data to the site level) and simultaneously rename
Osland_2016_site_core_data <- Osland_2016_land_climate %>%
  mutate(core_id = as.numeric(plot), core_longitude = as.numeric(lon),
         core_latitude = as.numeric(lat), core_elevation = as.numeric(elev)) %>%
  rename(site_id = estuary) %>% # rename to site ID
  select(-ID, -tran, -plot, -lon, -lat, -elev, -dist, -MAP, -AI, -PET, -Tmin, -TIdist,
         -GMdist)

# One "site", Galveston Bay, is not actually just Galveston Bay. It's too big.
# Splitting it into multiple, geomorphologically-derived sites.

# Osland_2016_site_core_data <- Osland_2016_site_core_data %>%
#   mutate(site_id = ifelse(site_id == "Galveston_Bay" & core_latitude < 27.45
#                           & core_longitude > -97.37 & core_latitude > 27.21
#                           & core_longitude < -97.75, "Baffin_Bay", site_id))



# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R") 
Osland_2016_site_data_boundaries <- create_multiple_geographic_coverages(Osland_2016_site_core_data)
Osland_2016_site_data <- Osland_2016_site_core_data %>%
  left_join(Osland_2016_site_data_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude, -core_id, -core_elevation)
# remove NAs before aggregation
Osland_2016_site_data <- na.omit(Osland_2016_site_data) 

# Now aggeregate data to the site level
Osland_2016_site_data <- Osland_2016_site_data %>%
  group_by(site_id) %>%
  summarize_all(mean) %>%
  mutate(study_id = "Osland_et_al_2016")



## Core data ####################

# from Osland_2016_veg and previously generated site_core data

# There's an unwieldy amount of columns, so we'll select them down
Osland_2016_core_data <- Osland_2016_veg %>%
  select(1:13, `1hSp`) %>%
  rename(site_id = estuary, core_id = plot) %>%
  mutate(core_id = as.numeric(core_id))

Osland_2016_core_data <- Osland_2016_site_core_data %>%
  left_join(Osland_2016_core_data) %>%
  # concatenate date attributes, then remove
  unite("core_date", c("year", "month", "day"), sep = "-") %>%
  mutate(core_date = ymd(core_date)) %>%
  # current core IDs are not unique, so concatenate with site IDs
  mutate(core_id = tolower(paste0(site_id, "_", core_id))) %>%
  rename(core_notes = "criteria", tallest_species = `1hSp`) %>%
  mutate(core_elevation_datum = "NAVD88") %>%
  mutate(study_id = "Osland_et_al_2016") %>%
  select(study_id, site_id, core_id, core_longitude, core_latitude, core_elevation, 
         core_date, core_notes, core_elevation_datum, tallest_species)

# There's one core without coordinates. We can estimate its location by the ID
#   and description.
Osland_2016_core_data <- Osland_2016_core_data %>% 
  mutate(core_latitude = ifelse(core_id == "mission_aransas_bay_87", 28.060055, 
                                core_latitude)) %>% 
  mutate(core_longitude = ifelse(core_id == "mission_aransas_bay_87", -97.237439, 
                                 core_longitude)) %>% 
  mutate(core_position_method = ifelse(core_id == "mission_aransas_bay_87", 
                                       "other low resolution", NA)) %>% 
  mutate(core_position_notes = ifelse(core_id == "mission_aransas_bay_87", 
                                       "location estimated from core description using Google Maps", NA)) 
  
# There's another core that is missing from the depthseries table, 
#   ("Dataset_02_macroclimate_soil_data_2_22_2016). Remove it from core level
Osland_2016_core_data <- Osland_2016_core_data %>% 
  filter(core_id != "tampa_bay_97")
  

## Depth series data ###################

# From Osland_2016_soil

# Call functions from curation_functions script
# source("./scripts/1_data_formatting/curation_functions.R") 

Osland_2016_depth_series_data <- depth_series_data %>%
  rename(site_id = "estuary") %>%
  mutate(core_id = paste0(site_id, "_", plot)) %>%
  mutate(core_id = as.factor(tolower(core_id))) %>% # Core IDs are expressed as factor not numeric
  rename(dry_bulk_density = "bd") %>%
  mutate(fraction_organic_matter = as.numeric(som)/100) %>%
  # CCRCN does not have standards for soil moisture content yet, but this approach
  #   most closely aligns with other attributes
  mutate(fraction_moisture_content = as.numeric(moist)/100) %>%
  
  # The legend dictates that only one soil depth interval was sampled, 01-5 cm.
  # So we'll add a single set of min and max depths for each core
  mutate(depth_min = as.double(0), depth_max = as.double(15)) %>%
  
  mutate(study_id = "Osland_et_al_2016") %>%
  # Re-order columns
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter)


## Species data ##################

# Select only the columns that correspond to the species presence in a 1-m2
#   plot surrounding each core, plus the necessary other data
Osland_2016_species_data <- Osland_2016_veg %>%
  select(1:182) %>%
  select("estuary", "plot", 16:182)

# Rename site and core IDs
Osland_2016_species_data <- Osland_2016_species_data %>%
  rename(site_id = "estuary") %>%
  # current core IDs are not unique, so concatenate with site IDs
  mutate(core_id = tolower(paste0(site_id, "_", plot))) %>%
  select(-plot) %>%
  mutate(study_id = "Osland_et_al_2016")

# NOTE 
# fraction coverage has been removed as it is not part of the CCRCN guidance
# currently there may be multiple species present at a given site 

# The species presence data is currently  wide-form, with each column
#   corresponding to the fraction area occupied by each plant species.
# Let's write a function that can be applied to each row to change the data to
#   long-form.
find_max <- function(df, first_col, last_col) {
  df <- df %>%
    # Gather the species columns into one column and select just the most dominant
    #   species present per core.
    gather(colname, value, first_col:last_col) %>%
    mutate(value = value) %>%
    # Choose just the most dominant plant species
    filter(value == max(value))

  # If all of the values for the species presence columns are 0, then change the
  #   values to NA and only use the first row. The species presence value for
  #   the given core will now just be NA.
  if (df$value[[1]] == 0) {
    df$value <- NA
    df <- slice(df, 1)
  }
  df
}



for (i in 1:nrow(Osland_2016_species_data)) {
  
  df <- slice(Osland_2016_species_data, i)
  if (i == 1) {
    out <- find_max(df, 5, 168)
  } else {
    
    out <- rbind(out, find_max(df, 5, 168))
  }
}

colname <- out$colname

Osland_2016_species_data <- out %>%
  rename(species_code = "colname") %>%
  rename(fraction_coverage = "value") %>%
  mutate(fraction_coverage = as.numeric(fraction_coverage)) %>%
  select(study_id, site_id, core_id, species_code, fraction_coverage)

# Remove the '1c' string from the species_code entries
Osland_2016_species_data$species_code <- 
  sapply(Osland_2016_species_data$species_code,
    function(x) {
      gsub("1c", "", x) # Replace '1c' with ''                                           
   })

# Recode vegetation codes to the proper format
Osland_2016_species_data <- Osland_2016_species_data %>%
  mutate(species_code = recode_factor(species_code, 
                                      "ACAU" = "AcAu",  "ALPH" = "AlPh", "AMPARB" = "AmpArb", "AMPS" = "AmPs", "AVGE" = "AvGe", 
                                      "BAHA" = "BaHa", "BAMA" = "BaMa", "BOFR" = "BoFr", 
                                      "CHPI" = "ChPi", "CLMA" = "ClMa", "COER" = "CoEr", "CRVI" = "CrVi", "CUSP" = "CuSp", "DAEC" = "DaEc",  
                                      "DISP"  = "DiSp", "ELCE"  = "ElCe", "FIMCAS" = "FimCas","HAWR"= "HaWr",   "HYMU"= "HyMu",   "ILDE"= "IlDe",   "ILVO"= "IlVo",  
                                      "IPSA"= "IpSa",   "JURO" = "JuRo",  "KOVI"  = "KoVi", "LARA"= "LaRa", "LUAL" = "LuAl",  "LYCA"= "LyCa" ,
                                      "MOCE" = "MoCe",  "MOLI"  = "MoLi", "PAHA" = "PaHa","PARE"  = "PaRe", 
                                      "PAVA"  = "PaVa", "PAVI" = "PaVi", 
                                      "PHAU"  = "PhAu", "POHY" = "PoHy",  "RHMA"  = "RhMa", "RUTR" = "RuTr",  "RUVE"  = "RuVe", "SAAR" = "SaAr",  
                                      "SABI"  = "SaBi", "SADE" = "SaDe",  "SAV" = "SaV", "SCAM" = "ScAm",  "SCRO"  = "ScRo", "SCSC" ="ScSc",  "SEHE"  = "SeHe",
                                      "SEPO"  = "SePo", "SPAL" = "SpAl",  "SPBA"  = "SpBa", "SPCY" = "SpCy",  "SPPA"  = "SpPa", "SPSP" = "SpSp",  "SULI"="SuLi",
                                      "SYTE"  = "SyTe", "THTE" = "ThTe")) %>%
  recode_species(species_code = species_code) %>%
  select(study_id, site_id, core_id, species_code)

# reorder columns 
source("./scripts/1_data_formatting/qa_functions.R")

sites <- reorderColumns("sites", Osland_2016_site_data)

cores <- Osland_2016_core_data %>%
  reorderColumns("cores", .) %>%
  mutate(core_year = year(core_date), 
         core_month = month(core_date),
         core_day = day(core_date)) %>% select(-core_date)

depthseries <- Osland_2016_depth_series_data %>% 
  reorderColumns("depthseries", .) %>%
  mutate(method_id = "unknown methods")

species <- reorderColumns("species", Osland_2016_species_data)

## Create study-level data ######

if(!file.exists("data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_study_citations.csv")){
  doi <- "10.5066/F7J1017G"
  study <- "Osland_et_al_2016"
  
  # Get bibtex citation from DOI
  biblio_raw <- GetBibEntryWithDOI(doi)
  biblio_df <- as.data.frame(biblio_raw)
  
  study_citations <- biblio_df %>%
    mutate(bibliography_id = "Osland_et_al_2016_data", 
           study_id = study,
           publication_type = "primary dataset") %>%
    remove_rownames() %>%
    select(study_id, bibliography_id, publication_type, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016.bib")
  write_csv(study_citations, "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_study_citations.csv")
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("cores", "depthseries", "sites", "species")

updated <- updateTables(table_names)

# save listed tables to objects

sites <- updated$sites
# methods <- updated$methods
depthseries <- updated$depthseries

species <- updated$species

# Jaxine efforts to assign habitat 

# create a lookup table from the species 
hab_lookup <- updated$species %>% 
  filter(habitat != 'unvegetated') %>% 
  drop_na(habitat) %>% 
  distinct(core_id, habitat) %>% 
  group_by(core_id) %>% 
  summarize(habitat = toString(habitat)) %>% 
  rename(sp_habitat = habitat)

assignHabitat <- function(df, hab_col){
  return(df %>%
    mutate(new_col = case_when(grepl("SPBA|JURO|SCAM|SPPA|DISP|SPAL|CLMA|SPCY|PAVA|SEHE|BAMA|MOLI|PHAU|SAGLAN|PAVI|ELCE|BOFR|SOST|emergent|succulent|mixed grass|mixed sedge|salt panne|SPSP|bunchgrass|Spartina|grass mix", !!sym(hab_col)) ~ "marsh", 
                               grepl("AVGE|RHMA|LARA|mangrove", !!sym(hab_col)) ~ "mangrove",
                               grepl("unvegetated|water|bare", !!sym(hab_col)) ~ "unvegetated",
                               grepl("upland", !!sym(hab_col)) ~ "upland",
                               grepl("wood|forest", !!sym(hab_col)) ~ "swamp",
                               grepl("shrub|BAHA|IVFR", !!sym(hab_col)) ~ "scrub/shrub",
                               grepl("seagrass|HAWR", !!sym(hab_col)) ~ "seagrass", 
                               grepl("alga|ALGMAT", !!sym(hab_col)) ~ "algal mat",
                               grepl("flat", !!sym(hab_col)) ~ "mudflat")))
}

hab_assignment <- updated$cores %>% 
  left_join(hab_lookup) %>% select(-core_notes, core_notes) %>% 
  separate_wider_delim(core_notes, names = c("first", "second"), delim = "/", too_few = "align_start", cols_remove = F) %>% 
  separate_wider_delim(second, names = c("second", "dominate"), delim = "-", too_many = "merge") %>% 
  assignHabitat(., "first") %>% rename(first_habitat = new_col) %>% 
  assignHabitat(., "second") %>% rename(second_habitat = new_col) %>%  
  assignHabitat(., "tallest_species") %>% rename(tallest_habitat = new_col) %>%   
  
  # conditionally assign habitat
  mutate(habitat = case_when(grepl("1", dominate) ~ first_habitat,
                             grepl("2", dominate) ~ second_habitat,
                             grepl("mixed", dominate) & first_habitat == second_habitat ~ first_habitat,
                             # I think beyond this could use a second opinion
                             first_habitat %in% c("unvegetated", "mudflat") ~ second_habitat,
                             first_habitat == tallest_habitat ~ first_habitat,
                             is.na(first_habitat) & !is.na(tallest_habitat) ~ tallest_habitat,
                             second_habitat == "mudflat" ~ first_habitat,
                             tallest_habitat == "mangrove" ~ tallest_habitat,
                             is.na(sp_habitat) ~ first_habitat,
                             T ~ sp_habitat)) %>% 
         # tallest_habitat = case_when(grepl()) %>% 
  select(core_id, tallest_species, dominate, first, second, first_habitat, second_habitat, sp_habitat, tallest_habitat, habitat)
  # filter(is.na(habitat) | grepl(",", habitat)) # uncomment to isolate and inspect the unassigned habitats 

  # [1] "ACSP"   "ALGMAT" "ALPH"   "ANGLE"  "AVGE"   "BAHA"   "BAMA"   "bare"   "BBF2"   "BOFR"   "CABO"   "CLMA"   "COER"  
  # [14] "DAEC"   "DISP"   "ELCE"   "EUAX"   "EUEX"   "EUPE"   "FICA"   "HAWR"   "HECU"   "ILDE"   "ILVO"   "IVFR"   "JURO"  
  # [27] "JUVI"   "LARA"   "litter" "LYCA"   "MOCE"   "MOLI"   "NA"     "OPSP"   "oyster" "PAHA"   "PAHE"   "PARE"   "PASP"  
  # [40] "PAVI"   "PHAU"   "PIEL"   "POCO"   "RAAC"   "RHMA"   "SABI"   "SADE"   "SAPA"   "SCAM"   "SCPU"   "SCRO"   "SCSC"  
  # [53] "SCTA"   "SCTE"   "SEHE"   "SEPO"   "SOST"   "SPAL"   "SPBA"   "SPCY"   "SPPA"   "SPSP"   "SULI"   "SYSU"   "THTE"  
  # [66] "TORA"   "VEVI"  

cores <- updated$cores %>% left_join(hab_assignment %>% select(core_id, habitat)) %>% 
  select(-tallest_species)

## QA/QC of data ################

# Check col and varnames
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
results <- test_numeric_vars(depthseries)

## Write data #################
write_csv(species, "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_species.csv")
write_csv(depthseries, "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_depthseries.csv")
write_csv(sites, "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_sites.csv")
write_csv(cores, "./data/primary_studies/Osland_2016/derivative/Osland_et_al_2016_cores.csv")

pal <- leaflet::colorFactor(
  palette = "Set1",
  domain = unique(cores$habitat))

cores %>% 
  filter(habitat == "upland") %>% 
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 2, label= ~core_notes, color = ~pal(habitat)) %>% 
  addLegend(pal = pal, values = ~habitat)
