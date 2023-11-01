## CCN Data Library ####

## Soil core data curation script for Marot et al 2020
## contact: Henry Betts, BettsH@si.edu

## Data release: Sedimentary Data from Grand Bay, Alabama/Mississippi, 2014–2016
## URL: https://doi.org/10.5066/P9FO8R3Y

# load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
library(RefManageR)

# load in helper functions
source("scripts/1_data_formatting/curation_functions.R") # For curation
source("scripts/1_data_formatting/qa_functions.R") # For QAQC

# link to database guidance for easy reference:
# https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html


## Step 1: Sediment Physical Properties ####

## 2014
sediment <- "./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_SedimentPhysicalProperties/14CCT01_SedimentPhysicalProperties.xlsx"
results <- function(path){
  df <- data.frame()
  # Read sheets 2:12
  for (a in 2:12) {
    sheet <- a
    temp.df <- read_xlsx(path, sheet = sheet, skip = 1, na = "--") 
    colnames(temp.df) <- 1:6 # Rename because R doesn't read the column names as the same between sheets
    df <- rbind(df, temp.df)
  }
  return(df)
}

# Include sheet 1 
sediment_2014_ss <- read_excel(sediment, skip = 1) %>% 
  rename(core_id = "Sample ID",
         water_content = "Water \r\nContent \r\n(gwater/gwet)",
         porosity = "Porosity (cm3voids/cm3wet)",
         dry_bulk_density = "Dry Bulk \r\nDensity \r\n(g/cm3)",
         fraction_organic_matter = "Loss On \r\nIgnition \r\n(gOM/gdry)") %>% 
  mutate(depth_min = 0,
         depth_max = 1,
         core_id = gsub("a|b|c", "", core_id)) %>% 
  filter(!grepl("=", core_id))

# Merge sheets
sediment_2014 <- results(path = sediment) %>% # Call the function from above
  rename(core_id = 1,
         depth = 2,
         water_content = 3,
         porosity = 4,
         dry_bulk_density = 5,
         fraction_organic_matter = 6) %>% 
  filter(!grepl("=", core_id)) %>% 
  separate(depth, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(depth_max = gsub("a", "", depth_max),
         depth_min = as.numeric(depth_min),
         depth_max = as.numeric(depth_max)) %>% 
  full_join(sediment_2014_ss) %>% 
  drop_na(core_id)

## 2015 & 2016: surface sample data (no depth provided)
sediment_2015 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2015-315-FA/15CCT02_SedimentPhysicalProperties/15CCT02_SedimentPhysicalProperties.xlsx", skip = 1, na = "--") 
sediment_2016_4 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2016-348-FA/16CCT04_SedimentPhysicalProperties/16CCT04_SedimentPhysicalProperties.xlsx", skip = 1)
sediment_ss <- full_join(sediment_2015, sediment_2016_4) %>% 
  rename(core_id = "Sample ID",
         water_content = "Water \r\nContent \r\n(gwater/gwet)",
         porosity = "Porosity (cm3voids/cm3wet)",
         dry_bulk_density = "Dry Bulk \r\nDensity \r\n(g/cm3)",
         fraction_organic_matter = "Loss On \r\nIgnition \r\n(gOM/gdry)") %>% 
  mutate(depth_min = 0,
         depth_max = 1,
         core_id = gsub("a|b|c|,", "", core_id)) %>% 
  filter(!grepl("=", core_id)) %>% 
  drop_na(core_id)

## 2016 mixed core data (depth provided)
ss_df <- data.frame()
sed_depth_files <- list("./data/primary_studies/Marot_et_al_2020/original/2016-358-FA/16CCT07_SedimentPhysicalProperties/16CCT07_SedimentPhysicalProperties.xlsx", 
                        "./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_SedimentPhysicalProperties/16CCT03_SedimentPhysicalProperties.xlsx")
for (b in sed_depth_files) {
  sheets <- excel_sheets(b)
    for (c in sheets) {
    temp.df <- read_xlsx(b, sheet = c, skip = 1, na = "--") 
    colnames(temp.df) <- 1:6
    ss_df <- rbind(ss_df, temp.df)  
  }
}

sediment_with_depth <- ss_df %>% 
  rename(core_id = 1,
         depth = 2,
         water_content = 3,
         porosity = 4,
         dry_bulk_density = 5,
         fraction_organic_matter = 6) %>% 
  filter(!grepl("=|Note", core_id)) %>% 
  mutate(depth = ifelse(depth == "Bulk", "0-1", depth)) %>% 
  separate(depth, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(depth_max = sub("[*]|a", "", depth_max),
         depth_max = ifelse(depth_min == "Top*", 1, depth_max),
         depth_min = ifelse(depth_min == "Top*", 0, depth_min),
         depth_max = as.numeric(depth_max),
         depth_min = as.numeric(depth_min),
         core_id = ifelse(core_id == "16CCT03-GB254G(M)", "16CCT03-GB254G_3", # See the renaming of core GB254_x in df_site and df_field
                          ifelse(core_id == "16CCT03-GB254G(V)", "16CCT03-GB254G_4", core_id)))

## Merge all Sediment data
df_sediment <- full_join(sediment_with_depth, sediment_ss) %>% 
  separate(core_id, into = c("site_id", "core_id"), sep = "-", remove = T) %>% 
  drop_na(core_id) %>% 
# Resolve core ID duplicate naming conventions to core_id_x, where x = duplicate #
# Note that some naming edits will drop a character as the datasheets already provide distinct core IDs for "duplicate pairs"
  mutate(core_id = gsub("GB200G[(]A[)]", "GB200G", core_id),
         core_id = gsub("GB200G[(]B[)]a", "GB200G_3", core_id), # Note that this must be read before the subsequent line
         core_id = gsub("GB200G[(]B[)]", "GB200G_2", core_id),
         core_id = gsub("GB230G1", "GB230G", core_id),
         core_id = gsub("GB233Ga", "GB233G_2", core_id),
         core_id = gsub("GB237G2", "GB237G_2", core_id),
         core_id = gsub("GB238Ga", "GB238G_2", core_id),
         core_id = gsub("GB240G3", "GB240G_2", core_id),
         core_id = gsub("GB241G2", "GB241G_2", core_id),
         core_id = gsub("GB242G1", "GB242G_2", core_id),
         core_id = gsub("GB243G3,a", "GB243G_3", core_id), # Note that this must be read before the subsequent line
         core_id = gsub("GB243G3", "GB243G_2", core_id),
         core_id = gsub("GB256Ga", "GB256G_2", core_id),
         core_id = gsub("GB256Ga", "GB256G_2", core_id),
         core_id = gsub("GB265Ga", "GB265G_2", core_id),
         core_id = gsub("GB282Sa", "GB282S_2", core_id),
         core_id = gsub("GB289Sa", "GB289S_2", core_id))


## Step 2: Alpha Spectroscopy ####

## 2014 & 2015: this is full-core pb210 dating without other associated information in other datasets
alpha_files <- list("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_AlphaSpectroscopy/14CCT01_AlphaSpectroscopy.xlsx",
                    "./data/primary_studies/Marot_et_al_2020/original/2015-315-FA/15CCT02_AlphaSpectroscopy/15CCT02_AlphaSpectroscopy.xlsx")
df <- data.frame()
for (d in alpha_files) {
  temp.df <- read_xlsx(d, skip = 1) 
  df <- rbind(df, temp.df)  
}

alpha_merge <- df %>% 
  mutate(depth_min = 0,
         depth_max = "max")

## 2016: this has depth data
alpha_raw <- data.frame()
alpha_path <- "./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_AlphaSpectroscopy/16CCT03_AlphaSpectroscopy.xlsx"
names <- excel_sheets(alpha_path)
for (e in names) {
  temp.df <- read_xlsx(alpha_path, sheet = e, skip = 1) 
  alpha_raw <- rbind(alpha_raw, temp.df)  
}

df_alpha <- alpha_raw %>% 
  separate("Depth\r\n (cm)", into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(depth_min = as.numeric(depth_min),
         depth_max = gsub("a", "", depth_max),
         depth_max = as.numeric(depth_max)) %>% 
# Don't include the alpha_merge full-core data  
#  full_join(alpha_merge, by = c("Core ID" = "Sample ID", "Total Pb-210 Error \r\n(+/- dpm/g)", "Total Pb-210 \r\n(dpm/g)", 
#                                "depth_min", "depth_max")) %>% 
  rename(core_id = "Core ID",
         total_pb210_activity = "Total Pb-210 \r\n(dpm/g)",
         total_pb210_activity_se = "Total Pb-210 Error \r\n(+/- dpm/g)",
         ) %>% 
  filter(!grepl("=", core_id)) %>% 
  separate(core_id, into = c("site_id", "core_id"), sep = "-", remove = T) %>% 
  mutate(method_id = "alpha spectroscopy",
         site_id = gsub("166CCT03", "16CCT03", site_id)) %>% 
  mutate(pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA))



## Step 3: GammaSpectroscopy ####

# Read in docs without Be data
gamma_raw <- data.frame()
gamma_files <- list("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_GammaSpectroscopy/14CCT01_GammaSpectroscopy.xlsx",
                    "./data/primary_studies/Marot_et_al_2020/original/2016-358-FA/16CCT07_GammaSpectroscopy/16CCT07_GammaSpectroscopy.xlsx") 
for (f in gamma_files) {
  sheets <- excel_sheets(f)
  for (g in sheets) {
    temp.df <- read_xlsx(f, sheet = g, skip = 1, na = c("ND", "Tr", "--")) 
    gamma_raw <- rbind(gamma_raw, temp.df)  
  }
}

# Read in docs with Be data
gamma_raw_be_1 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_GammaSpectroscopy/16CCT03_GammaSpectroscopy.xlsx", sheet = 1, skip = 1, na = c("ND", "--")) %>% 
  drop_na("Sample/Core ID")
gamma_raw_be_2 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_GammaSpectroscopy/16CCT03_GammaSpectroscopy.xlsx", sheet = 2, skip = 1, na = c("ND", "--")) %>% 
  drop_na('Core ID') %>% 
  rename(cs137 = "Cs-137 \r\n(dpm/g)")

gamma_raw_be <- full_join(gamma_raw_be_1, gamma_raw_be_2, by = c("Depth\r\n (cm)", "Be-7 \r\n(dpm/g)", "Be-7 Error \r\n(+/- dpm/g)", 
                                                                 "Cs-137 Error \r\n(+/- dpm/g)", "Pb-210 \r\n(dpm/g)", "Pb-210 Error \r\n(+/- dpm/g)", 
                                                                 "Ra-226 \r\n(dpm/g)", "Ra-226 Error \r\n(+/- dpm/g)", "Th-234 \r\n(dpm/g)", 
                                                                 "Th-234 Error \r\n(+/- dpm/g)", "K-40 \r\n(dpm/g)", "K-40 Error \r\n(+/- dpm/g)", 
                                                                 "Sample/Core ID" = "Core ID", "Cs-137 \r\n(dpm/g)" = "cs137"))

## Merge all Gamma data
df_gamma <- full_join(gamma_raw, gamma_raw_be, by = c("Core ID" = "Sample/Core ID", "Depth\r\n (cm)", "Cs-137 Error \r\n(+/- dpm/g)", 
                                                      "Pb-210 \r\n(dpm/g)", "Pb-210 Error \r\n(+/- dpm/g)", "Ra-226 \r\n(dpm/g)", 
                                                      "Ra-226 Error \r\n(+/- dpm/g)", "Th-234 \r\n(dpm/g)", "Th-234 Error \r\n(+/- dpm/g)", 
                                                      "K-40 \r\n(dpm/g)", "K-40 Error \r\n(+/- dpm/g)", "Cs-137 \r\n(dpm/g)")) %>% 
  rename(core_id = `Core ID`,
         cs137_activity = "Cs-137 \r\n(dpm/g)",
         cs137_activity_se = "Cs-137 Error \r\n(+/- dpm/g)",
         total_pb210_activity = "Pb-210 \r\n(dpm/g)",
         total_pb210_activity_se = "Pb-210 Error \r\n(+/- dpm/g)",
         ra226_activity = "Ra-226 \r\n(dpm/g)",
         ra226_activity_se = "Ra-226 Error \r\n(+/- dpm/g)",
         be7_activity = "Be-7 \r\n(dpm/g)",
         be7_activity_se = "Be-7 Error \r\n(+/- dpm/g)") %>% 
  mutate(cs137_unit = ifelse(!is.na(cs137_activity), "disintegrationsPerMinutePerGram", NA),
         pb210_unit = ifelse(!is.na(total_pb210_activity), "disintegrationsPerMinutePerGram", NA),
         method_id = "gamma spectroscopy",
         ra226_unit = ifelse(!is.na(ra226_activity), "disintegrationsPerMinutePerGram", NA),
         be7_unit =  ifelse(!is.na(be7_activity), "disintegrationsPerMinutePerGram", NA)) %>% 
  separate("Depth\r\n (cm)", into = c("depth_min", "depth_max"), sep = "-") %>% 
  filter(!grepl("=|[*]", core_id)) %>%
  filter(!grepl("Top", depth_min)) %>% 
  mutate(depth_min = as.numeric(depth_min),
         depth_max = as.numeric(depth_max)) %>% 
  separate(core_id, into = c("site_id", "core_id"), sep = "-", remove = T) 


## Step 3: Field Log ####

# Select field log files we want from all xls files
study_dir <- "./data/primary_studies/Marot_et_al_2020/original"
all_xls <- list.files(study_dir, pattern = ".xlsx", recursive = T, full.names = T)
log_files <- all_xls[grepl("FieldLogs|Field_Logs", all_xls)] 

# df to store merged table product
result <- data.frame() 

for(file in log_files){ # This will take a while to run
  
  tempsheets <- excel_sheets(file) # store sheet names
  tempsheets <- tempsheets[!grepl("READ ME", tempsheets)] # discard readme sheets
  
  # Loop through sheets
  for(s in tempsheets){
    log_xls <- read_xlsx(file, sheet = s, col_names = F, na = c("N/A", "NA", "", "Not recorded"))
    
    # Data wrangling
    templog <- bind_rows(log_xls[1:2], log_xls[3:4],
                         by = c("...1" = "...3", "...2" = "...4")) %>% 
      mutate(site_id = log_xls$...2[1],
             core_id = s) %>% 
      slice(-1) %>% drop_na("...2")
    
      # Paste rows together
      result <- bind_rows(templog, result) %>% 
        filter(!("...1" == "...3"))
  }
}

# Widen result table from long format
wide_result <- result %>% 
  pivot_wider(id_cols = c(site_id, core_id), names_from = ...1, values_from = ...2) %>% 
  mutate(across(where(is.list), sapply, toString)) %>% 
  drop_na(core_id)

## Clean the dataframe
field_wo_cores <- wide_result %>% 
  mutate(species_code_v = ifelse(`Vegetation Type` == "Black needle rush", "Juncus roemerianus",
                               ifelse(`Vegetation Type` == "Black needle rush, black needle rush", "Juncus roemerianus",
                                      ifelse(`Vegetation Type` == "Black needle rush, Black needle rush", "Juncus roemerianus",
                                             ifelse(`Vegetation Type` == "black needle rush, black needle rush", "Juncus roemerianus",
                                                    ifelse(`Vegetation Type` == "Spartina (short)", "Spartina sp.",
                                                           ifelse(`Vegetation Type` == "Juncus", "Juncus sp.",
                                                                  ifelse(`Vegetation Type` == "Juncus (BNR)", "Juncus sp.", ""))))))),
         species_code_s = ifelse(`Vegetation/Sediment Type` == "Spartina alterniflora", "Spartina alterniflora",
                               ifelse(`Vegetation/Sediment Type` == "Spartina Alterniflora", "Spartina alterniflora",
                                      ifelse(`Vegetation/Sediment Type` == "Spartina", "Spartina sp.",
                                             ifelse(`Vegetation/Sediment Type` == "Blooming Juncus", "Juncus sp.",
                                                    ifelse(`Vegetation/Sediment Type` == "Pine/Juncus", "Juncus sp.",
                                                           ifelse(`Vegetation/Sediment Type` == "Juncus r.", "Juncus roemerianus",
                                                                  ifelse(`Vegetation/Sediment Type` == "Juncus", "Juncus sp.",
                                                                         ifelse(`Vegetation/Sediment Type` == "Juncus/Mud", "Juncus sp.",
                                                                                ifelse(`Vegetation/Sediment Type` == "Juncus / Mud - stiff", "Juncus sp.", 
                                                                                       ifelse(`Vegetation/Sediment Type` == "Distichlis", "Distichlis sp.",
                                                                                              ifelse(`Vegetation/Sediment Type` == "Transition, Spartina→Juncus", "Spartina and Juncus sp.", ""))))))))))),
         species_code = str_c(species_code_v, sep = "", species_code_s),
         code_type = ifelse(grepl("sp.", species_code), "Genus", 
                            ifelse(species_code == "", NA_character_, "Genus species")),
         full_date = ifelse(Date == 42504, "May 14, 2016",
                       ifelse(Date == 42505, "May 15, 2016",
                              ifelse(Date == 42506, "May 16, 2016",
                                     ifelse(Date == 42507, "May 17, 2016", 
                                            ifelse(Date == 42508, "May 18, 2016", 
                                                   ifelse(Date == "42507, May 16, 2016", "May 17, 2016", Date)))))),
         as_full_date = as.Date(full_date, "%B %d, %Y"),
         year = year(as_full_date),
         month = month(as_full_date),
         day = day(as_full_date),
         latitude = coalesce(`Latitude (DD)`, Latitude),
         longitude = coalesce(`Longitude (DD)`, Longitude),
         Salinity = as.numeric(Salinity),
         salinity_class = case_when(Salinity < 5 ~ 'oligohaline',
                                    Salinity < 18 ~ "mesohaline",
                                    Salinity < 30 ~ "polyhaline",
                                    Salinity < 40 ~ "mixoeuhaline",
                                    Salinity < 50 ~ "saline",
                                    Salinity > 50 ~ "brine",
                                    T ~ NA_character_),
         compaction_m = ifelse(`Compaction (m)` == "2 0.1", "0.1",
                               ifelse(`Compaction (m)` == "Not recorded", "", `Compaction (m)`)),
         compaction_m = as.numeric(compaction_m)*100,
         compaction_cm = as.numeric(`Compaction (cm)`),
         compaction_4 = as.numeric(`4" OD Compaction (cm)`),
         compaction_45 = as.numeric(`4.5" OD Compaction (cm)`),
         compaction = coalesce(compaction_m, compaction_cm, compaction_4, compaction_45)) %>% 
  separate(core_id, into = c("core_id_1", "core_id_2", "core_id_3"), sep = "-|_") %>%  # Fully separate core ids to reorganize below
  separate(site_id, into = "site_id", sep = "-")

field_raw <- field_wo_cores %>% # Resolve entries that list multiple cores on a single excel sheet into a unique id under "core_id_2"
  rbind(field_wo_cores %>% 
          filter(!is.na(core_id_3)) %>% 
          mutate(core_id_2 = core_id_3)) %>% 
  rbind(field_wo_cores %>% # Resolve the same for entries with 16CCT03 site ids
          filter(!is.na(core_id_2)) %>% 
          filter(!grepl('16CCT03', core_id_1)) %>% 
          mutate(core_id_2 = core_id_1)) %>% 
  mutate(core_id_2 = ifelse(is.na(core_id_2), core_id_1, core_id_2), # Put the core ids into the same (#2) column
         core_id_2 = gsub("DMRS", "D,M,R,S", core_id_2),
         core_id_2 = gsub(" |&", "", core_id_2)) %>% 
  separate(core_id_2, into = c("core_id_2", "core_type"), sep = ",", extra = "merge") %>% # Pull out core type ids (DMRSVG)
  separate_rows(core_type, sep = ",") %>% # Disaggregate cores into rows
  mutate(flag = ifelse(substr(core_id_2, nchar(core_id_2), nchar(core_id_2)) == "B", 1, 0)) %>% # Flag "B" cores (no associated data)
  filter(flag < 1) # Remove "B" cores
  
df_field <- field_raw %>% 
  rbind(field_raw %>% 
          filter(!is.na(core_type)) %>% 
          mutate(core_id_2 = substr(core_id_2, 1, nchar(core_id_2)-1),
                 core_id_2 = paste(core_id_2, core_type, sep = ""))) %>% 
  rename(core_id = core_id_2) %>% 
  mutate(core_id = ifelse(core_id == "241G", "GB241G",
                          ifelse(core_id == "243G", "GB243G", 
                                 ifelse(grepl("14GB", core_id), substr(core_id, 3, nchar(core_id)), core_id))),
         core_id = gsub("GB257V[(]B[)]", "GB257V_2", core_id),
         core_id = ifelse(core_id == "GB254G" & day == 16, "GB254G",
                          ifelse(core_id == "GB254G" & day == 17, "GB254G_2", core_id))) # 254G has two position measurements
         

## Step 4: Site Information ####

# Create a call path
study_dir <- "./data/primary_studies/Marot_et_al_2020/original"
all_xlsx <- list.files(study_dir, pattern = ".xlsx", recursive = T, full.names = T)
site_files <- all_xlsx[grepl("SiteInformation", all_xlsx)] 

## 2014 
site_2014 <- read_excel(site_files[1], skip = 2, na = c("NA", "--")) %>% 
  slice_head(n = 38) %>% 
  separate("Site ID", into = c("site_id", "core_id", "core_id_2"), sep = "-|& -") %>% 
  rename(date = "Date \r\nCollected",
         latitude_wgs84 = "Latitude (WGS84)",
         longitude_wgs84 = 'Longitude (WGS84)',
         latitude_nad83 = "Latitude (NAD83)",
         longitude_nad83 = "Longitude (NAD83)",
         elevation = "Orthometric Height\r\n(m NAVD88, Geoid 12A)",
         salinity = "Salinity")

## 2015 
site_2015 <- read_excel(site_files[2], skip = 2, na = "--") %>% 
  slice_head(n = 37) %>% 
  separate("Site ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(date = "Date \r\nCollected",
         latitude_wgs84 = "Latitude (WGS84)",
         longitude_wgs84 = "Longitude (WGS84)",
         salinity = "Salinity") 

## 2016 
site_2016_3 <- read_excel(site_files[3], skip = 2, na = "--") %>% 
  slice_head(n = 42) %>% 
  separate("Site ID", into = c("site_id", "core_id", "core_id_2"), sep = "-|& -") %>% 
  rename(date = "Date\r\nCollected",
         latitude_wgs84 = "Latitude (WGS84)",
         longitude_wgs84 = 'Longitude (WGS84)',
         latitude_nad83 = "Latitude (NAD83)",
         longitude_nad83 = "Longitude (NAD83)",
         elevation = "Orthometric Height (m, NAVD88, \r\nGEOID 12A)",
         salinity = "Salinity")

site_2016_4 <- read_excel(site_files[4], skip = 1) %>% 
  separate("Site ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(date = "Date & Time\r\nCollected",
         latitude_nad83 = "Latitude (NAD83)",
         longitude_nad83 = "Longitude (NAD83)") %>% 
  mutate(core_id = paste(core_id, "G", sep = "")) # Defined as surface sample ("G") in metadata

site_2016_7 <- read_excel(site_files[5], skip = 1) %>% 
  separate("Site ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(date = "Date \r\nCollected",
         latitude_nad83 = "Latitude (NAD83)",
         longitude_nad83 = "Longitude (NAD83)",
         elevation = "Orthometric Height\r\n(m, NAVD88 Geoid 12A)") %>% 
  mutate(core_id = paste(core_id, "M", sep = "")) # Defined as push core ("M") in metadata

## Merge Site Information
site_join <- function(){
  joiners <- c("site_id", "core_id", "date", "latitude_wgs84", "longitude_wgs84", "latitude_nad83", 
               "longitude_nad83", "core_id_2", "Site Description",  "salinity", "Type of Samples Collected", 
               "elevation")
  site_info_list <- list(site_2014, site_2015, site_2016_3, site_2016_4, site_2016_7)
  df <- data.frame()
  
  for (site in site_info_list) {
    names_of_site <- intersect(joiners, names(site))
    col_intersect <- intersect(names(df), names_of_site)
    temp.df <- site 
    df <- full_join(df, temp.df, by = col_intersect) 
  }
  return(df)
}

df_site <- site_join() %>% 
  rbind(site_join() %>% 
          filter(!is.na(core_id_2)) %>% 
          mutate(core_id = core_id_2)) %>% 
  rename(samples = "Type of Samples Collected",
         site_description = "Site Description") %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         habitat = ifelse(site_description == "Marsh", "marsh", NA),
         salinity_class = case_when(salinity < 5 ~ 'oligohaline',
                                    salinity < 18 ~ "mesohaline",
                                    salinity < 30 ~ "polyhaline",
                                    salinity < 40 ~ "mixoeuhaline",
                                    salinity < 50 ~ "saline",
                                    salinity > 50 ~ "brine",
                                    T ~ NA_character_),
         core_id = gsub("[*]", "", core_id),
         core_id = gsub("254M|254V", "254", core_id), # This core is double named in the datasheet 
         core_id = gsub(" ", "", core_id),
         samples = gsub(" ", "", samples)) %>% 
  separate_rows(samples, sep = ",") %>%
  mutate(samples = ifelse(core_id == "GB254" & day == 16, "G", # These two GB254 cores match those of df_field but not df_sediment
                          ifelse(core_id == "GB254" & day == 17, "G_2", samples))) %>% 
  filter(!grepl("B", samples)) %>% # Remove "B" non-cores
# Don't assign the core_length_flag until after the merge to make the cores table
#  mutate(core_length_flag = ifelse(samples == "M", "core depth represents deposit depth", 
#                                   ifelse(samples == "R", "core depth represents deposit depth",
#                                          ifelse(samples == "V", "core depth represents deposit depth", 
#                                                 "core depth limited by length of corer")))) %>% 
  unite(col = "core_id", "core_id", samples, sep = "", na.rm = T) %>% 
  mutate(core_id = gsub("GB30S", "GB30S_2", core_id)) # Rename because this salinity measurement is distinct from GB30S in df_field

## Step 5: Radiocarbon #### 
radio_14 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_Radiocarbon/14CCT01_Radiocarbon.xlsx", skip = 1) %>% 
  separate("Core ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(c14_age = "Age",
         c14_age_se = "Age\r\nError",
         c14_material = "Sample\r\nType",
         date = "Date\r\nReported",
         depth_max = "Depth\r\n(cm)") %>% 
  mutate(depth_min = depth_max - 1)
  
radio_16 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_Radiocarbon/16CCT03_Radiocarbon.xlsx", skip = 1, na = c("--", "N/A")) %>% 
  slice(1:(n()-1)) %>% 
  separate("Sample ID", into = c("site_id", "core_id"), sep = "-") %>% 
  separate("Depth\r\n(cm)", into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(c14_age = gsub("> ", "", Age),
         c14_age = as.numeric(c14_age),
         depth_max = as.numeric(depth_max),
         depth_min = as.numeric(depth_min)) %>% 
  rename(c14_material = "Sample\r\nType",
         c14_age_se = "Age\r\nError",
         date = "Date\r\nReported")

df_radio <- full_join(radio_14, radio_16, by = c("site_id", "core_id", "c14_material", "c14_age", "c14_age_se", "date", "depth_min", "depth_max"))


## Step 6: Make the Depthseries table ####
depth_joins <- c("depth_min", "depth_max", "site_id", "core_id")
depthseries_raw <- bind_rows(df_sediment, df_gamma) %>% 
  bind_rows(df_radio) %>% 
  bind_rows(df_alpha) %>% 
  mutate(study_id = "Marot_et_al_2020",
         core_id = gsub("GB232M[(]A[)]", "GB232M", core_id), # Resolve duplicate naming conventions
         core_id = gsub("GB232M[(]B[)]", "GB232M_2", core_id),
         core_id = gsub("GB53M[(]A[)]", "GB53M", core_id),
         core_id = gsub("GB53M[(]B[)]", "GB53M_2", core_id),
         core_id = paste(site_id, core_id, sep = "_"),
         core_method = substr(core_id, nchar(core_id) - 2, nchar(core_id)),
         method_id = ifelse(is.na(method_id), "no spectroscopy", method_id))

depthseries <- depthseries_raw %>% 
  mutate(core_method = ifelse(grepl("D", core_method), "shovel_corer",
                              ifelse(grepl("M", core_method), "push_core",
                                     ifelse(grepl("R", core_method), "russian_corer",
                                            ifelse(grepl("S", core_method), "surface_sample",
                                                   ifelse(grepl("V", core_method), "vibracore",
                                                          ifelse(grepl("G", core_method), "surface sample", "")))))),
         method_id = paste(core_method, method_id, sep = "_")) %>% 
  select(c(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, 
         fraction_organic_matter, cs137_activity, cs137_activity_se, cs137_unit, total_pb210_activity, 
         total_pb210_activity_se, pb210_unit, ra226_activity, ra226_activity_se, ra226_unit, c14_age, 
         c14_age_se, c14_material, be7_activity, be7_activity_se, be7_unit)) 


## Step 7: Make the Cores table ####
cores <- full_join(df_field, df_site, by = c("site_id", "core_id", "year", "month", "day", "salinity_class")) %>% 
  mutate(core_id = paste(site_id, core_id, sep = "_"),
         elevation_accuracy = .014,
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         study_id = "Marot_et_al_2020",
         elevation_datum = "NAVD88",
         elevation_method = "other high resolution",
         position_method = "other high resolution",
         salinity_method = "measurement",
         core_length_flag = ifelse(grepl("M", core_id), "core depth represents deposit depth", 
                                   ifelse(grepl("R", core_id), "core depth represents deposit depth",
                                          ifelse(grepl("V", core_id), "core depth represents deposit depth", 
                                                 "core depth limited by length of corer")))) %>%
  bind_rows(depthseries_raw) %>% # Add 19 core IDs from depthseries data (this will only add their IDs and no cores-table related data)
  mutate(inundation_class = ifelse(grepl("G", substr(core_id, nchar(core_id) - 2, nchar(core_id))), "low", NA),
         inundation_method = "field observation") %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude, longitude, position_method,
           elevation, elevation_datum, elevation_accuracy, elevation_method, salinity_class, salinity_method, 
           habitat, inundation_class, inundation_method, core_length_flag)) %>% 
  distinct()

cores <- cores %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise_all("first") %>% 
  ungroup() %>% 
  mutate(year = ifelse(is.na(year), 2000 + as.numeric(substr(core_id, 1,2)), year)) %>% 
  filter(core_id %in% unique(depthseries$core_id))


## Step 8: Make the Species table ####
species <- full_join(df_field, df_site, by = c("site_id", "core_id")) %>% 
  mutate(core_id = paste(site_id, core_id, sep = "_"),
         study_id = "Marot_et_al_2022") %>% 
  select(c(study_id, site_id, core_id, species_code, code_type, habitat)) %>% 
  filter(core_id %in% depthseries$core_id)


## Step 9: Make the Methods table ####
methods <- read_excel("data/primary_studies/Marot_et_al_2020/original/Marot_et_al_2020_methods.xlsx", sheet = 2, na = "NA") %>% 
  select(where(notAllNA))


## Step 10: QAQC ####

## Mapping
leaflet(cores) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 3, label = ~core_id)

## Table testing
table_names <- c("cores", "depthseries", "methods", "species")

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
testNumericCols(depthseries)


## Step 11: Bibliography ####
study_citation <- data.frame(study_id = "Marot_et_al_2020",
                            bibliography_id = "Marot_et_al_2020_data",
                            title = "Sedimentary data from Grand Bay, Alabama/Mississippi, 2014–2016 (ver. 1.1, April 2020): U.S. Geological Survey data release",
                            author = "Marot, M.E., Smith, C.G., McCloskey, T.A., Locker, S.D., Khan, N.S., and Smith, K.E.L.",
                            publication_type = "primary dataset",
                            doi = "10.5066/P9FO8R3Y",
                            url = "https://doi.org/10.5066/P9FO8R3Y",
                            bibtype = "Misc",
                            year = "2020",
                            month = "apr",
                            day = "28")

bib_file <- study_citation %>%
  remove_rownames() %>% 
  select(-c(study_id, publication_type)) %>% 
  column_to_rownames("bibliography_id")

# JH: I want to overwrite the site_id
# These are all grand bay and the sites don't seem to represent any type of unique gradient or sampling locations within
cores <- mutate(cores, site_id = "Grand Bay")
depthseries <- mutate(depthseries, site_id = "Grand Bay")
species <- mutate(species, site_id = "Grand Bay")

## Step 12: Write curated data ####
write_csv(cores, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_cores.csv") 
write_csv(depthseries, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_species.csv")
write_csv(methods, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_methods.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_study_citations.bib")
write_csv(study_citation, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_study_citations.csv")



