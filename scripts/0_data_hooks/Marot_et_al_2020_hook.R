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
  i <- NULL
  # Read sheets 2:12
  for (i in 2:12) {
    sheet <- i
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
a <- NULL
b <- NULL
sed_depth_files <- list("./data/primary_studies/Marot_et_al_2020/original/2016-358-FA/16CCT07_SedimentPhysicalProperties/16CCT07_SedimentPhysicalProperties.xlsx", 
                        "./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_SedimentPhysicalProperties/16CCT03_SedimentPhysicalProperties.xlsx")
for (a in sed_depth_files) {
  sheets <- excel_sheets(a)
    for (b in sheets) {
    temp.df <- read_xlsx(a, sheet = b, skip = 1, na = "--") 
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
  separate(depth, into = c("depth_min", "depth_max"), sep = "-") %>% 
  mutate(depth_max = sub("[*]|a", "", depth_max),
         depth_max = ifelse(depth_min == "Top*", 1, depth_max),
         depth_min = ifelse(depth_min == "Top*", 0, 
                            ifelse(depth_min == "Bulk", NA, depth_min)),
         depth_max = as.numeric(depth_max),
         depth_min = as.numeric(depth_min))

## Merge all Sediment data
df_sediment <- full_join(sediment_with_depth, sediment_ss) %>% 
  separate(core_id, into = c("site_id", "core_id"), sep = "-", remove = T) %>% 
  drop_na(core_id)


## Step 2: Alpha Spectroscopy ####

## 2014 & 2015: this is full-core pb210 dating without other associated information in other datasets
alpha_files <- list("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_AlphaSpectroscopy/14CCT01_AlphaSpectroscopy.xlsx",
                    "./data/primary_studies/Marot_et_al_2020/original/2015-315-FA/15CCT02_AlphaSpectroscopy/15CCT02_AlphaSpectroscopy.xlsx")
df <- data.frame()
for (a in alpha_files) {
  temp.df <- read_xlsx(a, skip = 1) 
  df <- rbind(df, temp.df)  
}

alpha_merge <- df %>% 
  mutate(depth_min = 0,
         depth_max = "max")

## 2016: this has depth data
alpha_raw <- data.frame()
i <- NULL
for (i in sheets) {
  alpha_path <- "./data/primary_studies/Marot_et_al_2020/original/2016-331-FA/16CCT03_AlphaSpectroscopy/16CCT03_AlphaSpectroscopy.xlsx"
  sheets <- excel_sheets(alpha_path)
  temp.df <- read_xlsx(alpha_path, sheet = i, skip = 1) 
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
         total_pb210_activity_se = "Total Pb-210 Error \r\n(+/- dpm/g)") %>% 
  filter(!grepl("=", core_id)) %>% 
  mutate(method_id = "alpha spectroscopy") %>% 
  separate(core_id, into = c("site_id", "core_id"), sep = "-", remove = T)


## Step 3: GammaSpectroscopy ####

# Read in docs without Be data
gamma_raw <- data.frame()
a <- NULL
b <- NULL
gamma_files <- list("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_GammaSpectroscopy/14CCT01_GammaSpectroscopy.xlsx",
                    "./data/primary_studies/Marot_et_al_2020/original/2016-358-FA/16CCT07_GammaSpectroscopy/16CCT07_GammaSpectroscopy.xlsx") 
for (a in gamma_files) {
  sheets <- excel_sheets(a)
  for (b in sheets) {
    temp.df <- read_xlsx(a, sheet = b, skip = 1, na = c("ND", "Tr", "--")) 
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
  mutate(cs137_unit = "dpm/g",
         pb210_unit = "dpm/g",
         method_id = "gamma spectroscopy",
         ra226_unit = "dpm/g",
         be7_unit = "dpm/g") %>% 
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
                                                                                ifelse(`Vegetation/Sediment Type` == "Juncus / Mud - stiff", "Juncus sp.", ""))))))))),
         species_code = str_c(species_code_v, sep = "", species_code_s),
         code_type = ifelse(grepl("sp.", species_code), "Genus", 
                            ifelse(species_code == "", "", "Genus species")),
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
         position_method = ifelse(`Handheld GPS used` == "", "", "handheld"),
         salinity_class = case_when(Salinity < 5 ~ 'mesohaline',
                                    Salinity < 18 ~ "polyhaline",
                                    T ~ ""),
         salinity_method = "measurement",
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
  separate_rows(core_type, sep = ",") # Disaggregate cores into rows
  
df_field <- field_raw %>% 
  rbind(field_raw %>% 
          filter(!is.na(core_type)) %>% 
          mutate(core_id_2 = substr(core_id_2, 1, nchar(core_id_2)-1),
                 core_id_2 = paste(core_id_2, core_type, sep = ""))) %>% 
  rename(core_id = core_id_2) %>% 
  mutate(core_id = ifelse(core_id == "241G", "GB241G",
                          ifelse(core_id == "243G", "GB243G", 
                                 ifelse(grepl("14GB", core_id), substr(core_id, 3, nchar(core_id)), core_id))))


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
         longitude_nad83 = "Longitude (NAD83)")

site_2016_7 <- read_excel(site_files[5], skip = 1) %>% 
  separate("Site ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(date = "Date \r\nCollected",
         latitude_nad83 = "Latitude (NAD83)",
         longitude_nad83 = "Longitude (NAD83)",
         elevation = "Orthometric Height\r\n(m, NAVD88 Geoid 12A)")

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
  rename(samples = "Type of Samples Collected") %>% 
  mutate(study_id = "Marot_et_al_2020",
         year = year(date),
         month = month(date),
         day = day(date),
         salinity_class = case_when(salinity < 5 ~ 'oligohaline',
                                    salinity < 18 ~ "mesohaline",
                                    salinity < 30 ~ "polyhaline",
                                    salinity < 40 ~ "mixoeuhaline",
                                    salinity < 50 ~ "saline",
                                    salinity > 50 ~ "brine",
                                    T ~ ""),
         salinity_method = ifelse(salinity > 0, "measurement", ""),
         elevation_datum = "NAVD88",
         position_method = "handheld",
         core_id = gsub("[*]|M|V", "", core_id),
         core_id = gsub(" ", "", core_id),
         samples = gsub(" ", "", samples)) %>% 
  separate_rows(samples, sep = ",") %>% 
  filter(!grepl("B", samples)) %>% # Remove non-cores
  mutate(core_length_flag = ifelse(samples == "M", "core depth represents deposit depth", 
                                   ifelse(samples == "R", "core depth represents deposit depth",
                                          ifelse(samples == "V", "core depth represents deposit depth", 
                                                 "core depth limited by length of corer")))) %>% 
  unite(col = "core_id", "core_id", samples, sep = "", na.rm = T)
  

## Step 5: Radiocarbon #### 
radio_14 <- read_excel("./data/primary_studies/Marot_et_al_2020/original/2014-323-FA/14CCT01_Radiocarbon/14CCT01_Radiocarbon.xlsx", skip = 1) %>% 
  separate("Core ID", into = c("site_id", "core_id"), sep = "-") %>% 
  rename(c14_age = "Age",
         c14_age_se = "Age\r\nError",
         c14_material = "Sample\r\nType",
         date = "Date\r\nReported",
         depth_max = "Depth\r\n(cm)")
  
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

df_radio <- full_join(radio_14, radio_16, by = c("site_id", "core_id", "c14_material", "c14_age", "c14_age_se", "date", "depth_max"))

## Step 6: Make the Depthseries table ####
depth_joins <- c("depth_min", "depth_max", "site_id", "core_id")
depthseries_raw <- full_join(df_sediment, df_gamma, by = depth_joins) %>% 
  full_join(df_radio, by = depth_joins) %>% 
  full_join(df_field, by = c("site_id", "core_id")) %>% 
  full_join(df_site, by = c("site_id", "core_id", "year", "month", "day", "salinity_class", "salinity_method", "position_method")) %>% 
  full_join(df_alpha, by = c(depth_joins, "total_pb210_activity", "total_pb210_activity_se", "method_id")) %>% 
  fill(study_id, .direction = "downup") %>%
  mutate(core_id = gsub("[(]|[)]|,", "", core_id)) 

depthseries <- depthseries_raw %>% 
  select(c(study_id, site_id, core_id, method_id, depth_min, depth_max, dry_bulk_density, 
         fraction_organic_matter, cs137_activity, cs137_activity_se, cs137_unit, total_pb210_activity, 
         total_pb210_activity_se, pb210_unit, ra226_activity, ra226_activity_se, ra226_unit, c14_age, 
         c14_age_se, c14_material, be7_activity, be7_activity_se, be7_unit, compaction)) 


## Step 7: Make the Cores table ####
cores <- depthseries_raw %>% 
  mutate(elevation_accuracy = .014,
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  select(c(study_id, site_id, core_id, year, month, day, latitude,longitude, position_method, 
           elevation, elevation_datum, elevation_accuracy, salinity_class, salinity_method, 
           core_length_flag))


## Step 8: Make the Species table ####
species <- depthseries_raw %>% 
  select(c(study_id, site_id, core_id, species_code, code_type))


## Step 9: Make the Methods table ####
methods <- read_excel("data/primary_studies/Marot_et_al_2020/original/Marot_et_al_2020_methods.xlsx", col_names = T)


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


## Step 12: Write curated data ####
write_csv(cores, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_cores.csv") 
write_csv(depthseries, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_depthseries.csv")
write_csv(species, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_species.csv")
write_csv(methods, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_methods.csv")
WriteBib(as.BibEntry(bib_file), "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_study_citations.bib")
write_csv(study_citations, "data/primary_studies/Marot_et_al_2020/derivative/Marot_et_al_2020_study_citations.csv")



