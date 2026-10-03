# Coastal Carbon Research Coordination Network
# Hook for Krauss 2018, jointly paired with Jones 2017
# Contact: klingesd@si.edu
#          
## Citations
# Krauss, K.W., Noe, G.B., Duberstein, J.A., Conner, W.H., Stagg, C.L., Cormier, 
# N., Jones, M.C., Bernhardt, C.J., Lockaby, B.G., From, A.S., Doyle, T.W., Day, 
# R.H., Ensign, S.H., Pierfelice, K.N., Hupp, C.R., Chow, A.T., and Whitbeck, J.L., 
# 2018, The role of the upper tidal estuary in wetland blue carbon storage and 
# flux: Global Biogeochemical Cycles, v. 32, no. 5, p. 817-839, 
# https://doi.org/10.1029/2018GB005897.

# Krauss 2018 data release:
# 10.5066/F7TM7930
# Jones et al 2007 DOI
# 10.1002/2017JG004015 (methods coding)

## 0. Workspace prep ###################

# NOTE: 

# Not all of these packages will be helpful for every hook:
# - if you don't download the data, you can remove the rvest and RCurl
# - if you don't need to import excel spreadsheets to R (as in, your data is in
#   .csv or tab-delimited formats). you can remove readxl
# - if you won't be working with date or time series data, you can remove lubridate
# - you will always be using tidyverse. If you aren't then you're doing something
#   wrong.
library(tidyverse)
library(readxl)
# library(rvest)
# library(RCurl)
library(lubridate)
library(RefManageR)

## 1. Download data #####################

## ARCHIVED DATA DOWNLOADING WORKFLOW

# # 1. Designate the target webpage to scrape for data
# #   Paste the url of the target webpage here, wrapped in quotation marks
# 
# URL <- "https://www.sciencebase.gov/catalog/item/59e9f79de4b05fe04cd690a1"
# 
# # 2. Name the files
# #   Add the names for each file into this list, wrapped in quotation marks, IN 
# #   THE SAME ORDER THAT THEY ARE LISTED ON THE WEBPAGE ITSELF. Include the file
# #   extension (.csv, .xlsx, etc.) in the name of the file as well.
# 
# FILE_NAMES <- list("TFFW_Carbon_Budget_GHG.csv", "TFFW_Carbon_Budget_Root_Ingrowth.csv",
#                    "TFFW_Carbon_Budget_Stand_Structure_2005_and_2012.csv", 
#                    "TFFW_Carbon_Budget_Wood_Increment_and Litterfall_2005_2015.csv",
#                    "TFFW_Carbon_Budget_WoodyDebris.csv",
#                    "TFFW_elemental_carbon.csv",
#                    "TFFW_Radiocarbon.csv",
#                    "TFFW_soil_core_data.csv",
#                    "TFFW_Carbon_metadata.xml"
# )
# 
# # 3. Designate file path of where these data files will go in the CCRCN library
# #   Paste the file path here, wrapped in quotation marks. The getwd() function 
# #   will automatically detect the working directory of the R project (in the case 
# #   of the CCRCN Data library, the location of where this repository is stored on 
# #   your local drive + "CCRCN-Data-Library"), which will be pasted in combination
# #   with whatever you include within the quotation marks.
# 
# FILE_PATH <- paste0(getwd(), "/data/primary_studies/Krauss_2018/original", "/" )
# 
# # 4. Designate the html node and attri that the web scraper will search for. This
# #   will correspond to what classes of div's the data URL(s) are held in. You can
# #   find out this information by inspecting the web page (right click + inspect)
# #   or using a web browser plug-in (e.g., Google Chrome Web Scraper:
# #   https://chrome.google.com/webstore/detail/web-scraper/jnhgnonknehpejjnehehllkliplmbmhn?hl=en)
# 
# HTML_NODE <- ".sb-download-link"
#   
#   HTML_ATTR <- "data-url"
#   
#   # The stem of the url should always be the same
#   BASE_URL <- "https://www.sciencebase.gov"
# 
# print("If you are downloading data, un-comment the following lines")
# page <- read_html(URL)
# 
# # Extract the url paths for each data file embedded on the webpage, and save
# #   those paths to a list
# url_list <- page %>%
#   html_nodes(HTML_NODE) %>%
#   html_attr(HTML_ATTR)
# 
# # For each data file path on the webpage....
# # for (i in 1:length(url_list)) {
# # 
# #   # ...extract and download file
# #   download.file(paste0(BASE_URL, url_list[[i]]), paste0(FILE_PATH, FILE_NAMES[[i]]),
# #                 mode = "wb")
# # }

## 2. Read in data #####################


## ....2a. Read in Krauss data ############
# The original data from the table is stored in the Drexler_et_al_2009_peat_accretion_age_depth.csv file. 
# Because the site-core-sample ID field could not easily be separated using tidy logic, I did it manually in excel
# Additionally, the negative sign character used in the elevation field was causing problems in both excel and R and was replaced with (-)
# That edited file is read in 
# Different computers are having trouble parsing the column names with spaces, leading to errors during cutation. 
# Renaming columns in the read_excel call and skipping the first line which represents the old column names

# Designate column types 
# Does the imported data have column names?

# NOTE: no need to import the same data file twice. so you may not necessarily
#   have an impact_raw, species_raw, etc. if all of that data is coming from the
#   same data file (or is being digitized/generated by hand)

# There also may be multiple data files corresponding to a single data level 
#   (e.g. two files, both with depthseries data, which will be joined together)
depthseries_DBD_raw <- read_csv("./data/primary_studies/Krauss_2018/original/TFFW_soil_core_data.csv", 
                              col_names = TRUE, 
                              col_types = cols(
                                "Compression (%)" = col_double()
                              ) 
                            )

depthseries_carbon_raw <- read_csv("./data/primary_studies/Krauss_2018/original/TFFW_elemental_carbon.csv")

radiocarbon_raw <- read_csv("./data/primary_studies/Krauss_2018/original/TFFW_Radiocarbon.csv")

# Few datasets will include biomass, at the time of writing at least
root_productivity <- read_csv("data/primary_studies/Krauss_2018/original/TFFW_Carbon_Budget_Root_Ingrowth.csv")

species_edited <- read_csv("data/primary_studies/Krauss_2018/intermediate/Krauss_et_al_2018_species.csv")

## ....2b. Read in Jones data ###############

# Downloaded geochrono datasets
geochron_oligo <- read_csv("./data/primary_studies/Jones_2017/intermediate/dataset25338.csv")
geochron_heavy_salt <- read_csv("./data/primary_studies/Jones_2017/intermediate/dataset25345.csv")
geochron_mod_salt <- read_csv("./data/primary_studies/Jones_2017/intermediate/dataset25364.csv")

# Downloaded LOI datasets
LOI_oligo_raw <- read_csv("./data/primary_studies/Jones_2017/original/dataset25339.csv")
LOI_heavy_salt_raw  <- read_csv("./data/primary_studies/Jones_2017/original/dataset25346.csv")
LOI_mod_salt_raw  <- read_csv("./data/primary_studies/Jones_2017/original/dataset25365.csv")

## 3. Curate data ######################

# Use curation functions if you need
source("./scripts/1_data_formatting/curation_functions.R")
source("./scripts/1_data_formatting/qa_functions.R")


id <- "Krauss_et_al_2018"

## ....3A. Depthseries data ##################

## ......3Aa. Prep depthseries from Krauss #####################
# Prep radiocarbon data to join to DBD data
radiocarbon <- radiocarbon_raw %>%
  separate("Depth (cm)", into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(depth_max = as.double(depth_max), depth_min = as.double(depth_min))

Krauss_depthseries <- depthseries_DBD_raw %>%
  rename(depth_max = "Depth (cm)") %>% # See readme for assumptions on depth
  mutate(depth_min = depth_max - 1) %>% # See readme for assumptions on depth

  # Now join in the radioncarbon data before we rename anything
  full_join(radiocarbon,  by = c("River", "Core ID", "depth_min", "depth_max")) %>%

  # Ok, now good to rename
  rename(core_id = "Core ID", dry_bulk_density = "Dry bulk density (g/cc)",
         "fraction_organic_matter" = "LOI (%)", site_id = River, 
         c14_material = "Material Dated", compaction_fraction = "Compression (%)",
        c14_age = "14C Age", c14_age_sd = "SE") %>% 
  
  # Convert compaction percent to fraction
  mutate(compaction_fraction = compaction_fraction / 100) %>% 
  
  # Add study id
  mutate(study_id = id) %>%
  
  # Recode cores
  mutate(core_id = recode(core_id, "11-11-2-1" = "turkey_creek_1",
                          "11-11-2-3" = "turkey_creek_2",
                          "11-11-3-1" = "butler_island_1",
                          "11-11-1-2" = "richmond_island_1",
                          "12-12-10-3" = "savannah_mid_1",
                          "12-12-10-2" = "savannah_low_1",
                          "12-12-9-3" = "savannah_mid_2",
                          "12-12-11-1" = "savannah_high_1"
  )) %>%
  
  # Each core represents differente veg/salinity/etc. spectra, so each core will
  #   be a distinct site
  # mutate(site_id = recode(core_id, "turkey_creek_1" = "Waccamaw_River_Turkey_creek_high",
  #                         "turkey_creek_2" = "Waccamaw_River_Turkey_creek_low",
  #                         "butler_island_1" = "Waccamaw_River_Butler_island",
  #                         "richmond_island_1" = "Waccamaw_River_Richmond_island",
  #                         "savannah_mid_1" = "Savannah_River_mid",
  #                         "savannah_low_1" = "Savannah_River_low",
  #                         "savannah_mid_2" = "Savannah_River_mid",
  #                         "savannah_high_1" = "Savannah_River_high"
  # )) %>%
  # 
  # Some c14 ages yielded post-modern dates. set to NA and add a note
  mutate(c14_notes = ifelse(c14_age == ">Modern", "c14 age yielded greater than modern day", NA)) %>%
  mutate(c14_age = ifelse(c14_age == ">Modern", NA, c14_age)) %>%
  # Now coerce ages to numeric
  mutate(c14_age = as.double(c14_age)) %>%
  
  # Convert percent to fraction
  mutate(fraction_organic_matter = fraction_organic_matter / 100) %>%
  
  # Add 'River' to site names
  # mutate(site_id = paste(site_id, "River", sep = "_")) %>%
  select(-"Lab ID")
  

## ......3Ab. COMMENTED OUT Prep pb210-dated core ##################

# The Krauss data comes with a Pb-210 dated core, which is one of the cores already
#   representd in Noe et al 2016. Although the Noe et al 2016 is not currently
#   public (in the working group repo), we'll leave this core out here operating
#   under the assumption that the Noe cores will make their way into the library
#   soon.
# pb_210_core <- depthseries_carbon_raw %>%
#   # Quick rename
#   rename(core_id = Core) %>%
#   # Assuming site is Waccamaw because core id is "W2"
#   mutate(site_id = "Waccamaw_River") %>%
#   # create new cols as converted versions of old cols
#   mutate(total_pb210_activity = convert_dpm_g_to_bec_kg(`Total Pb-210 activity (dpm/g)`)) %>%
#   mutate(fraction_carbon = `%C` / 100) %>%
#   mutate(fraction_organic_matter = `%LOI` / 100) %>%
#   
#   # Parse out depth to min and max and coerce to numeric
#   separate(Depth_cm, into = c("depth_min", "depth_max"), sep= "-") %>%
#   mutate(depth_min = as.double(depth_min)) %>%
#   mutate(depth_max = as.double(depth_max)) %>%  
#   
#   select(site_id, core_id, depth_min, depth_max, fraction_organic_matter, fraction_carbon,
#          total_pb210_activity)
#  
# Join pb210 core to other cores
# Krauss_depthseries <- Krauss_depthseries %>%
#   bind_rows(pb_210_core)

## ......3Ab. Prep depthseries from Jones to join age models  #####################

# only including if resolved to join in c14 ages from Jones 2017
geochron_oligo <- geochron_oligo %>%
  mutate(core_id = "turkey_creek_1")

geochron_heavy_salt <- geochron_heavy_salt %>%
  mutate(core_id = "turkey_creek_2")

geochron_mod_salt <- geochron_mod_salt %>%
  mutate(core_id = "butler_island_1")

# source("./scripts/1_data_formatting/curation_functions.R")
geochron <- geochron_oligo %>%
  bind_rows(geochron_heavy_salt) %>%
  bind_rows(geochron_mod_salt) %>%
  mutate(study_id = "Jones_et_al_2017") %>%
  
  # Checking against Krauss depths, which are more informative, here's how to convert
  rename(depth_min = Depth) %>%
  mutate(depth_max = depth_min + Thickness) %>%
  rename(sample_id = SampleID, c14_age = Age, c14_material = MaterialDated,
         c14_age_sd = ErrorOlder) %>%
  mutate(c14_age = ifelse(c14_age == 0, NA, c14_age)) %>%
  mutate(sample_id = as.character(sample_id)) %>%
  mutate(c14_age = ifelse(c14_age == 0, NA, c14_age),
         sample_id = paste0("S", sample_id)) %>%
  select(study_id, core_id, sample_id, depth_min, depth_max, c14_age, c14_age_sd,
        c14_material)

# Coerce into matrix and transpose
LOI_oligo <- t(as.matrix(LOI_oligo_raw))
LOI_oligo <- as.data.frame(LOI_oligo)
LOI_oligo <- as_tibble(LOI_oligo, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "turkey_creek_1")

# Coerce into matrix and transpose
LOI_heavy_salt <- t(as.matrix(LOI_heavy_salt_raw))
LOI_heavy_salt <- as.data.frame(LOI_heavy_salt)
LOI_heavy_salt <- as_tibble(LOI_heavy_salt, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "turkey_creek_2")

# Coerce into matrix and transpose
LOI_mod_salt <- t(as.matrix(LOI_mod_salt_raw))
LOI_mod_salt <- as.data.frame(LOI_mod_salt)
LOI_mod_salt <- as_tibble(LOI_mod_salt, rownames = NULL) %>%
  slice(-1:-5) %>%
  mutate(core_id = "butler_island_1")

# Join LOI datasets
LOI <- LOI_oligo %>%
  bind_rows(LOI_heavy_salt) %>%
  bind_rows(LOI_mod_salt) %>%
  separate(`V1`, into = c("depth_min", "depth_max"), sep = "-") %>%
  mutate(depth_min = as.double(depth_min)) %>%
  mutate(depth_max = as.double(gsub(" cm", "", depth_max))) %>%
  separate("V6", into = c("age_max", "age", "age_min"), sep = "/") %>%
  rename(sample_id = "V5") %>%
  mutate(age_max = as.double(age_max), age = as.double(age),
         age_min = as.double(age_min),
         age_depth_model_reference = "YBP",
         sample_id = paste0("S", sample_id)) 

Jones_depthseries <- geochron %>%
  full_join(LOI, by = c("core_id", "sample_id", "depth_min", "depth_max")) %>%
  arrange(core_id, depth_min) %>%
  select(core_id, sample_id, depth_min, depth_max,
         age, age_min, age_max) %>%
  
  # Some depth intervals are duplicated, because there are 2 samples take from
  #   these intervals it seems...however only 1/2 of the samples for these intervals
  #   have modeled ages, and because that's all we need from Jones data we'll 
  #   only include those
  filter(!is.na(age))
  
## ......3Ac. Join Krauss and Jones depthseries data  #####################

# only including if resolved to join in c14 ages from Jones 2017
depthseries_join <- Krauss_depthseries %>%
  full_join(Jones_depthseries)

# Update study IDs based on revised methods table
# Three different coring methods were used on these cores, each combination gets a unique study ID
depthseries <- depthseries_join %>%
  mutate(method_id = case_when(core_id == "butler_island_1" | core_id == "turkey_creek_2" | core_id == "richmond_island_1" ~ "piston corer used", 
                               core_id == "turkey_creek_1" ~ "vibracore used",
                               TRUE ~ "russian corer used"))

depthseries <- reorderColumns("depthseries", depthseries)

# former ID associations
# Krauss_et_al_2018a piston corer 
# Krauss_et_al_2018b vibracore    
# Krauss_et_al_2018c russian corer

## ....3B. Core-level data ##################

cores <- depthseries %>%
  select(study_id, site_id, core_id) %>%
  distinct(study_id, site_id, core_id) %>%

  # manually add core coordinates sent to JH, ML, and DK on 2019-04-19
  # email subject: FW: [EXTERNAL] Positional Information for Krauss and Jones Data Release
  mutate(core_latitude = case_when(core_id == "turkey_creek_1" ~ 33.35003,
                                   core_id == "turkey_creek_2"~ 33.34001,
                                   core_id == "butler_island_1"~ 33.422823,
                                   core_id == "richmond_island_1"~ 33.55564,
                                   core_id == "savannah_mid_1"~ 32.17,
                                   core_id == "savannah_low_1"~ 32.18,
                                   core_id == "savannah_mid_2" ~ 32.24,
                                   core_id == "savannah_high_1" ~ 32.238)) %>%
  
  mutate(core_longitude = case_when(core_id == "turkey_creek_1" ~ -79.3447,
                                    core_id == "turkey_creek_2" ~ -79.34166,
                                    core_id == "butler_island_1" ~ -79.207996,
                                    core_id == "richmond_island_1" ~ -79.08943,
                                    core_id == "savannah_mid_1" ~ -81.14,
                                    core_id == "savannah_low_1" ~ -81.14, 
                                    core_id == "savannah_mid_2" ~ -81.15,
                                    core_id == "savannah_high_1" ~ -81.155),
         year = case_when(site_id == "Waccamaw"~2011,
                          site_id == "Savannah"~2012)) %>%
  
  # Designate salinity class. Note that both the "oligohaline", "salty", and 
  #   "fresh tidal" sites are all classified as oligohaline according to CCRCN standards.
  #   Salinity measurements pulled from metadata.
#   core_id == "savannah_mid_1" | core_id == "turkey_creek_1" | core_id == "savannah_low_1" | core_id == "turkey_creek_2"
# | core_id == "savannah_mid_2" | core_id == "butler_island_1" ~ "oligohaline",
  mutate(salinity_class = case_when(core_id == "savannah_high_1" | core_id == "richmond_island_1" ~ "fresh", 
                                TRUE ~ "oligohaline")) %>%
  mutate(vegetation_class = case_when(core_id == "savannah_low_1" | core_id == "savannah_mid_1" | core_id == "turkey_creek_1" ~ "emergent",
                                      core_id == "butler_island_1" | core_id == "turkey_creek_2" ~ "forested to emergent",
                                      core_id == "savannah_mid_2" | core_id == "richmond_island_1" | core_id == "savannah_high_1" ~ "forested")) %>%
  mutate(vegetation_method = "measurement",
         core_length_flag = "not specified") %>%
  mutate(inundation_class = case_when(core_id == "savannah_low_1" | core_id == "savannah_mid_1" | core_id == "turkey_creek_1" | core_id == "turkey_creek_2" ~ "low",
                                      core_id == "butler_island_1" | core_id == "savannah_mid_2" ~ "mid",
                                      core_id == "richmond_island_1" | core_id == "savannah_high_1" ~ "high"))
                                   
  # select(study_id, site_id, core_id, core_latitude, core_longitude, salinity_class,
  #        vegetation_class, vegetation_method, inundation_class)

## RC edit, adding `habitat` coding for snythesis update 
cores <- cores %>% 
  mutate(habitat = case_when(vegetation_class == "emergent" ~ "marsh",
                             vegetation_class == "forested" ~ "swamp",
                             TRUE ~ "scrub/shrub"))

## ....3C. Site-level data #############

# sites <- cores %>%
#   distinct(study_id, site_id, salinity_class, vegetation_class, vegetation_method,
#          inundation_class)
#                                 
                              
## ....3D. Impact data #################

impacts <- cores %>% 
  select(study_id, site_id, core_id) %>% 
  mutate(impact_class = "natural")

# ## ....3E. Materials and Methods #############
# 
# methods <- cores %>%
#   select(study_id, site_id, core_id) %>%
#   distinct(study_id, site_id, core_id) %>%
#   mutate(loss_on_ignition_temperature = c(550)) %>%
#   # see personal communication, different coring methods for each site
#   mutate(coring_method = ifelse(site_id == "Waccamaw_River", "vibracore",
#                                 "russian corer"))  %>%
#   mutate(compaction_flag = ifelse(site_id == "Waccamaw_River", "compaction quantified",
#                                   "corer limits compaction")) %>%
#   mutate(age_depth_model_reference = "YBP")

raw_methods <- read_csv("data/primary_studies/Krauss_2018/intermediate/Krauss_et_al_2018_methods.csv")

methods <- raw_methods %>%
  mutate(study_id = id,
         coring_method = recode(coring_method,
                                "Russian peat corer" = "russian corer",
                                "vibracorer" = "vibracore")) %>%
  mutate(method_id = paste0(coring_method, " used")) %>%
  select(study_id, everything())

## ....3F. Species data ##############
species <- species_edited %>%
  mutate(study_id = id) %>%
  select(-common_name) %>% 
  mutate(site_id = ifelse(str_detect(site_id, "Savannah"), "Savannah", site_id),
         site_id = ifelse(str_detect(site_id, "Waccamaw"), "Waccamaw", site_id))


## ....3H. Study citations ################

if(!file.exists("data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_study_citations.csv")){
  # Get BibTex entries from DOI
  paper_bibs_raw <- GetBibEntryWithDOI(c("10.1029/2018GB005897","10.1002/2017JG004015"))
  data_bib_raw <- GetBibEntryWithDOI("10.5066/F7TM7930")
  
  # # If the data citation worked...
  # if(class(data_bib_raw)[1] == "BibEntry") {
  #   data_bib <- data_bib_raw  # Save it
  # 
  # } else {
  #   # Otherwise manually write it 
  #   data_bib <- tibble(
  #     study_id = id,
  #     bibliography_id = id,
  #     publication_type = "misc",
  #     key = "Krauss_2018_data",
  #     bibtype = "misc",
  #     doi = "10.5066/F7TM7930",
  #     url = "https://doi.org/10.5066/f7tm7930",
  #     year = "2018",
  #     publisher = "USGS",
  #     author= "Ken Krauss",
  #     title = "Carbon budget assessment of tidal freshwater forested wetland and oligohaline marsh ecosystems along the Waccamaw and Savannah rivers, U.S.A. (2005-2016)"
  #   )
  # }
  
  study_ids <- cores %>%
    select(study_id) %>%
    distinct()
  
  # Convert this to a dataframe
  paper_biblio <- as.data.frame(paper_bibs_raw) %>%
    mutate(bibliography_id = c("Krauss_et_al_2018_article", "Jones_et_al_2017_article"),
           publication_type = "associated source") %>%
    # GetBibEntryWithDOI() defaults study name as a row name, convert to column
    merge(study_ids)
  
  data_biblio <- as.data.frame(data_bib_raw) %>% 
    mutate(publication_type = "primary dataset",
           bibliography_id = "Krauss_et_al_2018_data") %>%
    merge(study_ids)
  
  # Curate biblio so ready to read out as a BibTex-style .bib file
  study_citations <- data_biblio %>%
    bind_rows(paper_biblio) %>%
    select(study_id, bibliography_id, publication_type, bibtype, everything())
  
  # Write .bib file
  bib_file <- study_citations %>%
    select(-study_id, -publication_type) %>%
    distinct() %>%
    column_to_rownames("bibliography_id")
  
  WriteBib(as.BibEntry(bib_file), "data/primary_studies/Krauss_2018/derivative/Krauss_2018.bib")
  write_csv(study_citations, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_study_citations.csv")
  
}

# Update Tables ###########
source("./scripts/1_data_formatting/versioning_functions.R")

table_names <- c("methods", "cores", "depthseries", "impacts", "species")

updated <- updateTables(table_names)

# save listed tables to objects

# sites <- updated$sites
impacts <- updated$impacts
methods <- updated$methods
depthseries <- updated$depthseries
cores <- updated$cores
species <- updated$species

## 4. QA/QC of data ################

## ....4A. Column and Variable names ###############
testTableCols(table_names)
testTableVars(table_names)
testRequired(table_names)

## ....4B. Quality control on cell values ###################
test_unique_cores(cores)
test_unique_coords(cores)
test_core_relationships(cores, depthseries)
fraction_not_percent(depthseries)
numeric_test_results <- test_numeric_vars(depthseries)


# Reorder tables according to CCRCN guidance
cores <- reorderColumns("cores", cores)
depthseries <- reorderColumns("depthseries", depthseries)
# sites <- reorderColumns("sites", sites)
impacts <- reorderColumns("impacts", impacts)
species <- reorderColumns("species", species)


## 5. Write data ######################
write_csv(depthseries, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_depthseries.csv")
write_csv(cores, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_cores.csv")
# write_csv(sites, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_sites.csv")
write_csv(impacts, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_impacts.csv")
write_csv(species, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_species.csv")
write_csv(methods, "data/primary_studies/Krauss_2018/derivative/Krauss_et_al_2018_methods.csv")

