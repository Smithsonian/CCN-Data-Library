## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Citations  ###############
# Data Citation
# Thorne, K. 2015. Field and model data for studying the effects of sea-level rise on eight tidal marshes in coastal Washington and Oregon. 
# US Geological Survey Data Release. 10.5066/F7SJ1HNC.


# Publication Citation
# Karen Thorne, Glen MacDonald, Glenn Guntenspergen, Richard Ambrose, Kevin Buffington, Bruce Dugger, Chase Freeman, 
# Christopher Janousek, Lauren Brown, Jordan Rosencranz, James Holmquist, John Smol, Kathryn Hargan, and John Takekawa, 2018, 
# U.S. Pacific coastal wetland resilience and vulnerability to sea-level rise: Science Advances, v. 4, iss. 2.

## 2. Prep workspace #######################
# Load RCurl, a package used to download files from a URL
library(tidyverse)
library(lubridate)
library(readxl)

## 3. Data Location ########

# I'm unable to scrape files from USGS. Until I figure it out, I'll download and place in original folder. 

URL <- "https://www.sciencebase.gov/catalog/item/55ae7d09e4b066a24924239f"

## 4. Import data ####################

# The soil core and depthseries data is spread across multiple sheets in an excel file. 
# Each core's depth series has it's own page but there is no core ID in the table. 
# Instead, the sheet name is the associated core name. 
# Each sheet will need to be read in as part of a loop

## ... 4A. Assemble vector of core names and import ##################
core_ids <- c("BM01", "BM03", "BM05", "CB00", "CB03", 'CB06', "GH01", "GH03", "GH06", 
              "NQ01", "NQ04", "NQ06", "PS02", "PS04", 'PS05', "SZ02", "SZ03", "SZ05",
              "SK02", "SK04", "SK06", "WB01", "WB04", "WB06")

num_cores <- length(core_ids)

for(i in 1:num_cores) {
  d <- read_excel("./data/Thorne_2015_a/original/NWCSC Sediment Core Data.xlsx", sheet=as.character(core_ids[i]))
  d <- d %>%
    mutate(core_id = core_ids[i]) %>%
    rename(depth_min = "Depth (cm)",
           fraction_organic_matter = "Organic Content", 
           dry_bulk_density = "Bulk Density") %>%
    mutate(fraction_organic_matter = as.double(fraction_organic_matter),
           dry_bulk_density = as.double(dry_bulk_density))
  assign(core_ids[i],d)
}

## ... 4B Import core-level data 
raw_core_data <- read_excel("./data/Thorne_2015_a/original/NWCSC Sediment Core Data.xlsx", sheet="CoreSurveys_CS137")

## 5. Curate Data ##################

## ... 5A. Append depthseries data, add appropriate core ID, and curate ############
depthseries_data <- data.frame(matrix(nrow=0, ncol=4))
colnames(depthseries_data) <- colnames(BM01)

core_ids <- list(BM01, BM03, BM05, CB00, CB03, CB06, GH01, GH03, GH06, 
              NQ01, NQ04, NQ06, PS02, PS04, PS05, SZ02, SZ03, SZ05,
              SK02, SK04, SK06, WB01, WB04, WB06)

depthseries_data <- depthseries_data %>%
  bind_rows(core_ids) %>%
  mutate(depth_max = depth_min + 1, 
         fraction_organic_matter = fraction_organic_matter / 100,
         study_id = "Thorne et al. 2018") %>%
  select(core_id, study_id, depth_max, depth_min, fraction_organic_matter, dry_bulk_density)

## ... 5B. Curate core-level data #############
core_data <- raw_core_data %>%
  rename(core_elevation = `Elevation (m, NAVD88)`,
         cs137_peak_cm = `CS137 Peak (cm)`) %>%
  mutate(core_id = paste(Site, Core, sep="_"),
         study_id = "Thorne et al. 2018", 
         core_position_method = "RTK", 
         zone = 10) 

output <- convert_UTM_to_latlong(core_data$Easting, core_data$Northing, core_data$zone, core_data$core_id)
easting <- core_data$Easting
northing <- core_data$Northing
zone <- core_data$zone
core_id <- core_data$core_id




