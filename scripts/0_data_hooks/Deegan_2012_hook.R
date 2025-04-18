## CCRCN Data Library
# contact: klingesd@si.edu
#          lonnemanm@si.edu

## 1. Citations for data and publications ############
## Data citations
# BELOW GROUND BIOMASS
# Deegan L., D. FitzGerald, S. Fagherazzi. 2012. 2009 LENS belowground biomass core results. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/bc041b3546ba4a3730fd391852741620. 
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-pie.216.2

# ABOVE GROUND BIOMASS
# Deegan L., D. FitzGerald, S. Fagherazzi. 2012. 2009 LENS aboveground biomass results. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/6830381e663fedc52bbdb3c501fdf3ee. 
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-pie.217.2

# Publication: 
# Deegan, Linda A., David Samuel Johnson, R. Scott Warren, Bruce J. Peterson, John W. Fleeger, 
# Sergio Fagherazzi, and Wilfred M. Wollheim. 2012. “Coastal Eutrophication as a Driver of Salt Marsh Loss.” 
# Nature 490 (7420): 388–92. https://doi.org/10.1038/nature11533.


## 2. Prep workspace and scrape data #######################
# library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
# library(rcrossref)
# library(bib2df)
library(RefManageR)

## DATA DOWNLOAD WORKFLOW ARCHIVED ####
# ## ... 2A. download and save below ground biomass #########
# infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/216/2/e2c8e7c0b2338c593f00ab988d0e9774" 
# infile1 <- sub("^https","http",infile1) 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                , col.names=c(
#                  "Location",     
#                  "Site.Number",     
#                  "Collection.Date",     
#                  "Latitude",     
#                  "Longitude",     
#                  "Biomass.Core.Segment",     
#                  "Live.Rhizomes",     
#                  "Live.Roots",     
#                  "Detritus"    ), check.names=TRUE)
# 
# write_csv(dt1, "./data/primary_studies/Deegan_2012/original/LTE-TIDE-LENS-2009-below-bio.csv")
# 
# ## ... 2B. download above ground biomass data ###########
# infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/217/2/79e90861144b45ea7c229ca40cdaba40" 
# infile2 <- sub("^https","http",infile2) 
# dt2 <-read.csv(infile2,header=F 
#                ,skip=1
#                ,sep=","  
#                , col.names=c(
#                  "Location",     
#                  "Site.Number",     
#                  "Collection.Date",     
#                  "Latitude",     
#                  "Longitude",     
#                  "Aboveground.Stem.Length",     
#                  "Aboveground.mass"    ), check.names=TRUE)
# 
# write_csv(dt2, "./data/primary_studies/Deegan_2012/original/LTE-TIDE-LENS-2009-above-bio.csv")

## 3. Curate data #################
dt1 <- read_csv("data/primary_studies/Deegan_2012/original/LTE-TIDE-LENS-2009-below-bio.csv")
dt2 <- read_csv("data/primary_studies/Deegan_2012/original/LTE-TIDE-LENS-2009-above-bio.csv")

## ... 3AA. prep biomass depthseries data #########
# Warning: There is no guidance yet on biomass data 
biomass_depthseries <- dt1 %>%
  rename(site_id = Location,
         live_rhizomes = Live.Rhizomes,
         live_roots = Live.Roots,
         detritus = Detritus) %>%
  mutate(core_id = paste0(site_id,Site.Number)) %>%
  select(-Collection.Date, -Latitude, -Longitude, -Site.Number) %>%
  separate(col="Biomass.Core.Segment", into=c("depth_min", "depth_max"), sep="-") %>%
  mutate(study_id = "Deegan_et_al_2012")

## ... 3AB. prep biomass aggregated data ###########

biomass_agg <- biomass_depthseries %>%
  group_by(study_id, site_id, core_id) %>%
  summarize(live_rhizomes = sum(live_rhizomes), live_roots = sum(live_roots),
            detritus= sum(detritus))

agb_biomass <- dt2 %>%
  rename(site_id = Location) %>%
  mutate(core_id = paste0(site_id,Site.Number)) %>%
  select(core_id, Aboveground.Stem.Length, Aboveground.mass) %>%
  rename(aboveground_stem_length = Aboveground.Stem.Length, 
         aboveground_mass = Aboveground.mass)

biomass_agg <- biomass_agg %>% 
  full_join(agb_biomass)

## ... 3B. Prep core-level data #########
cores <- dt1 %>%
  rename(site_id = Location, 
         core_latitude = Latitude, 
         core_longitude = Longitude) %>%
  mutate(core_id = paste0(site_id,Site.Number)) %>%
  mutate(Collection.Date = gsub("Sep","09", Collection.Date)) %>%
  mutate(Collection.Date = gsub("-","/", Collection.Date)) %>%
  mutate(core_date = as.Date(Collection.Date, format="%d/%m/%Y")) %>%
  group_by(core_id) %>%
  summarize(site_id = first(site_id), core_latitude = first(core_latitude), core_longitude = first(core_longitude), 
            core_date = first(core_date)) %>%
  mutate(core_length_flag = "core depth limited by length of corer", 
         vegetation_class = "salt marsh", 
         study_id = "Deegan_et_al_2012")


## ... 3D. Prep site-level data ##########
site_data <- cores %>%
  select(site_id, core_id, study_id, core_latitude, core_longitude)

# Find min and max lat/long for each site
source("./scripts/1_data_formatting/curation_functions.R")
site_boundaries <- create_multiple_geographic_coverages(site_data)
site_data <- site_data %>%
  left_join(site_boundaries) %>% # Add site bounds in
  select(-core_latitude, -core_longitude)
# remove NAs before aggregation
site_data <- na.omit(site_data)

# Now aggeregate data to the site level
site_data <- site_data %>%
  group_by(site_id) %>%
  summarize(study_id = first(study_id), 
            site_longitude_max = first(site_longitude_max), site_longitude_min = first(site_longitude_min),
            site_latitude_max = first(site_latitude_max), site_latitude_min = first(site_latitude_min)) %>%
  mutate(site_description = "Plum Island Sound Estuary, Massachusetts, USA", 
         vegetation_class = "salt marsh")

## 4. Create study-level data ######
# Get bibtex citation from DOI
# The following function works haphazardly: sometimes it acquires the information and sometimes it doesn't.
# biblio_raw <- GetBibEntryWithDOI(c("10.6073/pasta/bc041b3546ba4a3730fd391852741620",
#                                    "10.6073/pasta/6830381e663fedc52bbdb3c501fdf3ee"))

biblio_raw <- list(c(BibEntry(bibtype = "Misc",
                              key = "Deegan_et_al_2009_a_data",
                              title = "2009 LENS aboveground biomass core results",
                              author = "Linda Deegan and Duncan FitzGerald and Sergio Fagherazz",
                              year = "2012",
                              doi = "10.6073/pasta/6830381e663fedc52bbdb3c501fdf3ee",
                              url = "https://doi.org/10.6073/pasta/6830381e663fedc52bbdb3c501fdf3ee",
                              publisher = "Environmental Data Initiative"),
                     BibEntry(bibtype = "Misc",
                              key = "Deegan_et_al_2009_b_data",
                              title = "2009 LENS belowground biomass core results",
                              author = "Linda Deegan and Duncan FitzGerald and Sergio Fagherazz",
                              year = "2012",
                              doi = "10.6073/pasta/bc041b3546ba4a3730fd391852741620",
                              url = "https://doi.org/10.6073/pasta/bc041b3546ba4a3730fd391852741620",
                              publisher = "Environmental Data Initiative")
))

biblio_df <- as.data.frame(biblio_raw)

study_citations <- biblio_df %>%
  mutate(bibliography_id = row.names(.), 
         study_id = "Deegan_et_al_2012",
         publication_type = "primary") %>%
  remove_rownames() %>%
  select(study_id, bibliography_id, publication_type, everything())

# Write .bib file
bib_file <- study_citations %>%
  select(-study_id, -publication_type) %>%
  column_to_rownames("bibliography_id")

WriteBib(as.BibEntry(bib_file), "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012.bib")
write_csv(study_citations, "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_study_citations.csv")

## 5. QA/QC of data ################
source("./scripts/1_data_formatting/qa_functions.R")

# Make sure column names are formatted correctly: 
test_colnames("core_level", cores)
test_colnames("site_level", site_data) 
#test_colnames("biomass", biomass_depthseries) # no ccrcn guidance yet

# Test relationships between core_ids at core- and depthseries-levels
# the test returns all core-level rows that did not have a match in the depth series data
results <- test_core_relationships(cores, biomass_depthseries)

## 6. Write data ##################
write_csv(cores, "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_cores.csv")
write_csv(site_data, "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_sites.csv")
write_csv(biomass_depthseries, "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_depthseries.csv")
write_csv(biomass_agg, "./data/primary_studies/Deegan_2012/derivative/Deegan_et_al_2012_biomass.csv")
