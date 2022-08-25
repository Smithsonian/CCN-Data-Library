# Parse EML metadata
# David Klinges, klingesd@si.edu

## Prep workspace #################

# install this package if you haven't already
install.packages('EML')

# Now mount the EML package into the R library
library(EML)

## Import and parse EML metadata ###############

# Set the file path to the xml file
CO2_file_path <- paste0(getwd(), "/Phase 2 Free Air CO2 Enrichment Model-Data Synthesis- Data from Six US-Located Elevated CO2 Experiments.xml")

# Read in the EML-formatted xml file
CO2_metadata <- read_eml(CO2_file_path)

# Designate a search term used to parse
PARSE_TERM <- set_a_term_here

# the above search term needs to match exactly how it is spelled in the xml 
#   file, and has to be place within quoation marks. E.g., replacing set_a_term_here
#   with "boundingCoordinates".
# I didn't do this for you to make sure that you're actually reading what I wrote

# Now parse the metadata for that term 
eml_get(CO2_metadata, PARSE_TERM)


## This xml file is not EML-formatted, and so read_eml won't work
facemds_file_path <- "./data/metadata_samples/facemds-standard-name-table-modvars.xml"

facemds_metadata <- read_eml(metadata_file_path)
