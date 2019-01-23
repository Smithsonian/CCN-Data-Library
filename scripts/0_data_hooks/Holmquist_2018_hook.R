## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Holmquist et al 2018 Scientific Reports
#   data release. 

# Load RCurl, a package used to download files from a URL
library(RCurl)

# Create a list of the URLs for each data file
url_list <- list("https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_core_data.csv?sequence=7&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_depth_series_data.csv?sequence=8&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_impact_data.csv?sequence=9&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_methods_data.csv?sequence=10&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_species_data.csv?sequence=11&isAllowed=y"
)

# Apply a function, which downloads each of the data files, over url_list
lapply(url_list, function(x) {
  # Extract the file name from each URL
  filename <- as.character(x)
  filename <- substring(filename, 56)
  filename <- gsub("\\..*","", filename)
  # Now download the file into the "data" folder
  download.file(x, paste0(getwd(), "./data/Holmquist_2018/", filename, ".csv"))
})

