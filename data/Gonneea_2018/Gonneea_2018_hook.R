## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Holmquist et al 2018 Scientific Reports
#   data release. 

# Load RCurl, a package used to download files from a URL
library(RCurl)

# The Gonneea et al (2018) data release features a diverse suite of file types:
#   a .jpg, a .xlsx, a .csv, and a .xml
# So we'll need to have a custom hook for each file

url_list <- list("https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__6f%2F73%2F4b%2F6f734b0239c27f78c7f347dcf277c491a4a47903",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__7d%2Fe4%2Fdc%2F7de4dc002db596e1d7fbe8254de9ccc3af05ae3b",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__70%2F1e%2F99%2F701e99829e5860c1a0dc512056e5d71ff292dc19",
                 "https://www.sciencebase.gov/catalog/file/get/5a748e35e4b00f54eb19f96c?f=__disk__3e%2F2d%2Ff5%2F3e2df544c537a35007214d1fe595b45499df2f4a")

# Extract Saltmarsh_AR.jpg
download.file(url_list[[1]], paste0(getwd(), "./data/Gonneea_2018/Saltmarsh_AR.jpg"),
              mode = "wb")

# Extract Waquoit_Core_data_release.xlsx
download.file(url_list[[2]], paste0(getwd(), "./data/Gonneea_2018/Waquoit_Core_data_release.xlsx"), 
              mode = "wb")

# Extract Waquoit_Core_data_release.csv
download.file(url_list[[3]], paste0(getwd(), "./data/Gonneea_2018/Waquoit_Core_data_release.csv"),
              mode = "wb")

# Extract Waquoit_Core_data_release_meta.xml
download.file(url_list[[4]], paste0(getwd(), "./data/Gonneea_2018/Waquoit_Core_data_release_meta.xml"),
              mode = "wb")





