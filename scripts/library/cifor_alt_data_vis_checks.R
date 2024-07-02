
# library 
library(tidyverse)

cores_master <- read_csv("data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")
depths_master <- read_csv("data/primary_studies/CIFOR/derivative_ALT/cifor_alt_depthseries.csv")

studies <- unique(cores$study_id)

for (i in studies) {
 cores <- cores_master %>% filter(study_id == i)
 depthseries <- depths_master %>% filter(study_id == i)

  writeDataVizReport(i)
}
