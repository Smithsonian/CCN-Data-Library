## CCN Data Library 
## Jaxine Wolfe <wolfejax@si.edu>

# Housekeeping: make sure that all data has at least one associated citation

library(tidyverse)

# Sanderman synthesis had a lot of study_id's w/o citation
# bib <- read_csv("data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_study_citations.csv")
cores <- read_csv("data/primary_studies/Sanderman_2018/derivative/Sanderman_2018_cores.csv")
# 
# no_citations <- unique(cores$study_id)[!unique(cores$study_id) %in% unique(bib$study_id)]
# no_citations %in% unique(cores$study_id)

# create function to automate test
testCitations <- function(){
  
  bib <- read_csv("data/CCRCN_synthesis/CCRCN_study_citations.csv")
  cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv", guess_max = 7000)
  
  no_citations <- unique(cores$study_id)[!unique(cores$study_id) %in% unique(bib$study_id)]
  
  if (!is_empty(no_citations)){
    print("The following studies have no associated citation:")
    return(no_citations)
  } else {
    return("Looks good! All data is cited.")
  }
}

testCitations()
