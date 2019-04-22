## CCRCN Data Library
# contact: klingesd@si.edu

# This script writes a BibTeX file for each data source in
#   the CCRCN library


## Workspace prep ########
library(RefManageR)
library(fuzzyjoin)
library(tidyverse)

## Curate Bibliography ###############

# import the CCRCN bibliography 
CCRCN_bib <- bib2df("./docs/bibliography/CCRCN_bibliography.bib")

study_ids <- CCRCN_coredata %>%
  distinct(study_id) %>%
  select(study_id) %>%
  arrange(study_id)

# fuzzyjoin
CCRCN_bib_df <- fuzzy_join(study_ids, CCRCN_bib, by = c("study_id" = "BIBTEXKEY"), 
                        match_fun = str_detect, mode = "full")

# Create a df of just study_id and BIBTEXKEY to link bib entries
CCRCN_bib_link <- CCRCN_bib_df %>%
  select(study_id, BIBTEXKEY)


## Write bibliography ################

# Write out as .csv
write_csv(CCRCN_bib_link, "./docs/bibliography/CCRCN_bibliography_link.csv")

# Convert back to BibEntry and write BibTeX file
df2bib(CCRCN_bib, file = "./docs/bibliography/CCRCN_bibliography_out.bib")





