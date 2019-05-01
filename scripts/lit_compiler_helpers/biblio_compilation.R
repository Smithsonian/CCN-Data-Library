## CCRCN Data Library
# contact: klingesd@si.edu

# This script writes a BibTeX file for each data source in
#   the CCRCN library


## Workspace prep ########
library(RefManageR)
library(fuzzyjoin)
library(tidyverse)
library(bib2df)

# Import CCRCN core data
CCRCN_coredata <- read_csv("./data/CCRCN_synthesis/CCRCN_core_data.csv")

# Import the CCRCN bibliography 
CCRCN_bib <- bib2df("./docs/CCRCN_bibliography.bib")

## Curate Bibliography ###############


study_ids <- CCRCN_coredata %>%
  distinct(study_id) %>%
  select(study_id) %>%
  mutate(temp_id = study_id) %>%
  separate(temp_id, into = c("author", "rest"),  sep = "_") %>%
  mutate(year = gsub("\\D", "", study_id)) %>%
  mutate(author = gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', author)) %>%
  unite(author_year, c(author, year), sep = "")

# fuzzyjoin
CCRCN_bib_df <- fuzzy_join(CCRCN_bib, study_ids, by = c("BIBTEXKEY" = "author_year"), 
                        match_fun = str_match, mode = "full")

# fuzzyjoi
CCRCN_bib_df <- regex_inner_join(CCRCN_bib, study_ids, by = c(BIBTEXKEY = "author_year"))

# Create a df of just study_id and BIBTEXKEY to link bib entries
CCRCN_bib_link <- CCRCN_bib_df %>%
  select(study_id, BIBTEXKEY, author)


## Write bibliography ################

# Write out as .csv
write_csv(CCRCN_bib_link, "./docs/bibliography/CCRCN_bibliography_link.csv")

# Convert back to BibEntry and write BibTeX file
df2bib(CCRCN_bib, file = "./docs/bibliography/CCRCN_bibliography_out.bib")





