# Adding Database Citation to synthesis

# create citation table
# db_citation <- data.frame(
#   bibliography_id = "Coastal_Carbon_Data_Library",
#   publication_type = "database",
#   bibtype = "Misc",
#   title = "Database: Coastal Carbon Network Data Library",
#   author = "James Holmquist and Jaxine Wolfe and Michael Lonneman and Dave Klinges and J. Patrick Megonigal",
#   doi = "10.25573/serc.21565671",
#   url = "https://doi.org/10.25573/serc.21565671",
#   year = "2023",
#   publisher = "Smithsonian Environmental Research Center"
# )

library(RefManageR)

db_citation <- as.data.frame(GetBibEntryWithDOI("10.25573/serc.21565671")) %>% 
  mutate(bibliography_id = "Coastal_Carbon_Data_Library",
         publication_type = "database",
         title = str_replace(title, past_version_code, new_version_code)) %>% 
  remove_rownames()

# convert to bib file table
db_bib <- db_citation %>% select(-publication_type) %>%   
  column_to_rownames("bibliography_id")

# add to synthesis
ccrcn_synthesis$study_citations <- bind_rows(ccrcn_synthesis$study_citations, db_citation)
bib_file <- bind_rows(bib_file, db_bib)

readMeFile <- readLines("data/CCN_synthesis/README.txt")
readMeFile<-str_replace(readMeFile, past_version_code, new_version_code)
writeLines(readMeFile, "data/CCN_synthesis/README.txt")

readMeFile2 <- readLines("README.md")
readMeFile2<-str_replace(readMeFile2, past_version_code, new_version_code)
writeLines(readMeFile2, "README.md")


# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])
