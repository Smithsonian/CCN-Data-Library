## CCN Data Library

## MarineGEO seascapes query

# Questions:
# Does seascape context matters for carbon storage?
# Do two habitats adjacent to one another have more carbon (in their biomass or sediments) than either habitat by itself?
# which data don't have associated articles?

# query for instances of multiple habitats documented in one study

library(tidyverse)
library(readxl)
library(fuzzyjoin)

cores <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_cores.csv", guess_max=10000, col_types = cols())

# identify multi-habitat studies
multi_habitat <- cores %>% 
  group_by(study_id) %>% 
  summarize(habitat) %>% 
  drop_na(habitat) %>% 
  distinct() %>% 
  filter(!(habitat %in% c("scrub shrub", "scrub/shrub", "upland", "algal mat", "swamp"))) %>% 
  add_count(study_id) %>% 
  filter(n > 1) %>% 
  arrange(study_id)
length(unique(multi_habitat$study_id))  # 47

# subset synthesis bibliography for multi-habitat studies
bib <- read_csv("data/CCRCN_synthesis/derivative/CCRCN_study_citations.csv") %>% 
  filter(study_id %in% unique(multi_habitat$study_id)) %>% 
  arrange(study_id) %>% 
  filter(bibliography_id != "Fourqurean_et_al_2012_article") %>% 
  select(study_id, bibliography_id, title, author, year, publication_type, bibtype, journal, doi, url)
  # distinct()

# cores_na_habitat <- cores %>% 
#   filter(study_id %in% multi_habitat$study_id[is.na(multi_habitat$habitat)]) %>% 
#   filter(is.na(habitat))

# Compare with MGEO query ####

# paper_path <- dir("/path/to/papers", pattern = ".xlsx", full.names = T)
# 
# review_papers <- data.frame()
# 
# for(i in paper_path){
#   temp_df <- read_xlsx(i, sheet = 1) %>% select(Authors, `Article Title`, Year, `Source Title`, Included) %>% 
#     rename(title = `Article Title`) %>% 
#     drop_na(Authors)
# 
#   review_papers <- bind_rows(review_papers, temp_df)
# }
# write_csv(review_papers, "query/data/mgeo_seascapes_papers.csv")

review_papers <- read_csv("query/data/mgeo_seascapes_papers.csv")

included <- review_papers %>% 
  mutate(Included = tolower(Included)) %>% 
  filter(Included %in% c("y", "m", "yes") | is.na(Included))

# fuzzy match based on title
bib_match <- stringdist_join(review_papers, bib,
                             by = "title",
                             mode = "right",
                             ignore_case = T, 
                             method = "osa", 
                             max_dist = 2,
                             distance_col = "dist") %>% 
  group_by(title.x) %>%
  select(study_id, bibliography_id, publication_type, bibtype, 
         title.x, title.y, author, Year, doi, year, `Source Title`, everything()) %>% 
  filter(!(study_id == "Radabaugh_et_al_2018" & is.na(Included) & bibtype == "Article")) %>% # duplicate
  mutate(doi = ifelse(is.na(doi), url, doi), 
         included = tolower(Included)) %>% 
  rename(mgeo_title = title.x, ccn_title = title.y) %>% 
  select(-c(Authors, Year, `Source Title`, url, dist, Included)) %>% 
  arrange(study_id)
  
write_csv(bib_match, "query/data/mgeo_ccn_bibmatch.csv")
