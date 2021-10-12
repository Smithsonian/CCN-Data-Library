## CCRCN Data Library
## Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)

# species synthesis
species <- read_csv("data/CCRCN_synthesis/original/CCRCN_species.csv")
# taxa lookup with associated habitat
spref <- read_csv("docs/versioning/species-habitat-classification-JH-20200824.csv")
# taxa database
ccn_taxa <- read_csv("docs/ccrcn_taxa_database.csv")

# unique list of all ccn taxa (misspellings included)
uniq_taxa <- c(unique(ccn_taxa$species_code, ccn_taxa$resolved_taxa))

# find species that are not in the database
no_record <- species %>% 
  filter(code_type != "description" | is.na(code_type)) %>% 
  filter(!(species_code %in% uniq_taxa)) %>% 
  distinct(species_code)

# resolve them

# etc.

# add code type via taxize? vs manual assignment? 
# alt: pattern matching to assign
# Ex. tax_rank("Rhizophora", db = "itis")

# join habitat assignment from spref table
taxaref <- left_join(ccn_taxa, spref)
