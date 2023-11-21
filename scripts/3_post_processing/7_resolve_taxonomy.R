## Resolve Taxonomy ####

## function to resolve taxa names using GNR 
# uses taxonomic authorities to resolve spelling rather than recoding everything by hand
resolveTaxa <- function(taxa) {
  
  resolved <- data.frame()
  unresolved <- vector()
  
  for (i in 1:length(taxa)){
    # gnr_sources <- taxize::gnr_datasources()
    
    # store resolved results
    gnr_result <- taxize::gnr_resolve(sci = as.vector(taxa[i]), 
                                      canonical = TRUE) %>%
      # gnr_datasources()
      # preferred_data_sources = c(150, 9, 4, 3)
      
      # pick the first result
      slice(1)
    
    if (!plyr::empty(gnr_result)) {
      # compile list of resolved taxa
      resolved <- rbind(resolved, gnr_result)
      
    } else {
      # save unresolved taxa
      unresolved <- rbind(unresolved, taxa[i])
      # skip unresolved taxa
      i <- i + 1
      next
    }
  }
  
  if (length(unresolved) > 0) {
    print("The following taxa could not be resolved:")
    print(unresolved)
  }
  
  return(resolved)
}
# Check the above function (taken from the MWG) against what is already in this repository


# resolve taxa names using GNR
taxa_db <- read_csv("docs/ccn_taxa_database.csv")

# pull all species from the synthesis and identify which ones have not been resolved yet
taxa <- sort(unique(ccrcn_synthesis$species$species_code))

taxa_index <- which(!(taxa %in% taxa_db$species_code))

# resolve the taxa which are not present in the database
if(length(taxa_index) > 0){
  
  taxa_resolved <- resolveTaxa(taxa[taxa_index])
  
  clean_resolved <- taxa_resolved %>% 
    rename(species_code = user_supplied_name,
           resolved_taxa = matched_name2,
           data_source = data_source_title) %>% 
    select(species_code, resolved_taxa, data_source, score)
  
  # add entries to the taxa database
  taxa_db <- bind_rows(taxa_db, clean_resolved) %>% arrange(species_code)
  write_csv(taxa_db, "docs/ccn_taxa_database.csv")
  
  } else {
  print("No new taxa.")
}

# join resolved names to species table
final_species <- left_join(ccrcn_synthesis$species, taxa_db) %>%
  # make corrections to misspelled species codes
  # some spot fixes
  mutate(species_code = case_when(species_code == "Unidentified forb" ~ "Forb",
                                  species_code == "Unidentified grass" ~ "Graminoid",
                                  species_code %in% c("Amphibolis australis", "Avicennia corniculatum", "Schoenoplectus montevidensis") ~ species_code,
                                  # cut off score is 75% match (most are 98%)
                                  # the 75% cases are usually for the Genus IDs where we have spp. added onto the name
                                  !is.na(resolved_taxa) & score >= 0.75 ~ resolved_taxa, 
                                  species_code == "Thassia hemprichii" ~ "Thalassia hemprichii",
                                  T ~ species_code)) %>%
  select(-c(resolved_taxa, data_source, score))

# investigate some unresolved cases
# store these in a table somewhere in the future
# unresolved <- ccrcn_synthesis$species %>% 
#   filter(species_code %in% c("Algal Mat", "Forb spp.", "High Marsh", "Mix", "mixed", "previously vegetated soils",
#                              "submerged aquatic vegetation", "Swamp", "text", "unvegetated",                
#                              "Wrack", "none", "unknown", "Surface Algae")) %>% 
#   distinct(study_id, site_id, species_code, code_type)

# write to synthesis
ccrcn_synthesis$species <- final_species

# clear workspace of unnecessary variables
rm(list= ls()[!(ls() %in% c("ccrcn_synthesis", "bib_file", "qa_numeric_results", "qa_results", "join_status", "file_paths"))])

