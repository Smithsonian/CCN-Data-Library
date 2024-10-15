                                    
# > lst <- list.files(path = "data/primary_studies", pattern = ".csv", full.names = TRUE, recursive = TRUE)
# > veg_lst <- lst[grep("plant|plot|biomass|debris|veg", lst)]
# > veg_lst

# # Index of table names
# tables <- c("depthseries", "cores", "sites", "species", "impacts", "methods", "study_citations")
# 
# # Other objects that we will need to track
# trackers <- c(
#   # .bib file paths stored in a list 
#   "bibs",
#   # CSV files that do not follow established naming conventions go here 
#   "unknown_csv",
#   # Non .bib or .csv files go here
#   "unknown_filetypes")
# 
# # Empty lists to fill with file paths
# file_paths <- vector("list", length(c(tables, trackers)))
# names(file_paths) <- c(tables, trackers)

# lst <- list.files(path = "data/primary_studies", pattern = ".csv", full.names = TRUE, recursive = TRUE)
# veg_lst <- lst[grep("plant|plot|biomass|debris|veg", lst)]

lst <- c("data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_plants.csv",
         "data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_plants.csv",
         "data/primary_studies//Morrissette_et_al_2023/original/Morrissette_et_al_2023_biomass.csv")

plot_lst <- c("data/primary_studies/Cifuentes_2023_Panama/original/Cifuentes_et_al_2023_plots.csv",                                    
              "data/primary_studies/Cifuentes_et_al_2024_Nicoya/original/Cifuentes_et_al_2024_plots.csv",                                
              "data/primary_studies/Morrissette_et_al_2023/original/Morrissette_et_al_2023_plots.csv", 
              "data/primary_studies/CIFOR/derivative_SWAMP/cifor_swamp_plots.csv")

data_compile <- plyr::rbind.fill(lapply(plot_lst, function(i){read.csv(i)})) ## tom.hengl@envirometrix.net

write_excel_csv(data_compile, "app_biomass_input.csv")

## Printing citations

library(RefManageR)

# Write .bib file
bib_file <- as.BibEntry(
  new_bib %>%
    select(-publication_type) %>%
    filter(grepl("data_soil|SWAMP_", bibliography_id)) %>% 
    column_to_rownames("bibliography_id")
)

TextCite(bib_file)

PrintBibliography(bib_file)

