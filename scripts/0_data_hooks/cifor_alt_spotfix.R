# read in CIFOR data and do higher level fixes

library(tidyverse)

fixStudyID <- function(df){
  df_corrected <- df %>% mutate(study_id = gsub(" |-", "_", study_id)) 
  return(df_corrected)
}

alt_files <- dir("data/primary_studies/CIFOR/derivative_ALT/", pattern = ".csv")

for(file in alt_files){
  write_csv(
  read_csv(paste0("data/primary_studies/CIFOR/derivative_ALT/", file)) %>% fixStudyID(),
  paste0("data/primary_studies/CIFOR/derivative_ALT/", file)
  )
}


# Citations
# gabon DOI: 10.17528/CIFOR/DATA.00214

## Citations 
#import DOIs

soil_citations_raw <- read_csv("./data/primary_studies/CIFOR/CIFOR_docs/SWAMP_alt_bib.csv") %>% 
  drop_na(study_id) %>% select(study_id, DOI) %>% rename(doi = DOI) %>% 
  fixStudyID() %>% distinct()

cifor_alt_cores <- read_csv("data/primary_studies/CIFOR/derivative_ALT/cifor_alt_cores.csv")

soil_citations <- full_join(cifor_alt_cores %>% distinct(study_id), soil_citations_raw) %>% 
  # patch in missing citations
  mutate(doi = case_when(study_id == "SWAMP_Data_Soil_carbon_Barreto_2017_Brazil" ~ "10.17528/CIFOR/DATA.00169",
                         study_id == "SWAMP_Data_Soil_carbon_Caetano_2017_Brazil" ~ "10.17528/CIFOR/DATA.00163",
                         study_id == "SWAMP_Data_Soil_carbon_Acarau_Boca_2016_Brazil"  ~ "10.17528/CIFOR/DATA.00171",
                         study_id == "SWAMP_Data_Soil_carbon_Baouth_2014_Senegal" ~ "10.17528/CIFOR/DATA.00155",
                         study_id == "SWAMP_Data_Soil_carbon_Caete_2017_Brazil" ~ "10.17528/CIFOR/DATA.00164",           
                         study_id == "SWAMP_Data_Soil_carbon_Cauassu_Leste_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00172",
                         study_id == "SWAMP_Data_Soil_carbon_Cauassu_Oeste_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00173", 
                         study_id == "SWAMP_Data_Soil_carbon_Cumbe_Leste_Camaro_2016_Brazil" ~ "10.17528/CIFOR/DATA.00174",
                         study_id == "SWAMP_Data_Soil_carbon_Cumbe_norte_Camarao_2016_Brazil" ~ "10.17528/CIFOR/DATA.00175",
                         study_id == "SWAMP_Data_Soil_carbon_Mangizal_Cauassu_2016_Brazil" ~ "10.17528/CIFOR/DATA.00176",
                         study_id == "SWAMP_Data_Soil_carbon_Manguinho_2016_Brazil" ~ "10.17528/CIFOR/DATA.00177", 
                         study_id == "SWAMP_Data_Soil_carbon_Porto_Ceu_Mangrove_2016_Brazil" ~ "10.17528/CIFOR/DATA.00178",
                         study_id == "SWAMP_Data_Soil_carbon_Porto_Ceu_Shrimp_2016_Brazil" ~ "10.17528/CIFOR/DATA.00179",
                         study_id == "SWAMP_Data_Soil_carbon_Paga_2014" ~ "10.17528/CIFOR/DATA.00214",
                         study_id == "SWAMP_Data_Soil_carbon_Bunaken_2011" ~ "10.17528/CIFOR/DATA.00141",
                         study_id == "SWAMP_Data_Soil_carbon_Cilacap_2011" ~ "10.17528/CIFOR/DATA.00142",
                         study_id == "SWAMP_Data_Soil_carbon_Sembilang_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00144",
                         study_id == "SWAMP_Data_Soil_carbon_Tanjung_Puting_2009_Indonesia" ~ "10.17528/CIFOR/DATA.00145",
                         study_id == "SWAMP_Data_Soil_carbon_Teminabuan_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00146", 
                         study_id == "SWAMP_Data_Soil_carbon_Timika_2011_Indonesia" ~ "10.17528/CIFOR/DATA.00147",
                         study_id == "SWAMP_Data_Soil_carbon_CanGio_2012_Vietnam" ~ "10.17528/CIFOR/DATA.00148",
                         T ~ doi),
         pub_doi = case_when(grepl("Brazil", study_id) ~ "10.1098/rsbl.2018.0208",
                             grepl("Indonesia", study_id) & doi != "10.17528/cifor/data.00192" ~ "10.1038/nclimate2734",
                             study_id == "SWAMP_Data_Soil_carbon_CanGio_2012_Vietnam" ~ "10.1007/s11273-015-9479-2",
                             grepl("Berahan|Timbulsloko", study_id) ~ "10.13057/biodiv/d211134",
                             doi == "10.17528/cifor/data.00215" | doi == "10.17528/cifor/data.00216" ~ "10.1371/journal.pone.0187749",
                             T ~ NA)) %>% 
  pivot_longer(-study_id, names_to = "publication_type", values_to = "doi") %>% 
  drop_na(doi) %>% 
  mutate(publication_type = recode(publication_type, 
                                   "doi" = "primary dataset",
                                   "pub_doi" = "associated source"),
         doi = tolower(doi))



# loop through and pull
soil_bibs <- data.frame()

for (i in unique(soil_citations$doi)) {
  temp_df <- as.data.frame(GetBibEntryWithDOI(i))
  soil_bibs <- bind_rows(soil_bibs, temp_df)
}

cifor_alt_study_citations <- soil_bibs %>% 
  remove_rownames() %>% 
  full_join(soil_citations %>% mutate(doi = tolower(doi))) %>% 
  mutate(bibliography_id = case_when(publication_type == "primary dataset" ~ paste0(study_id, "_data"),
                                     publication_type == "associated source" & doi %in% c("10.1371/journal.pone.0187749", "10.17528/cifor/data.00155") ~ paste0("Kauffman_and_Bhomia_", year, "_article"),
                                     T ~ paste0(word(author), "_et_al_", year, "_article")),
         bibliography_id = recode(bibliography_id, "J._et_al_2018_article" = "Kauffman_et_al_2018_article")) %>% 
  filter(study_id != "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil") %>% 
  select(study_id, bibliography_id, publication_type, everything()) %>% 
# needs manual addition
  add_row(study_id = "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil",
          bibliography_id = "SWAMP_Data_Soil_carbon_Furo_Grande_2017_Brazil_data",
          publication_type = "primary dataset",
          bibtype = "Misc",
          author = "Kauffman, J.B. and Bernardino, A.F. and Ferreira, T.O. and Giovannoni, L.R. and de O. Gomes, L.E. and Romero, D.J. and Jimenez, L.C.Z. and Ruiz, F.",
          publisher = "Center for International Forestry Research (CIFOR)",
          title = "SWAMP Dataset-Mangrove soil carbon-Furo Grande-2017",
          year = "2019",
          doi = "10.17528/CIFOR/DATA.00166",
          url = "https://doi.org/10.17528/CIFOR/DATA.00166"
  )


# write citations
write_csv(cifor_alt_study_citations, "./data/primary_studies/CIFOR/derivative_ALT/cifor_alt_study_citations.csv")
