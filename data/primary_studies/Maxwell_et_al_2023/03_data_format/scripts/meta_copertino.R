## import data from Margareth Copertino, southwestern Atlantic salt marsh review, under review
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 17.10.22


library(tidyverse)
input_file01 <- "reports/03_data_format/data/meta_analysis/Copertino_review_email/Copertino_Review.csv"

input_data01 <- read.csv(input_file01) 

##reformat style of data

input_data02 <- input_data01 %>% 
  slice(1:286) %>%  #drop rows with NAs
  rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%    #replacing . in columns by _
  mutate(Original_source = gsub("\\.", "", Original_source)) %>% 
  mutate(Original_source = gsub("\\.", "", Original_source))


input_data02[,c("SOM_perc", "SOM_perc_mean", 
                "SOM_perc_sd", "OC_perc_mean","OC_perc_sd", "OC_perc")] <- lapply(
                  input_data02[,c("SOM_perc", "SOM_perc_mean", 
                                  "SOM_perc_sd", "OC_perc_mean","OC_perc_sd", "OC_perc")], 
                  gsub, pattern = "\\,", replacement = "\\.")

input_data02 <- input_data02 %>% 
  mutate(across(N_samples:g_C_Kg_1,str_trim)) %>% 
  mutate(across(SOM_perc:g_C_Kg_1, as.numeric))

### remove data in TM_commment that is to be deleted (data estimated from figure)

input_data03 <- input_data02 %>% 
  filter(TM_comment == "OK" | 
           TM_comment == "changed_data"| 
           TM_comment == "changed_lat_long_seetab")




##### add informational  
source_name <- "Copertino et al under review"
author_initials <- "MC"


input_data04 <- input_data03 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core_ID),
         Habitat_type = "Salt marsh") %>% 
  rename(Core = Core_ID)


#### reformat data ####

input_data05 <- input_data04 %>% 
  mutate(Conv_factor = "0.47*x+0.0008",
         BD_estimated_factor = "(2.684-140.943*0.008)*exp(-0.008*OC (g.kg-1))",
    accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  rename(Method = METHOD) %>% 
  mutate(Method = fct_recode(Method, "EA" = "CHN"),
         Country = fct_recode(Country, "Brazil" = "BR", 
                              "Argentina" = "UK/Argentina")) %>% 
  # calculating a sd for OC_perc when OC is calculated from SOM with a mean and sd
  #using same conversion factor
  mutate(OC_perc_sd = case_when(is.na(OC_perc_mean) == TRUE & is.na(SOM_perc_sd) == FALSE ~ (0.47*SOM_perc_sd + 0.0008),
                                TRUE ~ OC_perc_sd))



## edit depth

input_data06 <- input_data05 %>% 
  separate(Section_interval_cm_, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

test <- input_data06 %>% 
  filter(is.na(L_depth_cm) == TRUE) ##kauffman study removed anyway (data already imported)

#### remove data from references already used ####
# kauffman

input_data07 <- input_data06 %>% 
  filter(Original_source != "Kaufmann et al 2018") %>% 
  droplevels()



#### rename studies 

input_data08 <- input_data07 %>% 
  mutate(Original_source = fct_recode(Original_source, "Idaszkin et al 2014" = "Idaskin et al 2014",
                                      "Otero et al 2006" = "Ottero et al 2006",
                                      "Rabelo 2012" = "Rebelo 2012"))


#### add year_collected and method to complete data paper #### 


input_data09 <- input_data08 %>% 
  dplyr::select(-c(Year_collected, Method)) %>% 
  mutate(Year_collected = case_when(Original_source == "Adaime 1978" ~ 1974,
                                    Original_source == "Azevedo 2015-UNPUBLISHED" ~ 2012,
                                    # Original_source == "Bouza et al 2017" ~ , # not presented in paper Bouza et al 2007 
                                    Original_source == "Braga et al 2011" ~ 2007,
                                    Original_source == "Costa et al 2019" ~ 2016,
                                    Original_source == "Ferreira 2008" ~ 2005,
                                    Original_source == "Flynn et al 1998" ~ 1988,
                                    Original_source == "Hidalgo et al 2021" ~ 2011,
                                   # Original_source == "Idaszkin et al 2014" ~ , # not presented in paper
                                  #  Original_source == "Idaszkin et al 2015" ~ # not presented in paper
                                  #Original_source == "Lacerda et al 1997" ~ , # not presented in paper
                                  Original_source == "Marinho et al 2018" ~ 2013,
                                  Original_source == "Negrin et al 2019" ~ 2011,
                                  Original_source == "Neto & Lana 1997" ~ 1991,
                                  Original_source == "Newton 2017" ~ 2013, # core-level
                                  Original_source == "Otero et al 2006" ~ 2004,
                                 # Original_source == "Payne et al 2019" ~ , # not presented in paper
                                  Original_source == "Rabelo 2012" ~ 2009,
                                #  Original_source == "Rios et al 2018" ~ ,# not presented in paper
                                  Original_source == "UNPUBLISHED" ~ 2005, 
                                  Original_source == "Zanin 2003" ~  2000)) %>% 
  
  ## year_collected_end when sampling over several years
  mutate(Year_collected_end = case_when(Original_source == "Adaime 1978" ~ 1975,
                                        Original_source == "Flynn et al 1998" ~ 1989,
                                        Original_source == "Marinho et al 2018" ~ 2014,
                                        Original_source == "Negrin et al 2019" ~ 2012,
                                        Original_source == "Zanin 2003" ~  2001)) %>%
  
  ##method for measuring OC or SOM
  mutate(Method = case_when(Original_source == "Adaime 1978" ~ "Wilson (1973)",
                            Original_source == "Azevedo 2015-UNPUBLISHED" ~ "LOI", #best guess
                            Original_source == "Bouza et al 2017" ~ " Tyurin (1951)", # data from Bouza et al 2007  https://doi.org/10.1016/j.geoderma.2007.01.001
                            Original_source == "Braga et al 2011" ~ "LOI", #SOM at 550C for 4h
                            Original_source == "Costa et al 2019" ~ "EA",
                            Original_source == "Ferreira 2008" ~ "EA",
                            Original_source == "Flynn et al 1998" ~ "SOM by Suguio (1973)",
                            Original_source == "Hidalgo et al 2021" ~ "LOI",
                            Original_source == "Idaszkin et al 2014" ~ "LOI",
                            Original_source == "Idaszkin et al 2015" ~ "LOI",
                          Original_source == "Lacerda et al 1997" ~ "LOI", #450C for 16h
                          Original_source == "Marinho et al 2018" ~ "LOI", #450C for 4h
                          Original_source == "Negrin et al 2019" ~ "LOI", # not mentioned in paper "Organic matter (OM) content was
                         # evaluated in the same sediment samples collected for metal analyses" using LOI as best guess
                          Original_source == "Neto & Lana 1997" ~ "LOI", #Dried samples were combusted at 550C for 60 min in order to determine
                         #organic content, and at 1000C for one additional hour to determine carbonates
                          Original_source == "Newton 2017" ~ "LOI", #The method is based on a two-step reaction whereby OM (carbon) is oxidised at 500-550oC 
                         #and lost from the sediment as carbon dioxide (CO2) and at 900-1000oC CO2 is released from CaCO3
                          Original_source == "Otero et al 2006" ~ "EA",
                          Original_source == "Payne et al 2019" ~ "LOI",
                          Original_source == "Rabelo 2012" ~ "LOI",
                          Original_source == "Rios et al 2018" ~ "LOI", # 430C after dehydratation at 105C for 12 
                          Original_source == "UNPUBLISHED" ~ "EA",
                          Original_source == "Zanin 2003" ~ "LOI")) %>%  #450C for 2h
  mutate(DOI = case_when(Original_source == "Adaime 1978" ~ "https://doi.org/10.1590/S0373-55241978000200001",
                         #  Original_source == "Azevedo 2015-UNPUBLISHED" ~ ,
                         Original_source == "Bouza et al 2017" ~ "https://doi.org/10.1007/978-3-319-48508-9_7",
                         Original_source == "Braga et al 2011" ~ "https://doi.org/10.1007/s11273-011-9215-5",
                         Original_source == "Costa et al 2019" ~ "https://doi.org/10.1007/s12237-019-00596-0",
                         Original_source == "Ferreira 2008" ~ "https://doi.org/10.13140/RG.2.2.21597.61924" ,
                         Original_source == "Flynn et al 1998" ~ "Macrobenthic Associations of the Lower and Upper Marshes of a Tidal Flat Colonized by Spartina alterniflora in Cananeia Lagoon Estuarine Region (Southeastern Brazil). BULLETIN OF MARINE SCIENCE, 63(2): 427–442, 1998",
                         Original_source == "Hidalgo et al 2021" ~ "https://doi.org/10.1016/j.ecss.2021.107534" ,
                         Original_source == "Idaszkin et al 2014" ~ "http://dx.doi.org/10.1016/j.marpolbul.2014.10.001",
                         Original_source == "Idaszkin et al 2015" ~ "http://dx.doi.org/10.1016/j.marpolbul.2015.09.047",
                         Original_source == "Lacerda et al 1997" ~ "https://doi.org/10.1023/A:1009990604727",
                         Original_source == "Marinho et al 2018" ~ "https://doi.org/10.1007/s10661-018-6975-x",
                         Original_source == "Negrin et al 2019" ~ "https://doi.org/10.1016/j.scitotenv.2018.08.357",
                         Original_source == "Neto & Lana 1997" ~ "https://doi.org/10.1006/ecss.1996.0154",
                         Original_source == "Newton 2017" ~ "http://hdl.handle.net/10026.1/9650",
                         Original_source == "Otero et al 2006" ~ "https://doi.org/10.1016/j.apgeochem.2006.07.012",
                         Original_source == "Payne et al 2019" ~ "https://doi.org/10.1016/j.quascirev.2019.03.022",
                         Original_source == "Rabelo 2012" ~ "Interações entre as comunidades macrobentônicas e os fatores ambientais associadas à marismas de Spartina alterniflora Loisel (1807) na Península Bragantina (Ajuruteuae furo Grande), Pará, Brasil. Rabelo, Dayanne Mary Lima.
                         Trabalho de Conclusão de Curso (graduação em oceanografia) – Universidade Federal do Pará, Instituto de Geociências, Faculdade de Oceanografia, Belém, 2012.",
                         Original_source == "Rios et al 2018" ~ "https://doi.org/10.1016/j.jsames.2018.04.015",
                        # Original_source == "UNPUBLISHED" ~ ,
                         Original_source == "Zanin 2003" ~ "ASPECTOS ECOLÓGICOS DA MARISMA DA ENSEADA DE RATONES, ILHA DE SANTA CATARINA, SC. VANESSA TODESCATO CATANEO ZANIN. 
                        UNIVERSIDADE FEDERAL DE SANTA CATARINA PROGRAMA DE PÓS-GRADUAÇÃO EM BIOLOGIA VEGETAL, FLORIANÓPOLIS - SC, 2003."))




#### correct study location 

input_data10 <- input_data09 %>% 
  mutate(Latitude = case_when(Original_source == "Newton 2017" ~ -51.826229,
                              Original_source == "Otero et al 2006" ~ -24.996546,
                              TRUE ~ Latitude)) %>% 
  mutate(Longitude = case_when(Original_source == "Newton 2017" ~ -58.595719,
                               Original_source == "Otero et al 2006" ~ -47.907042,
                               TRUE ~ Longitude))
  
# note: Payne et al has coarse GPS location - likely that Lat is -51.829208 and Long -58.595384 to be in saltmarsh

#### export ####
export_data01 <- input_data10 %>% 
  dplyr::select(Source, Site_name, Original_source, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, Conv_factor, SOM_perc,
                SOM_perc_mean, SOM_perc_sd, OC_perc, OC_perc_mean, OC_perc_sd,
                BD_reported_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Original_source, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.1038/s43247-023-00828-z")


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)


