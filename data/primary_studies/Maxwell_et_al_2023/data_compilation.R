## 


library(tidyverse)

##### 1. Combine data ####

lst <- list.files("data/primary_studies/Maxwell_et_al_2023/03_data_format/data/exported", ".csv", full.names = TRUE)
lst <- lst[!grepl("CCRCN.csv", lst)] # remove CCRCN data

data_compile1 <- plyr::rbind.fill(lapply(lst, function(i){read.csv(i)})) ## tom.hengl@envirometrix.net


data_compile2 <- data_compile1 %>% 
  mutate(Original_source = case_when(is.na(Original_source) == TRUE ~ Source,
                                     TRUE ~ Original_source)) %>% 
  mutate(Country = fct_recode(Country, "United States" = "USA"))

sum(is.na(data_compile2$Original_source))

# coerce numeric values to numeric type
data_compile2$Latitude <- as.numeric(data_compile2$Latitude)
data_compile2$Longitude <- as.numeric(data_compile2$Longitude)
data_compile2$OC_perc <- as.numeric(data_compile2$OC_perc)
data_compile2$BD_reported_g_cm3 <- as.numeric(data_compile2$BD_reported_g_cm3)
data_compile2$SOM_perc <- as.numeric(data_compile2$SOM_perc)

## Data Cleaning ####


## year_collected
data1 <- data_compile2 %>% 
  dplyr::select(-X) %>%  #ID from each export; doesn't have meaning
  separate(Year_collected, c("Year_collected", "Year_collected_end"), sep = '-' ) %>% 
  mutate(Year_collected = as.numeric(Year_collected)) %>% 
  mutate(N_perc = round(as.numeric(N_perc),3))

# table(is.na(data1$Year_collected))
# hist(data1$Year_collected)


#### 2. data corrections ####

## note: the OC values (seemingly directly calculated from SOM) are made NAs directly in the CCRCN script
# edit: 28.03.23


### now, changing OC_perc values to NA for studies which used a generic equation
## we will then use our equation to calculate OC_perc

#### 2.1 making sure generic eq have SOM values ####

##### step 1 ####
# derive SOM from studies with OC which was calculated from SOM 
# from OC_perc column
studies_no_SOM <- data1 %>% 
  filter(Source == "Burke et al 2022" | 
           Source == "Copertino et al under review" | 
           Source == "Gu et al 2020"| 
           Source == "Perera et al 2022"| 
           Source == "Ward 2020"| 
           Source == "Wollenberg et al 2018") %>% 
  filter(Method == "LOI") %>% 
  filter(is.na(OC_perc) == FALSE & is.na(SOM_perc) == TRUE) %>% 
  droplevels()

table(studies_no_SOM$Original_source)
table(studies_no_SOM$Conv_factor)


# #OC = 0.4*OM + 0.0025*(OM^2) 
# # y = 0.0025*(x^2) + 0.4*x + 0
# 
# # https://stackoverflow.com/questions/59062234/is-there-a-code-function-that-solves-an-equation-for-either-x-or-y
# #Remember in general if you want to solve y = Ax^2 + Bx + C for a particular value 
# #of y, you have to rearrange the equation to Ax^2 + Bx + (C-y) = 0. 
# # Translated to R code this is:
# 
# # coeff <- c(C-y,B,A)
# # polyroot(coeff)
# ### to do: add if source == Gu et al
# get_SOM_from_OC <- function(a, b, c, OC){
#   coeff <- c(c-OC,b,a) 
#   findy <- polyroot(coeff)
#   y<- Re(findy[1])
#   SOM <- round(y,2)
#   return(SOM)
# }
# 
# lapply(studies_no_SOM$OC_perc, get_SOM_from_OC,c = 0, b = 0.4, a = 0.0025)
# 
# data1_gu <- data1 %>% 
#   filter(Original_source == "Gu et al 2020") %>% 
#   mutate(SOM_perc_gu = lapply(OC_perc, get_SOM_from_OC,c = 0, b = 0.4, a = 0.0025)) %>% 
#   mutate(SOM_perc_gu = as.numeric(SOM_perc_gu))
# 
# data1_rios<- data1 %>% 
#   filter(Original_source == "Rios et al 2018") %>% 
#   mutate(SOM_perc_rios = (OC_perc - 0.0008)/0.47) %>% 
#   mutate(SOM_perc_rios = round(as.numeric(SOM_perc_rios),2))
# 
# ##### step 2 ####
# # derive SOM from studies with OC mean which was calculated from SOM mean 
# # from OC_perc_mean column
# 
# studies_no_SOM_mean <- data1 %>% 
#   filter(Source  == "Burke et al 2022" | 
#            Source  == "Copertino et al under review" | 
#            Source  == "Gu et al 2020"| 
#            Source  == "Perera et al 2022"| 
#            Source  == "Ward 2020"| 
#            Source  == "Wollenberg et al 2018") %>% 
#   filter(is.na(OC_perc_mean) == FALSE & is.na(SOM_perc_mean) == TRUE) %>% 
#   droplevels()
# 
# 
# table(studies_no_SOM_mean$Original_source)
# table(studies_no_SOM_mean$Conv_factor)
# 
# # OC = 0.47*OM 
# 
# data1_perera<- data1 %>% 
#   filter(Source == "Perera et al 2022") %>% 
#   mutate(SOM_perc_perrera = OC_perc_mean/0.47) %>% 
#   mutate(SOM_perc_perrera = round(as.numeric(SOM_perc_perrera),2))
# 
# data1_costa<- data1 %>% 
#   filter(Original_source == "Costa et al 2019" ) %>% 
#   mutate(SOM_perc_costa = OC_perc_mean/0.47) %>% 
#   mutate(SOM_perc_costa = round(as.numeric(SOM_perc_costa),2))
# 
# 
# ##### step 3 ####
# 
# #combine previous two datasets
# # main data plus gu
# data2_gu <- left_join(data1, data1_gu) %>% 
#   mutate(SOM_perc = coalesce(SOM_perc, SOM_perc_gu)) %>% 
#   dplyr::select(-SOM_perc_gu)
# 
# # main data plus gu plus rios
# data2_gu_rios <- left_join(data2_gu, data1_rios) %>% 
#   mutate(SOM_perc = coalesce(SOM_perc, SOM_perc_rios)) %>% 
#   dplyr::select(-SOM_perc_rios)
# 
# # main data plus gu plus rios plus perera
# data2_gu_rios_per <- left_join(data2_gu_rios, data1_perera) %>% 
#   mutate(SOM_perc_mean = coalesce(SOM_perc_mean, SOM_perc_perrera)) %>% 
#   dplyr::select(-SOM_perc_perrera)
# 
# 
# # main data plus gu plus rios plus perera plus costa
# data2_all <- left_join(data2_gu_rios_per, data1_costa) %>% 
#   mutate(SOM_perc_mean = coalesce(SOM_perc_mean, SOM_perc_costa)) %>% 
#   dplyr::select(-SOM_perc_costa)



#### 2.2 replacing OC values from generic eq with NA #####
### removing OC_perc_combined values from studies which used a generic conversion equation
## this is so that the values used are those which have come from our generated eq

data2 <- data1 %>% 
  # mutate(OC_perc= case_when(Source == "Burke et al 2022" | 
  #                             Source == "Copertino et al under review" & Method == "LOI" | 
  #                             Source == "Gu et al 2020"| 
  #                             Source == "Ward 2020"| 
  #                             Source == "Wollenberg et al 2018" ~ NA_real_,
  #                           TRUE ~ OC_perc)) %>% 
  mutate(OC_perc_mean= case_when(Source == "Perera et al 2022"| 
                                   Source == "Copertino et al under review" & 
                                   Method == "LOI" |
                                   Original_source == "Idaskin et al 2015" ~ NA_real_,
                                 TRUE ~ OC_perc_mean)) %>% 
  mutate(SOM_perc_mean = case_when(Source == "Costal et al 2019" ~ NA_real_,
                                   TRUE ~ SOM_perc_mean)) # seems that SOM was derived from OC. paper says TOC used



#### 3. merge columns ####

#to merge OC_perc with OC_perc_mean with Soil_OCcon_mean

data3 <- data2 %>% 
  #in column OC_perc_sd, keep OC_perc_sd values, and when empty fill with OC_perc_SD, or OC_perc_s values
  mutate(OC_perc_sd = coalesce(OC_perc_sd, OC_perc_SD, OC_perc_s)) %>% 
  mutate(OC_perc_mean = coalesce(OC_perc_mean, Soil_OCcon_mean, Soil_TCcon_mean)) %>% 
  mutate(SOM_perc_sd = coalesce(SOM_perc_sd, SOM_perc_SD)) %>% 
  mutate(BD_reported_g_cm3_mean = coalesce(BD_reported_g_cm3_mean, BD_g_cm3_mean, Soil_BD_mean)) %>% 
  mutate(BD_reported_g_cm3_sd = coalesce(BD_reported_g_cm3_sd, BD_g_cm3_SD, Soil_BD_SD)) %>% 
  mutate(BD_reported_g_cm3_se = coalesce(BD_reported_g_cm3_se, BD_g_cm3_se)) %>% 
  mutate(n_cores = coalesce(n_cores, Soil_OCcon_n, Soil_BD_n, n)) %>% 
  mutate(N_perc = coalesce(N_perc, Ntot_perc, TN_perc)) %>% 
  mutate(Admin_unit = coalesce(Nation, Emirate, State)) %>% 
  mutate(Treatment = coalesce(Treatment, Marsh_type)) %>% 
  mutate(Time_replicate = coalesce(Season, Replicate)) %>% 
  dplyr::select(-c(OC_perc_SD, OC_perc_s, Soil_OCcon_mean, Soil_TCcon_mean, SOM_perc_SD,
                   BD_g_cm3_mean, Soil_BD_mean, BD_g_cm3_SD, Soil_BD_SD, BD_g_cm3_se,
                   Soil_OCcon_n, Soil_BD_n, Ntot_perc, TN_perc, Nation, Emirate, State,
                   accuracy_code, N_perc, TC_mg_g, OC_mg_g, TN_mg_g, IC_mg_g, Replicate, DataID, Soil_OCcon_SD,
                   "Soil_OCden_n",           "Soil_OCden_mean",        "Soil_OCden_SD",          "Soil_TCcon_n",          
                   "Soil_TCcon_SD",          "Soil_TCden_n",           "Soil_TCden_mean",        "Soil_TCden_SD",         
                   "Soil_CS_n",              "Soil_CS_mean",           "Soil_CS_SD", Admin_unit, Compiled_by,
                   C_stock_MgC_ha, "IC_perc",                "Ctot_perc",              "Cinorg_perc")) 
                   # Marsh_type, Season, ))

           
             
[13] "Year_collected"         "Year_collected_end"     "U_depth_m"              "L_depth_m"             
[17] "Method"                 "OC_perc"                "BD_reported_g_cm3"      "DOI"                   
[21] "Core"                   "SOM_perc"               "SOM_perc_Heiri"         "N_perc"                
[25] "Conv_factor"            "Depth_to_bedrock_m"     "Original_source"        "SOM_perc_mean"         
[29] "SOM_perc_sd"            "OC_perc_mean"           "OC_perc_sd"             "OC_perc_se"            
[37] "Season"                 "Treatment"              "n_cores"                "BD_reported_g_cm3_mean"
[41] "BD_reported_g_cm3_sd"   "Soil_type"              "Replicate"              "n"                     
[45] "BD_reported_g_cm3_se"   "Publication"                           ""         
 "Species"               
[61] "Subsite"                "Carbonate_removed"      "C_stock_MgC_ha"         "Core_type"             
[65] "Marsh_type"             "Marsh_zone"             "delta_c13"                      
 "BD_interpol_g_cm3"     
[73] "Admin_unit"             

#### check that data isn't being omitted 
#Ctot_perc: only Shamrikova included Ctot_perc - OC_perc is also calculated from study 
#OC_mg_g already calcualted to OC_perc (Ewers Lewis et al 2020)
#TC_mg_g study also has OC_mg_G (Ewers Lewis et al 2020)
#need to KEEP C_stock_MgC_ha (Meng et al 2019 review only has this data)


data4 <- data3 %>% 
  dplyr::select(Source:Original_source, Soil_type, Conv_factor, Core, Admin_unit, Depth_to_bedrock_m,
                Treatment, Time_replicate, n_cores, SOM_perc_mean:OC_perc_se, BD_reported_g_cm3_mean, 
                BD_reported_g_cm3_se, BD_reported_g_cm3_sd)


#### 4. add data type & combining raw with site-level data  ####

data5 <- data4 %>% 
  mutate(Data_type = case_when(Source == "Cusack et al 2018" | Source == "Gailis et al 2021" |
                                 Source == "Gispert et al 2020" | Source == "Gispert et al 2021" |
                                 Source == "Perera et al 2022" | Source == "Rathore et al 2016" |
                                 Source == "Yang et al 2021" | Source == "Yu and Chmura 2010" |
                                 Source == "Yuan et al 2019" | Source == "Voltz et al 2021"
                               | Source == "Hayes et al 2014" | Source == "Day et al 2011"
                               | Source == "Ferronato et al 2019"
                               | Source == "Fuchs et al 2018" | Source == "Hatton et al 1983"
                               | Source == "Loomis and Craft 2010" | Source == "Macreadie et al 2017"
                               | Source == "Morris and Jensen 1998"| Source == "Sousa et al 2010"
                               | Source == "Sun et al 2019"| Source == "Ye et al 2015" 
                               | Source == "Zubrzycki et al 2013" ~ "Site-level",
                               Source == "Copertino et al under review" | 
                                 Source == "Fu et al 2021" | Source == "He et al 2020" |
                                 Source == "Hu et al 2020" | Source == "Meng et al 2019" |
                                 Source == "Wails et al 2021" | 
                                 Original_source == "Gao et al 2016"
                               ~ "Review",
                               TRUE ~ "Core-level")) %>% 
  relocate(Data_type, .before = OC_perc) %>% 
  mutate(OC_perc_combined = coalesce(OC_perc, OC_perc_mean)) %>% 
  mutate(SOM_perc_combined = coalesce(SOM_perc, SOM_perc_mean)) %>% 
  mutate(BD_reported_combined = coalesce(BD_reported_g_cm3, BD_reported_g_cm3_mean))



#### 5. remove rows with neither OC nor SOM data ####

data6 <- data5 %>% 
  filter(is.na(OC_perc_combined) == FALSE | 
           is.na(SOM_perc_combined) == FALSE)


#### 6. edit habitat type ####

table(data6$Habitat_type)

data7 <- data6 %>% 
  filter(Habitat_type != "tidal_freshwater_marsh",
         Habitat_type != "Freshwater" ,
         Habitat_type != "low sabkha" ,
         Habitat_type != "high sabkha")


#### 7. edit conversion factors ####



data8 <- data7 %>% 
  mutate(Conv_factor = as.factor(Conv_factor)) %>% 
  mutate(Conv_factor = fct_recode(Conv_factor, 
                                  "OC = 0.47*OM + 0.0008*(OM^2)" = "%Corg = 0.47 x %LOI + 0.0008 x (%LOI)^2 ", #careful! space at end here
                                  "OC = 0.4*OM + 0.0025*(OM^2)" = "(0.4*%LOI)+0.0025*(%LOI^2)",
                                  "OC = 0.8559*OM + 0.1953" = "(0.8559*SOM_perc)+0.1953",
                                  "OC = 1.1345*OM - 0.8806" = "(1.1345*SOM_perc)-0.8806",
                                  "OC = 0.22*(OM^1.1)" = "0.22*x^1.1", 
                                  "OC = 0.4*OM + 0.0025*(OM^2)" = "0.4*x + 0.0025*x2", 
                                  "OC = 0.40*OM + 0.025*(OM^2)" = "0.40*x + (0.0025*x)2", 
                                  "OC = 0.4068*OM + 0.6705" = "0.4068x+0.6705", 
                                  "OC = 0.44*OM - 1.33" = "0.44x -1.33", 
                                  "OC = 0.47*OM" = "0.47 from Craft 1991", 
                                  "OC = 0.47*OM + 0.0008" = "0.47*x+0.0008", 
                                  "OC = 0.58*OM" = "0.58", 
                                  "OC = 0.461*OM - 0.266" = "OC (% dw) = 0.461 x OM - 0.266", 
                                  "OC = 0.3102*OM - 0.066" = "OC = -0.066 + 0.3102*OM", 
                                  "OC = OM/1,724" = "SOM/1,724")) 
# note: the conversion factor used in de los Santos et al (2022 a) was developped from the data from Santos et al. 2019 
# both are from the Ria Formosa lagoon
# also, Kohlenfeld et al 2022 has both data used to develop the conversion factor and data where it was applied

table(data8$Conv_factor)




#### 8. edit accuracy flag ####

data9 <- data8 %>% 
  mutate(accuracy_flag = fct_recode(accuracy_flag, 
                                    "direct from dataset" = "exact",
                                    "direct from dataset" = "RTK GPS",
                                    "estimated from GE" = "estimated from GEE"
  ))

#### 9. NA for SOM > 100, SOM / BD/ OC <0 ####
data10 <- data9 %>% 
  mutate(SOM_perc_combined = case_when(is.na(SOM_perc_combined) == FALSE & 
                                         SOM_perc_combined > 100 ~ NA_real_,
                                       is.na(SOM_perc_combined) == FALSE & 
                                         SOM_perc_combined < 0 ~ NA_real_,
                                       TRUE ~ SOM_perc_combined)) %>% 
  mutate(BD_reported_combined = case_when(is.na(BD_reported_combined) == FALSE & 
                                            BD_reported_combined < 0 ~ NA_real_,
                                          TRUE ~ BD_reported_combined)) %>% 
  mutate(OC_perc_combined = case_when(is.na(OC_perc_combined) == FALSE & 
                                        OC_perc_combined < 0 ~ NA_real_,
                                      TRUE ~ OC_perc_combined))

##rearrange DOI column

data11 <- data10 %>% 
  relocate(DOI, .after = BD_reported_combined)

#### 9. export cleaned data ####

path_out = 'reports/04_data_process/data/'

export_file <- paste(path_out, "data_cleaned.csv", sep = '') 
export_df <- data11

write.csv(export_df, export_file, row.names = F)

