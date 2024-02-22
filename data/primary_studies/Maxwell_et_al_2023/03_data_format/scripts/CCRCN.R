## import data from CCRCN
## need to cite individual studies
## export for marsh soil C 
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 07.07.22


library(tidyverse)


#### compare bibliography March 2023 vs March 2022

CCRCN_bibliography_2022 <- read_csv("~/07_Cam_postdoc/Data/CCRCN_data_03_2022/CCRCN_bibliography_2022.csv") 
CCRCN_bibliography <- read_csv("reports/03_data_format/data/core_level/CCRCN_data/CCRCN_bibliography.csv")

studies_2022 <- CCRCN_bibliography_2022 %>% 
  distinct(study_id, .keep_all = FALSE)

studies_2023 <- CCRCN_bibliography %>% 
  distinct(study_id, .keep_all = FALSE)

studies_added <- anti_join(studies_2023, studies_2022)
studies_removed <- anti_join(studies_2022, studies_2023)


input_file1 <- "reports/03_data_format/data/core_level/CCRCN_data/CCRCN_depthseries.csv"
input_file2 <- "reports/03_data_format/data/core_level/CCRCN_data/CCRCN_cores.csv"

input_depthseries <- read.csv(input_file1)
input_cores <- read.csv(input_file2)

input_data01 <- right_join(input_cores, input_depthseries,
                        by = c("study_id", "core_id"))


#### reformat to match other datasets
input_data01$study_id <- input_data01$study_id %>% 
  fct_relabel(~ gsub("_", " ", .x)) %>% 
  fct_relabel(~ gsub("etal", "et al", .x))%>% 
  fct_relabel(~ gsub("\\(", "", .x)) %>% 
  fct_relabel(~ gsub("\\)", "", .x))



input_data02 <- input_data01 %>% 
  mutate(depth_min_cm = coalesce(depth_min, representative_depth_min),
         depth_max_cm = coalesce(depth_max, representative_depth_max)) %>% 
  mutate(U_depth_m = depth_min_cm/100, #cm to m
         L_depth_m = depth_max_cm/100,
         Source = "CCRCN") %>%  #cm to m
  dplyr::rename(Original_source = study_id,
                Country = country,
                Latitude = latitude,
                Longitude = longitude,
                Site = site_id.x,
                Core = core_id,
                Habitat_type = habitat,
                State = admin_division,
                Year_collected = year) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Method = "EA") # LOI measurements have not been converted to OC_perc

test <- input_data02 %>% 
  filter(is.na(U_depth_m) == TRUE) # note: boyd et al 2017 doesn't have depth increments

#making carbon data  match main dataset
input_data03 <- input_data02 %>% 
  mutate(OC_perc = fraction_carbon*100,
         SOM_perc = fraction_organic_matter*100) %>% 
  dplyr::rename(BD_reported_g_cm3 = dry_bulk_density)


### removing Schile-Beers_and_Megonigal_2017 dataset
# same dataset with more information is available from DRYAD
#https://datadryad.org/stash/dataset/doi:10.15146/R3K59Z


### also, 23.03.2023 now removing datasets which we'd already added to our dataset
## Burden et al 2018, Ward et al 2021

input_data04 <- input_data03 %>% 
  filter(Original_source != "Schile-Beers and Megonigal 2017", 
           Original_source != "Burden et al 2018",
         Original_source != "Ward et al 2021") # already in the data 


#### checking SOC vs SOM to make sure OC values have not been estimated from SOM


SOM_OC_CCRCN <- input_data05 %>% 
  filter(is.na(SOM_perc) == FALSE & is.na(OC_perc) == FALSE) %>% 
  ggplot(aes(x = SOM_perc, y = OC_perc))+
  theme_bw()+
  stat_poly_line()+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))+
  geom_point()+
  labs(x = "SOM (%)", y = "OC (%)")+
  facet_wrap(~Original_source)

SOM_OC_CCRCN


#### for studies where OC was estimated from SOM, remove the OC values

input_data05 <- input_data04 %>% 
  mutate(OC_perc = case_when(Original_source == "Keshta et al 2020" ~ NA_real_,
                                 Original_source == "Rodriguez et al 2022" ~ NA_real_,
                                 Original_source == "Elsey Quirk et al 2011" ~ NA_real_,
                             TRUE ~ OC_perc))


#### export

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Original_source, Core, Habitat_type, Country, State, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Method,
                U_depth_m, L_depth_m, OC_perc, SOM_perc, BD_reported_g_cm3)


## export

source_name <- "CCRCN"
path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)

