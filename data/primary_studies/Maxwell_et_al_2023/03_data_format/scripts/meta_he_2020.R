## import data from He et al 2020, supplementary info
# https://doi.org/10.1098/rstb.2019.0451 
## meta-analysis, effect of consumer 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 26.07.22
# edit 20.12.22

library(tidyverse)


input_file01 <- "reports/03_data_format/data/meta_analysis/He_2020_SI/He_2020_SI.csv"

input_data01 <- read.csv(input_file01)

input_data01 <- input_data01 %>% 
  slice(1:947) %>% 
  dplyr::select(-X)

input_file02 <- "reports/03_data_format/data/meta_analysis/He_2020_SI/He_2020_SI_studies.csv"

study_IDs <- read.csv(input_file02)

input_data02 <- full_join(input_data01, study_IDs, by = "PublicationID")



##### add informational  
source_name <- "He et al 2020"
author_initials <- "QH"

## renaming author names
input_data02$Author <- as.factor(input_data02$Author) 
levels(input_data02$Author) <- gsub("_", " ", levels((input_data02$Author)))


input_data03 <- input_data02 %>% 
  rename(Habitat_type = Ecosystem) %>% 
  mutate(Original_source = paste(Author, Year, sep = " ")) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, DataID),
         Country_detail = Country)


### renaming country names
## wanted to separate USA and state by _
#first need to separate The_Netherlands to Netherlands
input_data03$Country <- as.factor(input_data03$Country) 
levels(input_data03$Country) <- gsub("The_", "", levels((input_data03$Country)))


input_data04 <-  input_data03 %>% 
  separate(Country, c("Country", "State"), sep = '_')   #separate country and state


#### reformat data ####

input_data05 <- input_data04 %>% 
  rename(Publication = X,
         Year_collected = Year) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "2") %>% 
  mutate(Method = NA) %>% 
  separate(Soil_core_depth, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m


#### filter data ####

input_data05$Carbon_measure <- as.factor(input_data05$Carbon_measure) 

input_data_soil1 <- input_data05 %>% 
  filter(Carbon_measure == "Soil_OCcon" | Carbon_measure == "Soil_OCden" | 
           Carbon_measure == "Soil_TCcon" | Carbon_measure == "Soil_TCden" | 
           Carbon_measure == "Soil_BD" | Carbon_measure == "Soil_CS") %>% 
  filter(Habitat_type == "Marsh")

str(input_data_soil1)


### pivot table to extract relevant data 
input_data_soil2 <- input_data_soil1 %>% 
  pivot_longer(cols = c("n1":"SD2")) %>% 
  mutate(Treatment = case_when(name == "n1"|
                                 name == "mean1" |
                                 name == "SD1" ~ "Control",
                               name == "n2"|
                                 name == "mean2" |
                                 name == "SD2" ~ "Consumer presence")) %>% 
  mutate(name = fct_recode(name, "n" = "n1", "mean" = "mean1", "SD" = "SD1",
                           "n" = "n2", "mean" = "mean2", "SD" = "SD2"))

input_data_soil3 <- input_data_soil2 %>% 
  pivot_wider(names_from = c("Carbon_measure","name"),
              values_from = "value")

## removing sand layer depths 
## removing lab studies - none located for the study_venue (only field studies)

input_data_soil4 <- input_data_soil3 %>% 
  filter(U_depth_cm != "Sand layer") 


#### edit incorrect site location for Laperouse Bay 
# Wilson Jefferies 1996
# map from https://www.researchgate.net/publication/255620701_Goose-induced_Changes_in_Vegetation_and_Land_Cover_between_1976_and_1997_in_an_Arctic_Coastal_Marsh/figures?lo=1 
# new coordinate: 58.687124, -93.439282

input_data_soil5 <- input_data_soil4 %>% 
  mutate(Latitude = case_when(Original_source == "Wilson Jefferies 1996" ~ 58.687124,
                              TRUE ~ Latitude),
         Longitude = case_when(Original_source == "Wilson Jefferies 1996" ~ -93.439282,
                                                                    TRUE ~ Longitude))
         

#### export ####

export_data01 <- input_data_soil5 %>% 
  dplyr::select(Source, Original_source, Site_name, DataID, Habitat_type, Country, State, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code, Treatment, Publication,
                U_depth_m, L_depth_m, Method, Soil_OCcon_n:Soil_CS_SD)


export_data02 <- export_data01 %>% 
  relocate(Source, Original_source, Publication, Site_name, DataID, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, State, Year_collected, .before = U_depth_m) %>% 
  arrange(DataID)


###removing treatment values

export_data03 <- export_data02 %>% 
  filter(Treatment == "Control")

## keeping only OC concentration and BD values 
export_data04 <- export_data03 %>% 
  filter(Soil_OCcon_mean != is.na(Soil_OCcon_mean) |
           Soil_TCcon_mean != is.na(Soil_TCcon_mean) |
           Soil_BD_mean != is.na(Soil_BD_mean) )

table(export_data04$Country)


## removing data extracted elsewhere
## in CCRCN: Nolte et al. 2013 Does livestock grazing affect sediment deposition and accretion rates in salt marshes? Estuarine, Coastal and Shelf Science 135, 296-305. 
## YU and Chmura 2009, Sammul et al 2012

table(export_data04$Original_source)
export_data05 <- export_data04 %>%  
  filter(Original_source != "Nolte et al 2013" & 
         Original_source != "Yu Chmura 2009" & 
         Original_source != "Sammul et al 2012") %>% 
  mutate(DOI = "https://doi.org/10.1098/rstb.2019.0451")


# 
# paste3 <- function(df) {
#   if (df[, "U_depth_m"] > 0) {
#     Column_3 = Column_3-1
#   } else
#   Column_3 = seq(1:50)
#   return(Column_3)
# }
# 
# paste3(df = export_data05)
# 
# ## making a plot column
# # https://stackoverflow.com/questions/6112803/how-to-create-a-consecutive-group-number
# # note: this doesn't actually work because some cores have the same location
# export_data06 <- export_data05 %>% 
#   group_by(Original_source, Latitude, Longitude) %>% 
#   mutate(Column_3 = cur_group_id()) %>% 
#   # mutate(Column_3 = accumulate(U_depth_m == 0 , ~ifelse(.y==FALSE, 1, .x))) %>% 
#   relocate(Column_3, .before = U_depth_m)

## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data05

write.csv(export_df, export_file)



               