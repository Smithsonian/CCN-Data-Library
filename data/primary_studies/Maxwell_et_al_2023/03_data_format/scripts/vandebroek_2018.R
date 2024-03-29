## import data from Van de Broek et al. (2018), Mendeley data from Global Change Biology
## from 
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 21.07.22
# edit 20.12.22

library(readxl)  
library(tidyverse)
library(measurements)


##read all excel sheets
#https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames 
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, col_names = F, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

##### importing bulk density ####

input_file01 <- "reports/03_data_format/data/core_level/VandeBroek_2018_Mendeley_Data/Bulk density.xlsx"


mysheets <- read_excel_allsheets(input_file01)

#remove the information list
mysheets_noinfo <- mysheets[-1]


###extracting the depth and average bulk density from each sheet - this is the 1st and 6th columns

columns <- c("...1", "...3")

bulkdensity0 <- lapply(mysheets_noinfo, '[', columns)

##adding a column with site name
bulkdensity1 <- Map(cbind, bulkdensity0, Site = names(bulkdensity0))

##renaming the columns
colnames <- c("Depth_m", "BD_reported_g_cm3", "Site")
bulkdensity2 <- lapply(bulkdensity1, setNames, colnames)

##convert from list to dataframe
bulkdensity_df0 <- do.call(rbind.data.frame, bulkdensity2)

# this splits up the list to different data frames - not to use
#list2env(bulkdensity1,envir=.GlobalEnv)



##renaming the rows
rownames(bulkdensity_df0) <- 1:nrow(bulkdensity_df0)


###removing useless rows
bulkdensity_df1 <- bulkdensity_df0 %>%
  arrange(Depth_m) %>% 
  slice(-c(108:143)) %>% 
  arrange(Site) %>% 
  mutate(BD_reported_g_cm3 = as.numeric(BD_reported_g_cm3))

bulkdensity_df2 <- bulkdensity_df1 %>% 
  separate(Depth_m, c("U_depth_m", "L_depth_m"), sep = ' - ') %>% 
  mutate(U_depth_m = as.numeric(U_depth_m),
         L_depth_m = as.numeric(L_depth_m)) %>% 
  mutate(mid_depth_m = (L_depth_m + U_depth_m)/2)


bulkdensity_df3 <- bulkdensity_df2 %>% 
  mutate(Site = gsub("Young", "young",
                     gsub("Old", "old", Site)))

BD_final <- bulkdensity_df3


##### importing soil carbon ####

input_file02 <- "reports/03_data_format/data/core_level/VandeBroek_2018_Mendeley_Data/Organic_carbon.csv"

soilcarbon0 <- read.csv(input_file02)

##renaming sites to match

soilcarbon1 <- soilcarbon0 %>% 
  mutate(Site = gsub("Young", "young",
                     gsub("Old", "old", Site)))

soilcarbon1$Site <- str_trim(soilcarbon1$Site) 

  
### import locations

input_file03 <- "reports/03_data_format/data/core_level/VandeBroek_2018_Mendeley_Data/locations.csv"

locations0 <- read.csv(input_file03)

locations1 <- locations0 %>% 
  rename(lat_detail = Latitude,
         long_detail = Longitude,
         Latitude = Lat.DD,
         Longitude = Long.DD) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1")

##replace all "low" with "young" and "high" with "old"

locations2 <- locations1 %>% 
  mutate(Site = gsub("low", "young",
                     gsub("high", "old", Site))) 

locations2$Site <- str_trim(locations2$Site)

###merge soil carbon

soilcarbon2 <- full_join(soilcarbon1,locations2, by = "Site") %>% 
  slice(1:301)


##### edit dataset with info ####


##### add informational  
source_name <- "Van de Broek et al 2018"
author_initials <- "MVdB"


soilcarbon3 <- soilcarbon2 %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Site),
         Country = "Belgium",
         Method = "EA",
         Year_collected = "2016") %>% 
  rename(OC_perc = OC......mass.spec.)


## edit depth

soilcarbon4 <- soilcarbon3 %>% 
  group_by(Site) %>% 
  mutate(U_depth_m = Depth..m. - 0.015,
         L_depth_m = Depth..m. + 0.015) %>% 
  mutate(mid_depth_m = Depth..m.) %>% 
  mutate(Core = Site)
 

SOC_final <- soilcarbon4 %>%  # to use in the function         
 mutate(Site = as.factor(Site))


#### interpolate BD to SOC dataset ####

## function to interpolate BD, measured at fewer depth intervals, to the measured SOC intervals
# linear interpolation, recommended by author 

BD_interpol <- function(site){
  #input site name
  
  df2 <- SOC_final  %>% #data frame with SOC values
    filter(Site == site)
  
  BD <- BD_final %>% # data frame with BD values
    filter(Site == site)
  
  #function to linear interpolate from mid depth of bulk density DF to mid depth of SOC dataset
  a <- approx(x = BD$mid_depth_m, y = BD$BD_reported_g_cm3, 
              xout = df2$mid_depth_m, method = "linear", rule = 2 ) # rule = 2 to have values at the ends
  
  df2$depth_check <- a$x # to make sure the mid-depth is correct
  
  df2$BD_interpol_g_cm3 <- a$y # this is the interpolated BD
  
  
  return(df2)
}


list_soilcarbon5 <- lapply(unique(soilcarbon4$Site), BD_interpol) #applying BD_interpol to all levels of Site

soilcarbon5 <- as.data.frame(do.call(rbind,list_soilcarbon5 )) #binding and unlisting

test1 <- BD_interpol(site = "Sloehaven old") # checking data matches
test2 <- BD_interpol(site = "Hellegat young")

# # script from Lukas 
# sites = unique(SOC_final$Site)
# 
# # empty container to hold the results
# new_data = c()
# # loop through the sites
# for (s in sites){
#   site_data_results = BD_interpol(s)
#   # add the results of this site to the container
#   new_data = new_data + site_data_results
# }
# # transform to dataframe
# new_df = data.frame(new_data)  # or whichever function you need to transform this


#### export ####

export_data01 <- soilcarbon5 %>% 
  dplyr::select(Source, Site_name, Site, Core, Habitat_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, BD_interpol_g_cm3)


export_data02 <- export_data01 %>% 
  relocate(Source, Site_name, Site, Core, Habitat_type, Latitude, Longitude, 
           accuracy_flag, accuracy_code, Country, Year_collected, .before = U_depth_m) %>% 
  arrange(Site, Habitat_type) %>% 
  mutate(DOI = "https://doi.org/10.17632/2nnv9bw3hh.2")


mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = export_data02, aes(x = Longitude, y = Latitude, 
                                      color = Site), alpha = 0.5)
mp


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data02

write.csv(export_df, export_file)



                       