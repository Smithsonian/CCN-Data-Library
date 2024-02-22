### useful scripts for data reformatting

library(tidyverse)
library(measurements) #to convert to decimal degrees
library(stringr) # extract first n values for date
library(janitor) # to clean names

#### reformat columns
rename_with(~ gsub("..", "_", .x, fixed = TRUE)) %>% #replacing .. in columns by _
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))   #replacing . in columns by _

###recode factor
  fct_recode(x, "an apple" = "apple", "a bear" = "bear")


#### deg min sec to dec deg ####

input_data_main02 <- input_data_main01 %>% 
  mutate(lat_detail = "28°01.626",
         long_detail = "12°16.558",
         lat = gsub("°", " ",
                    gsub("\\.", " ", lat_detail)),
         long = gsub("°", " ",
                     gsub("\\.", " ", long_detail)),
         lat_dec_deg = measurements::conv_unit(lat, from = "deg_min_sec", to = "dec_deg"), #N , Keep positive
         long_dec_deg = measurements::conv_unit(long, from = "deg_min_sec", to = "dec_deg"), #W , convert to neg
         Latitude = as.numeric(lat_dec_deg),
         Longitude = as.numeric(long_dec_deg)*-1,
         accuracy_flag = "direct from dataset",
         accuracy_code = "1",
         Site = "Lacustrine_lagoon_THIII")

#### reformat dates ####

mutate(Date = lubridate::dmy(Collection_Date)) %>% 
  mutate(Year_collected = lubridate::year(Date), #separate Year, Month, Day
         Month = lubridate::month(Date), 
         Day = lubridate::day(Date)) %>% 


#### separate depth code ####

separate(Depth_range___cm_, c("U_depth_cm", "L_depth_cm"), sep = '-') %>%  #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100,
         L_depth_m = as.numeric(L_depth_cm)/100)  
  

  
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
  
  

  
### fill### 
  fill(Sample_ID, .direction = "down") 
  
  
#### bind datasets #### 
input_data01 = plyr::rbind.fill(input_data_inner02, input_data_main02)


#### check location points ####


mapWorld <- borders("world", colour="gray50", fill="white")

mp <- ggplot() + 
  mapWorld +
  ylim(-60,80)+
  geom_point(data = input_data02, aes(x = Longitude, y = Latitude, 
                                      color = Site), alpha = 0.5)
mp

#https://r-spatial.org/r/2018/10/25/ggplot2-sf.html 
library(ggplot2)
library(sf) #to map
library(rnaturalearth) #privides map of countries of world
library(rnaturalearthdata) 




##### export data

export_data <- input_data4 %>% 
  select(Source, Site_name, Location, Core, Habitat_type, Latitude, Longitude, 
         accuracy_flag, accuracy_code, Country, Year_collected, Depth_to_bedrock_m, U_depth_m, L_depth_m, 
         Method, Conv_factor, OC_perc, SOM_perc, BD_reported_g_cm3)


## export

path_out = 'reports/03_data_format/data/exported/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data_marsh

write.csv(export_df, export_file)



##### interpolation ####

#### test to interpolate

carbon_test <- soilcarbon4 %>% 
  filter(Site == "Sloehaven old")


BD_test <- bulkdensity_df3 %>% 
  filter(Site == "Sloehaven old")


approx(x = BD_test$mid_depth_m, y = BD_test$BD_reported_g_cm3,
       xout = carbon_test$mid_depth_m, method = "linear")

funcA <- approxfun(x = BD_test$mid_depth_m, y = BD_test$BD_reported_g_cm3,
                   method = "linear", rule = 2)

carbon_test2<- carbon_test %>% 
  mutate(BD_estimated = funcA(mid_depth_m)) %>% 
  select(Site, U_depth_m, L_depth_m, mid_depth_m, OC_perc, BD_estimated)

# ##trying BD test 
# 
# BD_test2 <- bulkdensity_df3 %>% 
#   filter(Site == "Sloehaven old") %>% 
#   rename(mid_depth_BD = mid_depth_m,
#          L_depth_BD = L_depth_m, 
#          U_depth_BD = U_depth_m)
# 
# carbon_testBD <- left_join(carbon_test, BD_test)


BD_interpol <- function(df, site){
  df2 <- eval(as.name(df)) %>% 
    filter(Site == site)
  
  BD <- BD_final %>% 
    filter(Site == site)
  
  a <- approx(x = BD$mid_depth_m, y = BD$BD_reported_g_cm3, 
              xout = df2$mid_depth_m, method = "linear", rule = 2 )
  
  df2$depth_check <- a$x
  
  df2$BD_estimated <- a$y
  
  
  return(df2)
}

test <- BD_interpol(df = "soilcarbon4", site = "Sloehaven old")
test


dfs <- rep("soilcarbon4", times =12)

sites <- c("Sloehaven old",  "Sloehaven young", "Zuidgors old", "Zuidgors young", 
           "Hellegat old", "Hellegat young", "Kruispolder old", "Kruispolder young", 
           "Mariekerke old", "Mariekerke young",
           "Grembergen old", "Appels young" )


soilcarbon5 <- mapply(BD_interpol, df = dfs, site = sites)

soilcarbon5 <- soilcarbon4 %>% 
  group_by(Site)


df_list <- by(soilcarbon4, soilcarbon4[c("Site")], function(df, site){
  df2 <- eval(as.name(df)) %>% 
    filter(Site == site)
  
  BD <- BD_final %>% 
    filter(Site == site)
  
  a <- approx(x = BD$mid_depth_m, y = BD$BD_reported_g_cm3, 
              xout = df2$mid_depth_m, method = "linear", rule = 2 )
  
  df2$depth_check <- a$x
  
  df2$BD_estimated <- a$y
  
  
  return(df2)
})


soilcarbon4$Site <- as.factor(soilcarbon4$Site)

BD_interpol2 <- function(site){
  df2 <- soilcarbon4  %>% 
    filter(Site == site)
  
  BD <- BD_final %>% 
    filter(Site == site)
  
  a <- approx(x = BD$mid_depth_m, y = BD$BD_reported_g_cm3, 
              xout = df2$mid_depth_m, method = "linear", rule = 2 )
  
  df2$depth_check <- a$x
  
  df2$BD_estimated <- a$y
  
  
  return(df2)
}

test2 <- BD_interpol2(site = "Sloehaven old")

soilcarbon4$A_new <- unlist(by(soilcarbon4, soilcarbon4$Site, BD_interpol2))

test3 <- lapply(unique(soilcarbon4$Site), BD_interpol2)

## THIS IS WHAT WORKS
test4 <- as.data.frame(do.call(rbind,lapply(unique(soilcarbon4$Site), BD_interpol2)))



##### from predict_site

### for the nndm function, you also need the area of prediction
#https://gis.stackexchange.com/questions/403977/sf-create-polygon-from-minimum-x-and-y-coordinates
# lon = c(0.6621003481445342, 0.9882569643554717) # min and max (corners)
# lat = c(51.67979880621543, 51.78951129583516) # min and max (corners)
# 
# pol = st_polygon(
#   list(
#     cbind(
#       lon[c(1,2,2,1,1)], 
#       lat[c(1,1,2,2,1)])
#   ))
# 
# site_polygon = st_sfc(pol, crs = 32611)

# 
# eq_label <- expression("OC =" ~~ (a_est ~ "±" ~ a_std)~"OM"^2 ~~ "+" ~~
#                          (b_est ~ "±" ~ b_std)~"OM" ~~ "-" ~~
#                          ((c_est*-1)~"±"~c_std))

# eq_label_tidy <- list(bquote(paste("OC = ","(", !!a_est_RE, "±", !!a_std_RE, ")OM^2 +",
#                                    "(", !!b_est_RE, "±", !!b_std_RE, ")OM +",
#                                    "(", !!c_est_RE, "±", !!c_std_RE, ")", sep = " ")))

