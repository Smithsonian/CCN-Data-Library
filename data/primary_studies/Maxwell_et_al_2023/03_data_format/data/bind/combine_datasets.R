##script top combine all datasets (from Andre and extracted from TM)
#18.08.22


#CAUTION: once this script runs, tidyverse doesn't work properly (due to plyr being loaded)
rm(list=ls()) # clear the workspace
library(plyr) #for data bind
library(tidyverse)

library(ggplot2)
library(sf) #to map
library(rnaturalearth) #privides map of countries of world
library(rnaturalearthdata) 

##### 1. Combine data ####

# setwd("~/07_Cam_postdoc/Data")
# andre_data = read.csv("Data from Andre.csv")

lst = list.files("reports/03_data_format/data/exported/", ".csv", full.names = TRUE)

data_compile1 = plyr::rbind.fill(lapply(lst, function(i){read.csv(i)})) ## tom.hengl@envirometrix.net

# 
# 
# data_compile1 = plyr::rbind.fill(data_compile0, andre_data)


data_compile2 <- data_compile1 %>% 
  mutate(Original_source = case_when(is.na(Original_source) == TRUE ~ Source,
                                     TRUE ~ Original_source)) %>% 
  mutate(Country = fct_recode(Country, "United States" = "USA"))

sum(is.na(data_compile2$Original_source))


data_compile2$Latitude <- as.numeric(data_compile2$Latitude)
data_compile2$Longitude <- as.numeric(data_compile2$Longitude)
data_compile2$OC_perc <- as.numeric(data_compile2$OC_perc)
data_compile2$BD_reported_g_cm3 <- as.numeric(data_compile2$BD_reported_g_cm3)
data_compile2$SOM_perc <- as.numeric(data_compile2$SOM_perc)



## note: to upload to GEE, need to replace NA values with blanks
#but this is NOT possible with numeric vectors

data_compile3 <- data_compile2 %>% 
#  filter(is.na(Latitude) == FALSE & is.na(Longitude) == FALSE) %>% 
  dplyr::rename(ID = X)

## 18.11 resolved issue of CCRCN location data 

#
data_unique <- data_compile2 %>% 
  distinct(Latitude,Longitude, .keep_all = TRUE)


# ## export data file
# 
# ## need to first change directory to working directory
setwd("~/07_Cam_postdoc/MarSOC-Dataset/reports/03_data_format/data/bind")
write.csv(data_compile3, "data_compile.csv", row.names =F)



##### 2. Figures ####

#### check locations ####


fig_title <- paste("Training dataset (unique location) as of", Sys.Date())

world <- ne_countries(scale = "medium", returnclass = "sf")

fig_n_points <- ggplot(data = world) +
  geom_sf() +
  coord_sf(ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  ggtitle(fig_title)+
  geom_point(data = data_unique, aes(x = Longitude, y = Latitude, 
                                       color = Source), size = 3, alpha = 0.4)+
  scale_size(range = c(2,8))+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

fig_n_points


# #### export figure
# path_out = 'reports/03_data_format/data/bind/point_maps/'
# 
# 
# fig_name <- paste(Sys.Date(),"n_points", sep = "_")
# export_file <- paste(path_out, fig_name, ".png", sep = '') 
# export_fig <- fig_n_points
# 
# ggsave(export_file, export_fig, width = 14.24, height = 8.46)



### figure for TNC (simple) 

fig_TNC <- ggplot(data = world) +
  geom_sf() +
  coord_sf(ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  labs(title = "Global tidal marsh soil carbon training dataset")+
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  geom_point(data = data_unique, aes(x = Longitude, y = Latitude, 
                                     color = Source), size = 3, alpha = 0.4)+
  scale_size(range = c(2,8))+
  theme(legend.position = "bottom")+
  theme(legend.position = "none")

fig_TNC

# #### export figure
# path_out = 'reports/03_data_format/data/bind/point_maps/'
# 
# 
# fig_name <- paste(Sys.Date(),"n_points", sep = "_")
# export_file <- paste(path_out, fig_name, ".png", sep = '') 
# export_fig <- fig_n_points
# 
# ggsave("2022_11_04_training-points_TNC.png", fig_TNC, width = 12.29, height = 7.12)
# 

        