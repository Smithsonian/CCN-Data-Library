# Rough Draft Map 
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(sf)
library(rgdal)
library(gridExtra)
library(tmaptools)

v2 <- read_csv("data/CCRCN_V2/cores.csv")
v2 <- read_csv("data/CCRCN_V2/cores.csv", guess=nrow(v2))

core_geography <- read_csv("data/CCRCN_V2/core_geography.csv") 

oregon_cores <- core_geography %>% 
  filter(state == "Oregon")

v2_long <- v2 %>% 
  gather(key = "qual_code", value = "sub_qual_code", -c(study_id:habitat_assignment_method, max_depth)) %>% 
  filter(complete.cases(sub_qual_code),
         core_id %in% oregon_cores$core_id) %>% 
  mutate(qual_code = str_remove(qual_code, "_qual_code"))

v2_long$qual_code <- factor(v2_long$qual_code, levels = c("stocks", "dates", "elevation"))

ggplot(data = v2_long, aes(x=longitude, y=latitude)) +
  geom_point(aes(color = sub_qual_code))+
  facet_grid(habitat~qual_code)

map.sf <- ne_countries(scale = 'medium', type = 'map_units',
                       returnclass = 'sf')

map.na.sf <- map.sf[map.sf$continent == 'North America',]

map.na.sf.aea <- st_transform(map.na.sf, 
                              crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# Load spatial data we will use
my_sp <- sp::SpatialPointsDataFrame(coords = data.frame(v2_long$longitude, 
                                                        v2_long$latitude), 
                                    data = data.frame(v2_long),
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

my_sf <- sf::st_as_sf(my_sp)

my_sf_aea <- st_transform(my_sf, crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
b <- st_bbox(my_sf_aea)


oregon_map <- ggplot(data = my_sf_aea) + 
  geom_sf(data=map.na.sf.aea, color="black", size=0.1, fill="grey") +
  geom_sf(aes(color = sub_qual_code)) +
  coord_sf(xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]),
           crs="+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0") +
  # Probably want to take off x and y axes text
  theme_minimal() +
  facet_grid(habitat~qual_code) +
  ggtitle("Oregon") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())
  
ggsave("temp/oregon_cores.jpg", height = 8, width = 4.5)
