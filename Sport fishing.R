##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)

# Clear workspace
rm(list = ls())

##Root results
bonefish = read_sf('data/sportfishing', layer = 'Bonefish') %>% st_transform(4326)
bonefish_stations = read_sf('data/sportfishing', layer = 'Bonefish_stations') %>% st_transform(4326)
permit = read_sf('data/sportfishing', layer = 'sportpermit') %>% st_transform(4326)
bonefish = read_sf('data/sportfishing', layer = 'Tarpon') %>% st_transform(4326)
bonefish_stations = read_sf('data/sportfishing', layer = 'Bonefish_stations') %>% st_transform(4326)


plot(sport_fish1)
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = connect)

##Root results
root = read_sf('data/smart coasts/root_results', layer = '06_prot_corl_30000ha_all') %>% st_transform(4326)

connect_inter = st_intersection(root, connect)

connect_dta = connect_inter %>% 
  st_drop_geometry() %>% 
  group_by(SDU_ID) %>% 
  summarise(tot_score = mean(tot_score))


write.csv(connect_dta, "data/connectivity.csv", row.names = F)
