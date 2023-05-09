##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)

# Clear workspace
rm(list = ls())

##Base data
connect = read_sf('data/connectivity', layer = 'MAR_reserves_2018Aug') %>% 
  st_set_crs(4326)

connect = connect %>% 
  mutate(tot_score = X48 + X25 + X11 + X34 + X9)

# plot(connect)
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
