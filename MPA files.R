##MPA files
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)
library(fmsb)

st_layers(dsn="data/MPA shapefiles/Marine Protected Areas Database.gdb")

x1 = st_read(dsn="data/MPA shapefiles/Marine Protected Areas Database.gdb", layer = "Existing_HPFBZ") %>% st_transform(4326) 
x2 = st_read(dsn="data/MPA shapefiles/Marine Protected Areas Database.gdb", layer = "MarineProtectedAreas2022_Updated") %>% st_transform(4326) 

st_write(x1, "data/MPA shapefiles/exsiting_HPCZ.shp")
st_write(x2, "data/MPA shapefiles/all_BLZ_MPAs.shp")


MPAs <- read_sf('data/MPA shapefiles', layer = 'all_BLZ_MPAs')

leaflet() %>%
  addTiles() %>%
  addPolygons(data = MPAs)
