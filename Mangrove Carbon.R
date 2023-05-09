##Mangrove Carbon
library(tidyverse)
library(raster)
library(leaflet)
library(sf)

r <- raster("data/toc_20160000023296-0000722176.tif")
plot(r)

ggplot(data = r) +
  