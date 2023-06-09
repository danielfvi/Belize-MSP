##Mangrove Carbon
library(tidyverse)
library(raster)
library(leaflet)
library(sf)

##Target fish biomass
r <- raster("data/mangrove/Total-Carbon-In-Mangroves-Belize.tiff")
plot(r)

x = data.frame(value = values(r)) %>% 
  filter(value>0)
dat_r = as.data.frame(r[data$value])
crs(r)
  
##Root results
root = read_sf('data/smart coasts/root_results', layer = '05_prot_mang_20000ha_all') %>% st_transform(26916)

root_dta = root %>% 
  st_drop_geometry()

root_dta_ID = root_dta %>% 
  mutate(ID = 1:nrow(root)) %>% 
  dplyr::select(ID, SDU_ID)

# leaflet() %>%
#   addTiles() %>% 
#   addRasterImage(data = r)

#Get central points for each unit
# root_point = st_centroid(root)

##Extract
dat = extract(r, root)


for(n in 14:602){
  print(n)
  x = data.frame(ID = n,
                 biomass = dat[[n]])
  if(n==14){
    dat_all = x
  }else(dat_all = rbind(dat_all, x))
}

bio = dat_all %>% 
  group_by(ID) %>% 
  summarise(carbon = sum(biomass, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(root_dta_ID) %>% 
  dplyr::select(SDU_ID, carbon)

root2 = root %>% 
  left_join(bio)

y = bio2 = bio %>% 
  mutate(carbon = if_else(carbon<1))
  
  

write.csv(bio, "data/carbon_estimates.csv", row.names = F)