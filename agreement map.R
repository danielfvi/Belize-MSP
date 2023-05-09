##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)

# Clear workspace
rm(list = ls())

##Root results
root = read_sf('data/smart coasts/root_results', layer = '06_prot_corl_30000ha_all') %>% st_transform(4326)

##Biomass and catch estimates
bio <- read_csv("data/bio_catch_estimates.csv") %>% 
  dplyr::select(SDU_ID, bio, bio_kg_ha, bio_total, est_catch, est_catch_rs)

root = root %>%
  left_join(bio)

##Connectivity
connectivity <- read_csv("data/connectivity.csv")

root = root %>%
  left_join(connectivity) %>% 
  mutate(tot_score = if_else(is.na(tot_score), 0, tot_score))

##Dta
root_dta = root %>% 
  st_drop_geometry()

###Agreement areas considering connectivity
root_dta2 = root_dta %>% 
  mutate(connect_rs = tot_score/max(root_dta$tot_score),
         agreement1 = cv_rs + rec_rs + lob_rs + targ_rs,
         agreement2 = cv_rs + rec_rs + lob_rs + targ_rs + connect_rs,
         agreement3 = cv_rs + rec_rs - lob_rs + targ_rs + connect_rs) %>% 
  dplyr::select(SDU_ID, connect_rs, agreement1, agreement2, agreement3)

write.csv(root_dta2, "data/agreement2.csv", row.names = F)

