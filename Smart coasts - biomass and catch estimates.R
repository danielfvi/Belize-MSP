##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)

##Target fish biomass
r <- raster("data/smart coasts/target_biomass/s0_clim0_targ.tif")
plot(r)

dat_r = as.data.frame(r[data])
crs(r)

leaflet() %>%
  addTiles() %>% 
  addRasterImage(data = r)

##Root results
root = read_sf('data/smart coasts/root_results', layer = '06_prot_corl_30000ha_all') %>% st_transform(26916)

#Get central points for each unit
root_point = st_centroid(root)

##Extract
dat = extract(r, root)


for(n in 1:602){
  print(n)
  x = data.frame(ID = n,
                 biomass = dat[[n]])
  if(n==1){
    dat_all = x
  }else(dat_all = rbind(dat_all, x))
}

bio = dat_all %>% 
  group_by(ID) %>% 
  summarise(bio = mean(biomass, na.rm = T))

write.csv(bio, "data/bio_estimates.csv", row.names = F)

bio_estimates <- read_csv("data/bio_estimates.csv")

bio_catch = root %>% 
  st_drop_geometry() %>%
  mutate(ID = 1:602) %>% 
  left_join(bio_estimates) %>% 
  mutate(bio_kg_ha = bio*100/1000) %>% 
  drop_na(bio)

x = as.numeric(quantile(bio_catch$bio_kg_ha, probs = 0.9))
Bmsy = x
K = 2*Bmsy
r = 0.23

bio_catch2 = bio_catch %>% 
  mutate(bio_total = prt_cr_*bio_kg_ha,
         f = r*(1-bio_kg_ha/K),
         est_catch = f*bio_total)

bio_catch3 = bio_catch2 %>% 
  mutate(est_catch_rs = est_catch/max(bio_catch2$est_catch))
         
write.csv(bio_catch3, "data/bio_catch_estimates.csv", row.names = F)


# y_df = data.frame(ID = 1:602,
#                   area_m2 = st_area(root)) %>% 
#   mutate(area_ha = area_m2/10000,
#          area_ha = as.double(area_ha))



dat2 = as.data.frame(dat[[1]])

dataFrame <- dat %>% 
  as_tibble()


root_dta = root %>% 
  st_drop_geometry()

leaflet() %>%
  addTiles() %>% 
  addPolygons(data = MPAs) %>% 
  addPolygons(data = replenishment)

###Root results


##Maps
# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    #legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = prt_crl), alpha = 0.5) +
  scale_fill_gradient(name="Priority areas",
                      #breaks = c(4:8),
                      #labels = c(4:8),
                      low="white", high="red") +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))

##Zoom into one MPA
#Turneffe Atoll (Maugre) (row 7)

mpa1 = MPAs[7,]

##Overlapping areas
root_mpa1 = st_intersection(mpa1, root)


##All MPAs in the area
pol2 = st_polygon(
  list(
    cbind(
      c(-87.6, -87.6, -88, -88, -87.6),
      c(17.1, 17.7, 17.7, 17.1, 17.1))
  )
)

mpas1_replenishment = st_intersection(mpa1, replenishment)

leaflet() %>%
  addTiles() %>% 
  addPolygons(data = mpas1_replenishment) 

ggplot() +
  geom_sf(data = MPAs[7,]) +
  geom_sf(data = root_mpa1, mapping = aes(fill = prt_crl), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                      #breaks = c(4:8),
                      #labels = c(4:8),
                      low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))


ggsave(g4, filename = "Figures/Biomass_map_tour.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g4, filename = "Figures/Biomass_map_tour.jpeg", 
       height = 6.2, 
       width = 10)

##Tradeoff
ggplot(data = root_mpa1) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = rec_rs), size = 5) +
  labs(x = "Catch potential", y = "Conservation benefits", color = "Tourism\nPotential") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))


