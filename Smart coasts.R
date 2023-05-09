##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)

# Clear workspace
rm(list = ls())

##Base data
MPAs <- read_sf('data/smart coasts/base', layer = 'mar_mpas_all') %>% filter(Country == "Belize") %>% st_transform(4326)
coastline = read_sf('data/smart coasts/base', layer = 'mar_coastline') %>% st_transform(4326)
replenishment = read_sf('data/smart coasts/base', layer = 'bz_fish_replenishment_areas') %>% st_transform(4326)
reef1 = read_sf('data/smart coasts/base', layer = 'Coral_baselinefootprint_degraded') %>% st_transform(4326)
reef2 = read_sf('data/smart coasts/base', layer = 'Coral_baselinefootprint_healthy') %>% st_transform(4326)

MPAs_dta = MPAs %>% 
  st_drop_geometry()

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

##Agreement2
agreement2 <- read_csv("data/agreement2.csv")

root = root %>%
  left_join(agreement2)

##Dta
root_dta = root %>% 
  st_drop_geometry()

# leaflet() %>%
#   addTiles() %>% 
#   addPolygons(data = MPAs) %>% 
#   addPolygons(data = replenishment)


##Maps
# Base theme
base_theme <- theme(legend.title = element_text(size = 10),
                    legend.text = element_text(size = 10),
                    plot.title = element_text(size = 13),
                    axis.text=element_blank(),
                    axis.title=element_blank(),
                    strip.text=element_blank(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    #legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

##Smart Coasts priority areas
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = prt_crl), alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                       breaks = c(50, 950),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme 

##Priority areas with connectivity
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = agreement2), alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas\n(connectivity)",
                       breaks = c(50, 950),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme 

#Reef biomass
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = log(targ))) +
  scale_fill_gradient2(name="Target species\nbiomass",
                       breaks = c(10, 15),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 13) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme 

#Predicted catch
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = log(est_catch))) +
  scale_fill_gradient2(name="Reef\ncatch",
                       breaks = c(4, 9),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 7) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme 

#Lobster catch
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = log(lob))) +
  scale_fill_gradient2(name="Lobster\ncatch",
                       breaks = c(2, 8),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 5) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme

#Tourism
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = log(rec))) +
  scale_fill_gradient2(name="Tourism",
                       breaks = c(-5, 5),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme

#Connectivity
ggplot() +
  geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = tot_score)) +
  scale_fill_gradient2(name="Connectivity",
                       breaks = c(0.2, 3.5),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.8) +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme

###Tradeoff
##Tradeoff
ggplot(data = root) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = log(rec)), size = 5) +
  scale_color_gradient2(name="Tourism",
                     breaks = c(-5, 5),
                     labels = c("Low", "High"),
                     low="blue", mid = "white", high="red", midpoint = 0) +
  labs(x = "Lobster fisheries", y = "Reef biomass", color = "Tourism") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

root = root %>% 
  mutate(priority = if_else(targ_rs>0.5&lob_rs<0.3, "High", "Low"))


ggplot(data = root) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = priority), size = 5) +
  # scale_color_gradient2(name="Tourism",
  #                       breaks = c(-5, 5),
  #                       labels = c("Low", "High"),
  #                       low="blue", mid = "white", high="red", midpoint = 0) +
  labs(x = "Lobster fisheries", y = "Reef biomass", color = "Priority") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

ggplot() +
  geom_sf(data = coastline) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root, mapping = aes(fill = priority), alpha = 0.8) +
  geom_sf(data = MPAs, alpha = 0.4) +
  geom_sf(data = replenishment, alpha = 0.4, fill = "green") +
  # scale_fill_gradient(name="Priority areas",
  #                     #breaks = c(4:8),
  #                     #labels = c(4:8),
  #                     low="white", high="red") +
  coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
  #labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme

##################################Examples

################################Glovers Reef (row 16)################

mpa1 = MPAs[16,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mpa1)

##Overlapping areas
root_mpa1 = st_intersection(mpa1, root)


mpas1_replenishment = st_intersection(mpa1, replenishment)

# leaflet() %>%
#   addTiles() %>% 
#   addPolygons(data = mpas1_replenishment) 

##Priority areas
ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = prt_crl), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                       breaks = c(200, 800),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Southwater Caye") +
  theme_bw() + base_theme

##Tradeoff
ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = priority), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Southwater Caye") +
  theme_bw() + base_theme

##Reef biomass
p1 = ggplot() +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = root, mapping = aes(fill = log(targ)), alpha = 0.5) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.3) +
  geom_sf(data = mpa1, alpha = 0.3) +
  scale_fill_gradient2(name="Target species\nbiomass",
                       breaks = c(10, 15),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 13) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme

p1

##Lobster fisheries
p2 = ggplot() +
  geom_sf(data = root, mapping = aes(fill = log(lob)), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  geom_sf(data = mpa1, alpha = 0.3) +
  scale_fill_gradient2(name="Lobster\ncatch",
                       breaks = c(2, 8),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 5) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 

p2

##Tourism gains
p3 = ggplot() +
  geom_sf(data = root, mapping = aes(fill = log(rec)), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  geom_sf(data = mpa1, alpha = 0.3) +
  scale_fill_gradient2(name="Tourism",
                       breaks = c(-5, 5),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 
p3

##Reef catch
p4 = ggplot() +
  geom_sf(data = root, mapping = aes(fill = log(est_catch)), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  geom_sf(data = mpa1, alpha = 0.3) +
  scale_fill_gradient2(name="Reef\ncatch",
                       breaks = c(4, 9),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 7) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme
p4

##Connectivity
p5 = ggplot() +
  geom_sf(data = root, mapping = aes(fill = tot_score), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  geom_sf(data = mpa1, alpha = 0.3) +
  scale_fill_gradient2(name="Connectivity",
                       breaks = c(0.2, 3.5),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.8) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme
p5


p = ggarrange(p1, p2, p3, 
              ncol=3, 
              nrow=1)
p

# ggsave(g4, filename = "Figures/Biomass_map_tour.pdf", 
#        width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)
# 
# ggsave(g4, filename = "Figures/Biomass_map_tour.jpeg", 
#        height = 6.2, 
#        width = 10)

##Tradeoff
ggplot(data = root_mpa1) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = rec_rs), size = 5) +
  labs(x = "Lobster fisheries", y = "Target species biomass", color = "Tourism") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))


######################Turneffe Atoll (Maugre) (row 7)

mpa1 = MPAs[7,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mpa1)

##Overlapping areas
root_mpa1 = st_intersection(mpa1, root)


mpas1_replenishment = st_intersection(mpa1, replenishment)

# leaflet() %>%
#   addTiles() %>% 
#   addPolygons(data = mpas1_replenishment) 

##Priority areas
ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = prt_crl), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                      breaks = c(200, 800),
                      labels = c("Low", "High"),
                      low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme

##Target species biomass
p1 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = targ_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Target species\nbiomass",
                       breaks = c(0.1, 0.3),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.2) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme

p1

##Lobster catch
p2 = ggplot() +
  geom_sf(data = MPAs[7,]) +
  geom_sf(data = root_mpa1, mapping = aes(fill = lob_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Lobster\nfisheries",
                       breaks = c(0.1, 0.3),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.2) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 

p2

##Tourism gains
p3 = ggplot() +
  geom_sf(data = MPAs[7,]) +
  geom_sf(data = root_mpa1, mapping = aes(fill = rec_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Tourism",
                       breaks = c(0.01, 0.03),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.02) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 
p3

##Reef catch
p4 = ggplot() +
  geom_sf(data = MPAs[7,]) +
  geom_sf(data = root_mpa1, mapping = aes(fill = est_catch_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Reef\nfisheries",
                       breaks = c(0.1, 0.3),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.2) +
  coord_sf(y=c(17.15, 17.7), x = c(-88, -87.7)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme
p4

p = ggarrange(p1, p2, p3, 
              ncol=3, 
              nrow=1)
p

# ggsave(g4, filename = "Figures/Biomass_map_tour.pdf", 
#        width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)
# 
# ggsave(g4, filename = "Figures/Biomass_map_tour.jpeg", 
#        height = 6.2, 
#        width = 10)

##Tradeoff
ggplot(data = root_mpa1) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = rec_rs), size = 5) +
  labs(x = "Fisheries catch", y = "Conservation benefits", color = "Tourism\nPotential") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

ggplot(data = root) +
  geom_point(aes(x = lob_rs, y = targ_rs)) +
  labs(x = "Fisheries", y = "Conservation") +
  theme_bw() + 
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))


################################Southwater Caye (row 13)

mpa1 = MPAs[13,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mpa1)

##Overlapping areas
root_mpa1 = st_intersection(mpa1, root)


mpas1_replenishment = st_intersection(mpa1, replenishment)

# leaflet() %>%
#   addTiles() %>% 
#   addPolygons(data = mpas1_replenishment) 

##Priority areas
ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = prt_crl), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                       breaks = c(200, 800),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(16.55, 16.91), x = c(-88.25, -88.04)) +
  #labs(title = "Southwater Caye") +
  theme_bw() + base_theme

##Predicted catch
p1 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = targ_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.3) +
  scale_fill_gradient2(name="Target species\nbiomass",
                       breaks = c(0.15, 0.5),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.3) +
  coord_sf(y=c(16.55, 16.91), x = c(-88.25, -88.04)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme

p1
##Conservation gains
p2 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = lob_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Lobster\nfisheries",
                       breaks = c(0.03, 0.09),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.02) +
  coord_sf(y=c(16.55, 16.91), x = c(-88.25, -88.04)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 

p2

##Tourism gains
p3 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = rec_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Tourism",
                       breaks = c(0.005, 0.02),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.01) +
  coord_sf(y=c(16.55, 16.91), x = c(-88.25, -88.04)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 
p3

##Reef catch
p4 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = est_catch_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Reef\nfisheries",
                       breaks = c(0.2, 0.55),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.4) +
  coord_sf(y=c(16.55, 16.91), x = c(-88.25, -88.04)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme
p4

p = ggarrange(p1, p2, p3, 
              ncol=3, 
              nrow=1)
p

# ggsave(g4, filename = "Figures/Biomass_map_tour.pdf", 
#        width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)
# 
# ggsave(g4, filename = "Figures/Biomass_map_tour.jpeg", 
#        height = 6.2, 
#        width = 10)

##Tradeoff
ggplot(data = root_mpa1) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = rec_rs), size = 5) +
  labs(x = "Fisheries catch", y = "Conservation benefits", color = "Tourism\nPotential") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

################################Glovers Reef (row 16)################

mpa1 = MPAs[16,]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mpa1)

##Overlapping areas
root_mpa1 = st_intersection(mpa1, root)


mpas1_replenishment = st_intersection(mpa1, replenishment)

# leaflet() %>%
#   addTiles() %>% 
#   addPolygons(data = mpas1_replenishment) 

##Priority areas
ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = prt_crl), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Priority areas",
                       breaks = c(200, 800),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 500) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Southwater Caye") +
  theme_bw() + base_theme

##Reef biomass
p1 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = root_mpa1, mapping = aes(fill = targ_rs), alpha = 0.5) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.3) +
  scale_fill_gradient2(name="Target species\nbiomass",
                       breaks = c(0.2, 0.6),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.4) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  #labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme

p1

##Lobster fisheries
p2 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = lob_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Lobster\nfisheries",
                       breaks = c(0.03, 0.09),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.02) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 

p2

##Tourism gains
p3 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = rec_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Tourism",
                       breaks = c(0.005, 0.02),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.01) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme 
p3

##Reef catch
p4 = ggplot() +
  geom_sf(data = mpa1) +
  geom_sf(data = root_mpa1, mapping = aes(fill = est_catch_rs), alpha = 0.5) +
  geom_sf(data = reef2, alpha = 0.5) +
  geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = coastline) +
  geom_sf(data = mpas1_replenishment, fill = "green", alpha = 0.5) +
  scale_fill_gradient2(name="Reef\nfisheries",
                       breaks = c(0.2, 0.55),
                       labels = c("Low", "High"),
                       low="blue", mid = "white", high="red", midpoint = 0.4) +
  coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
  labs(title = "Turneffe Atoll") +
  theme_bw() + base_theme
p4

p = ggarrange(p1, p2, p3, 
              ncol=3, 
              nrow=1)
p

# ggsave(g4, filename = "Figures/Biomass_map_tour.pdf", 
#        width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)
# 
# ggsave(g4, filename = "Figures/Biomass_map_tour.jpeg", 
#        height = 6.2, 
#        width = 10)

##Tradeoff
ggplot(data = root_mpa1) +
  geom_point(aes(x = lob_rs, y = targ_rs, color = rec_rs), size = 5) +
  labs(x = "Fisheries catch", y = "Conservation benefits", color = "Tourism\nPotential") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))
