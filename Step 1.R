##Load packages
library(tidyverse) # for general data wrangling and plotting
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(scales)
library(gdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

##lOAD Reef polygons
reefs <- read_sf('data/reef shapefiles', layer = 'WCMC008_CoralReef2018_Py_v4')
reefs$ID = 1:nrow(reefs)

v.false <- read_csv("~/MPA_Nutrition/data/v_false.csv")
v.false = as.vector(v.false$ID)

reefs = reefs %>% filter(!ID %in% v.false)

#List of reef countries
reef_countries = read_csv("~/MPA_Nutrition/data/reef_countries.csv")

##Load MPA polygons - MPA_Altlas
MPAs <- read_sf('data/MPA shapefiles', layer = 'mpatlas_20200918_clean') 

##Transform units to match other shapefiles
MPAs = MPAs %>% st_transform(4326)
MPAs$ID = 1:nrow(MPAs)

v.false = read_csv("~/MPA_Nutrition/data/vFalse_MPA.csv")

v.false = as.vector(v.false$ID)

MPAs = MPAs %>% filter(!ID %in% v.false)

##Step 1 - Seperate mesoamerican reefs only
reef_dta = reefs %>% 
  st_drop_geometry()

pol = st_polygon(
  list(
    cbind(
      c(-89, -89, -85.15, -85.15, -89),
      c(15.7, 21.6, 21.6, 15.7, 15.7))
  )
)

pol2 = st_polygon(
  list(
    cbind(
      c(-89, -89, -91, -91, -89),
      c(22, 23.6, 23.6, 22, 22))
  )
)

reefs_meso = reefs %>% 
  filter(ISO3 %in% c("BLZ", "MEX", "HND", "GTM")) %>% 
  st_crop(pol, .predicate = st_intersects)

reefs_belize = reefs %>% 
  filter(ISO3 %in% c("BLZ"))

st_write(reefs_meso, "reefs_meso.shp")
##Step 2 - sperate mesoamerican MPAs only
MPA_dta = MPAs %>% 
  st_drop_geometry()

MPAs_meso = MPAs %>% 
  filter(country %in% c("BLZ", "MEX", "HND", "GTM")) %>% 
  st_filter(pol, .predicate = st_intersects)

MPAs_belize = MPAs %>% 
  filter(country %in% c("BLZ"),
         !mpa_id == 68808425)

MPAs_BEL_dta = MPAs_belize %>% 
  st_drop_geometry()

st_write(MPAs_meso, "MPAs_meso.shp")

##Visualize data
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addPolygons(data = pol) %>% 
  addPolygons(data = reefs_belize, color = "geen") %>% 
  addPolygons(data = MPAs_belize)

##Step 3 - sperate reef survey data from mesoamerican reef

##Visualize data
pred = read_csv("~/MPA_Nutrition/Outputs/reef_MPA_predictions_bayes.csv")

pred_dta = pred %>% 
  dplyr::select(pred.oa, pred.restricted, all_catch, reef.id)

reefs_belize_pred = reefs_belize %>% 
  left_join(pred, by = c("ID" = "reef.id")) 

reefs_belize_pred$pred_nt = 0
reefs_belize_pred$tour = 0

for(n in 1:nrow(reefs_belize_pred)){
  reefs_belize_pred$pred_nt[n] = reefs_belize_pred$pred.oa[n] + runif(1, min=1, max=4)
  reefs_belize_pred$tour[n] = runif(1, min=10, max=100)
  }
reefs_belize_pred = reefs_belize_pred %>% 
  mutate(Biomass_diff = pred_nt - pred.oa)

#Produce maps
library(tidyverse) 
library(gdata)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(ggrepel)
library(sf)

world <- rnaturalearth::ne_countries("small", returnclass = "sf")

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

g = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=pred.oa, color = pred.oa)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8),
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # # Theme
  guides(color = "none")+
  labs(fill = "Biomass (non-MPA)") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))

ggsave(g, filename = "Figures/Biomass_map_OA.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Biomass_map_OA.jpeg", 
       height = 6.2, 
       width = 10)

g1 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=pred_nt, color = pred_nt)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "Biomass (MPA)") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))

ggsave(g1, filename = "Figures/Biomass_map_NT.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g1, filename = "Figures/Biomass_map_NT.jpeg", 
       height = 6.2, 
       width = 10)

g3 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=Biomass_diff, color = Biomass_diff)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "Biomass gain (MPA - non-MPA)") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))

ggsave(g3, filename = "Figures/Biomass_map_cons.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g3, filename = "Figures/Biomass_map_cons.jpeg", 
       height = 6.2, 
       width = 10)

g2 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=OA_all_catch, color = OA_all_catch)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "Catch potential") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))

ggsave(g2, filename = "Figures/Biomass_map_catch.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g2, filename = "Figures/Biomass_map_catch.jpeg", 
       height = 6.2, 
       width = 10)

g4 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=tour, color = tour)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "Tourism potential") +
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
belize_dta = reefs_belize_pred %>% 
  st_drop_geometry()

ggplot(data = belize_dta) +
  geom_point(aes(x = log(MU_catch), y = Biomass_diff, color = tour), size = 5) +
  labs(x = "Catch potential", y = "Conservation benefits", color = "Tourism\nPotential") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

ggplot(data = belize_dta) +
  plot_ly(x=log(MU_catch), y=Biomass_diff, z=log(tour), type="scatter3d", mode="markers")
  

geom_point(aes(x = log(MU_catch), y = Biomass_diff), size = 5) +
  labs(x = "Catch potential", y = "Conservation benefits") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23))

##MPA coverage 

reefs_belize_pred$ID = as.factor(reefs_belize_pred$ID)

g5 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred, mapping=aes(fill=ID, color = ID)) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "ID") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))
g5

g6 = ggplot() +
  geom_sf(data = world %>% filter(gu_a3 %in% c("BLZ"))) +
  # Plot small places
  geom_sf(data=reefs_belize_pred) +
  ##Add MPAs
  geom_sf(data=MPAs_belize, mapping=aes(fill=no_take, color = no_take), alpha = 0.5) +
  # Legend and labels
  # scale_fill_gradient(name="Biomass",
  #                     breaks = c(4:8),
  #                     labels = c(4:8)
  #                     low="navy", high="darkred") +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  guides(color = "none")+
  labs(fill = "MPAs") +
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13))
g6
