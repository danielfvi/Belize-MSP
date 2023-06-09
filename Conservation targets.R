##Smart coasts
library(tidyverse)
library(raster)
library(leaflet)
library(sf)
library(ggpubr)
library(fmsb)

# Clear workspace
rm(list = ls())

##Base data
MPAs <- read_sf('data/smart coasts/base', layer = 'mar_mpas_all') %>% filter(Country == "Belize") %>% st_transform(4326)
# coastline = read_sf('data/smart coasts/base', layer = 'mar_coastline') %>% st_transform(4326)
replenishment = read_sf('data/smart coasts/base', layer = 'bz_fish_replenishment_areas') %>% st_transform(4326)
# reef1 = read_sf('data/smart coasts/base', layer = 'Coral_baselinefootprint_degraded') %>% st_transform(4326)
# reef2 = read_sf('data/smart coasts/base', layer = 'Coral_baselinefootprint_healthy') %>% st_transform(4326)
# reef3 = read_sf('data/reef shapefiles', layer = 'reef') %>% st_transform(4326)

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

##Agreement areas considering connectivity
#agreement1 = cv_rs + rec_rs + lob_rs + targ_rs
#agreement2 = cv_rs + rec_rs + lob_rs + targ_rs + connect_rs
#agreement3 = cv_rs + rec_rs - lob_rs + targ_rs + connect_rs

agreement2 <- read_csv("data/agreement2.csv")

root = root %>%
  left_join(agreement2)

##Dta
root_dta = root %>% 
  st_drop_geometry()

##Calculate lobster fisheries benefits
root_dta2 = root_dta %>% 
  mutate(total = sum(root_dta$lob),
         fish = total-lob)

lob_benefit = root_dta2 %>% 
  mutate(fis_rs = (fish-min(root_dta2$fish))/(max(root_dta2$fish) - min(root_dta2$fish))) %>% 
  dplyr::select(SDU_ID, total, fish, fis_rs)

root = root %>% 
  left_join(lob_benefit)

##Dta
root_dta = root %>% 
  st_drop_geometry()

##Subset for management units within existing MPAs
root_MPAs_inter = as.data.frame(st_intersects(root, MPAs))

##ID for intersection
root_dta_ID = root_dta %>% 
  mutate(row.id = 1:nrow(root)) %>% 
  dplyr::select(row.id, SDU_ID)

root_MPAs = root_dta_ID %>% 
  left_join(root_MPAs_inter) %>% 
  mutate(col.id = replace_na(col.id, 0),
         col.id = if_else(col.id>0, 1, col.id)) %>% 
  rename(is_mpa = col.id) %>% 
  dplyr::select(SDU_ID, is_mpa)

root2 = root %>% 
  left_join(root_MPAs) %>% 
  filter(is_mpa == 1)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = MPAs) %>%
#   addPolygons(data = root)

##################Tradeoff
tradeoff_priority = root2 %>% 
  st_drop_geometry() %>% 
  mutate(priority_lob_targ1 = if_else(targ_rs>0.4&fis_rs>0.7, 1, 0),
         priority_lob_targ2 = if_else(targ_rs>0.3&fis_rs>0.7, 1, 0),
         priority_lob_targ3 = if_else(targ_rs>0.5&fis_rs>0.7, 1, 0)) %>%
  dplyr::select(SDU_ID, targ_rs, fis_rs, priority_lob_targ1:priority_lob_targ3) %>%
  reshape2::melt(id.vars = c("SDU_ID", "targ_rs", "fis_rs"))

priority_summary = tradeoff_priority %>% 
  group_by(variable) %>% 
  summarise(perc_target = round(100*sum(value)/nrow(root_dta), 1))

ggplot() +
  geom_point(data = root2, aes(y = targ_rs, x = fis_rs), size = 5) +
  geom_point(data = tradeoff_priority %>% filter(value == 1, variable == "priority_lob_targ2"), aes(y = targ_rs, x = fis_rs), size = 5, color = "red") +
  geom_point(data = tradeoff_priority %>% filter(value == 1, variable == "priority_lob_targ1"), aes(y = targ_rs, x = fis_rs), size = 5, color = "blue") +
  geom_point(data = tradeoff_priority %>% filter(value == 1, variable == "priority_lob_targ3"), aes(y = targ_rs, x = fis_rs), size = 5, color = "green") +
  geom_hline(yintercept = 0.3, size = 3, color = "red")+
  geom_hline(yintercept = 0.4, size = 3, color = "blue")+
  geom_hline(yintercept = 0.5, size = 3, color = "green")+
  annotate("text", x = 0.34, y = 0.32, label = "Threshold 1", col = "red", size = 5)+
  annotate("text", x = 0.34, y = 0.42, label = "Threshold 2", col = "blue", size = 5)+
  annotate("text", x = 0.34, y = 0.52, label = "Threshold 3", col = "green", size = 5)+
  # scale_color_gradient2(name="Tourism",
  #                       breaks = c(-5, 5),
  #                       labels = c("Low", "High"),
  #                       low="blue", mid = "white", high="red", midpoint = 0) +
  labs(x = "Lobster fisheries", y = "Reef biomass", color = "Priority", title = "Conservation/Fisheries tradeoff") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23),
        plot.title = element_text(size = 23))

##Smart Coasts
root_priority = root2 %>% 
  st_drop_geometry() %>% 
  mutate(priority3 = if_else(prt_crl>990, 1, 0),
         priority2 = if_else(prt_crl>750, 1, 0),
         priority1 = if_else(prt_crl>500, 1, 0)) %>%
  dplyr::select(SDU_ID, targ_rs, fis_rs, priority1:priority3) %>%
  reshape2::melt(id.vars = c("SDU_ID", "targ_rs", "fis_rs"))

priority_summary_root = root_priority %>% 
  group_by(variable) %>% 
  summarise(perc_target = round(100*sum(value)/nrow(root_dta), 1))

ggplot(data = root2) +
  geom_histogram(aes(x = prt_crl), bins = 100) +
  geom_vline(xintercept = 990, size = 1.5, color = "red")+
  geom_vline(xintercept = 750, size = 1.5, color = "blue")+
  geom_vline(xintercept = 500, size = 1.5, color = "green")+
  annotate("text", x = 890, y = 125, label = "Threshold 3", col = "red", size = 5)+
  annotate("text", x = 650, y = 125, label = "Threshold 2", col = "blue", size = 5)+
  annotate("text", x = 400, y = 125, label = "Threshold 1", col = "green", size = 5)+
  labs(y = "Frequency", x = "Priority score", title = "Smart Coasts Project") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23),
        plot.title = element_text(size = 23))
  

plot(root2$prt_crl)

root_priority = root2 %>%
  mutate(priority1 = if_else(prt_crl>900))
  

  
hist(root_dta$prt_crl)


  summarise(priority_lob_targ1 = round(100*sum(priority_lob_targ1)/nrow(root_dta)),
         priority_lob_targ2 = 100*sum(priority_lob_targ2)/nrow(root_dta),
         priority_lob_targ3 = 100*sum(priority_lob_targ3)/nrow(root_dta),
         priority_rec_targ1 = 100*sum(priority_rec_targ1)/nrow(root_dta),
         priority_rec_targ2 = 100*sum(priority_rec_targ2)/nrow(root_dta),
         priority_rec_targ3 = 100*sum(priority_rec_targ3)/nrow(root_dta),
         priority_lob_rec1 = 100*sum(priority_lob_rec1)/nrow(root_dta),
         priority_lob_rec2 = 100*sum(priority_lob_rec2)/nrow(root_dta),
         priority_lob_rec3 = 100*sum(priority_lob_rec3)/nrow(root_dta))


##Tradeoff
ggplot(data = root) +
  geom_point(aes(x = log(rec_rs), y = fis_rs), size = 5) +
  # scale_color_gradient2(name="Tourism",
  #                       breaks = c(-5, 5),
  #                       labels = c("Low", "High"),
  #                       low="blue", mid = "white", high="red", midpoint = 0) +
  #labs(x = "Lobster fisheries", y = "Reef biomass", color = "Priority") +
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


#################Proposals

# pol2 = st_polygon(
#   list(
#     cbind(
#       c(-87.7, -87.7, -87.8, -87.8, -87.7),
#       c(16.78, 16.98, 16.98, 16.78, 16.78))
#   )
# )
# #coord_sf(y=c(16.7, 16.95), x = c(-87.92, -87.65)) +
# pal <- colorQuantile("Reds", NULL, n = 5)
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = root, fillColor= ~pal(prt_crl), fillOpacity = 0.4, 
#               weight = 2, 
#               color = "white") %>%
#   addPolygons(data = replenishment) %>% 
#   addPolygons(data = pol2, color = "red")

##ID for intersection
root_dta_ID = root_dta %>% 
  mutate(row.id = 1:nrow(root)) %>% 
  dplyr::select(row.id, SDU_ID)

##Total lobster catch
lob_catch = root_dta$total[1] 

##Stakeholder 1
sk1 = st_polygon(
  list(
    cbind(
      c(-88.1, -88.1, -88.2, -88.2, -88.1),
      c(16.3, 16.5, 16.5, 16.3, 16.3))
  )
)


sk1_inter = as.data.frame(st_intersects(root, sk1))

sk1_dta = root_dta_ID %>% 
  left_join(sk1_inter) %>% 
  mutate(col.id = replace_na(col.id, 0)) %>% 
  rename(is_selected = col.id) %>% 
  dplyr::select(SDU_ID, is_selected)

sk1_outcomes = root_dta %>% 
  left_join(sk1_dta) %>% 
  mutate(score = is_selected*prt_crl,
         cv_selec = is_selected*cv,
         lob_selec = is_selected*lob,
         rec_selec = is_selected*rec,
         targ_selec = is_selected*targ,
         connect_selec = is_selected*tot_score) %>%
  summarise(score = mean(score),
            cv = sum(cv_selec),
            lob = lob_catch - sum(lob_selec),
            rec = sum(rec_selec),
            targ = sum(targ_selec),
            connect = mean(connect_selec))



##Stakeholder 2
sk2 = st_polygon(
  list(
    cbind(
      c(-88, -88, -88.1, -88.1, -88),
      c(17.1, 17.3, 17.3, 17.1, 17.1))
  )
)

sk2_inter = as.data.frame(st_intersects(root, sk2))

sk2_dta = root_dta_ID %>% 
  left_join(sk2_inter) %>% 
  mutate(col.id = replace_na(col.id, 0)) %>% 
  rename(is_selected = col.id) %>% 
  dplyr::select(SDU_ID, is_selected)

sk2_outcomes = root_dta %>% 
  left_join(sk2_dta) %>% 
  mutate(score = is_selected*prt_crl,
         cv_selec = is_selected*cv,
         lob_selec = is_selected*lob,
         rec_selec = is_selected*rec,
         targ_selec = is_selected*targ,
         connect_selec = is_selected*tot_score) %>%
  summarise(score = mean(score),
            cv = sum(cv_selec),
            lob = lob_catch - sum(lob_selec),
            rec = sum(rec_selec),
            targ = sum(targ_selec),
            connect = mean(connect_selec))


##Stakeholder 3
sk3 = st_polygon(
  list(
    cbind(
      c(-87.7, -87.7, -87.8, -87.8, -87.7),
      c(16.78, 16.98, 16.98, 16.78, 16.78))
  )
)

sk3_inter = as.data.frame(st_intersects(root, sk3))

sk3_dta = root_dta_ID %>% 
  left_join(sk3_inter) %>% 
  mutate(col.id = replace_na(col.id, 0)) %>% 
  rename(is_selected = col.id) %>% 
  dplyr::select(SDU_ID, is_selected)

sk3_outcomes = root_dta %>% 
  left_join(sk3_dta) %>% 
  mutate(score = is_selected*prt_crl,
         cv_selec = is_selected*cv,
         lob_selec = is_selected*lob,
         rec_selec = is_selected*rec,
         targ_selec = is_selected*targ,
         connect_selec = is_selected*tot_score) %>%
  summarise(score = mean(score),
            cv = sum(cv_selec),
            lob = lob_catch - sum(lob_selec),
            rec = sum(rec_selec),
            targ = sum(targ_selec),
            connect = mean(connect_selec))


##Plot comparison accross proposals
proposals = rbind(sk1_outcomes, sk2_outcomes, sk3_outcomes)

rownames(proposals) <- paste("Propposal" , 1:3 , sep=" ")

plot_dta = rbind(data.frame(score = c(max(proposals$score), min(proposals$score)),
                            cv = c(max(proposals$cv), min(proposals$cv)),
                            lob = c(max(proposals$lob), min(proposals$lob)),
                            rec = c(max(proposals$rec), min(proposals$rec)),
                            targ = c(max(proposals$targ), min(proposals$targ)),
                            connect = c(max(proposals$connect), min(proposals$connect))),
                 proposals) %>% 
  rename("Smart Coasts\nPriority" = score,
         "Coastal\nProtection" = cv,
         "Tourism" = rec,
         "Reef fish\nbiomass" = targ,
         "Connectivity" = connect,
         "Lobster fisheries" = lob) %>% 
  dplyr::select(-"Smart Coasts\nPriority")

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


radarchart(plot_dta, axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", 
           cglty=1, 
           axislabcol="black", caxislabels=c("", "", "", "", ""),
           cglwd=0.8,
            #custom labels
            vlcex=1 
)

# Add a legend
legend(x=0.7, y=1.1, legend = rownames(plot_dta[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)


######################Trade off analysis of proposals
proposals2 = rbind(sk1_outcomes, sk2_outcomes, sk3_outcomes) %>% 
  mutate(lob_rs = lob/sum(root_dta$lob),
         bio_rs = targ/sum(root_dta$targ),
         rec_rs = rec/sum(root_dta$rec),
         proposal = c("Proposal 1", "Proposal 2", "Proposal 3"))

##Biomass vs fisheries
ggplot(data = proposals2) +
  geom_point(aes(x = lob_rs, y = bio_rs, color = proposal), size = 5) +
  labs(x = "Lobster fisheries", y = "Reef fish biomass", color = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23),
        legend.text = element_text(size = 15))

##Biomass vs tourism
ggplot(data = proposals2) +
  geom_point(aes(x = rec_rs, y = bio_rs, color = proposal), size = 5) +
  labs(x = "Tourism", y = "Reef fish biomass", color = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 23),
        legend.text = element_text(size = 15))

#################Maps
#Proposal 1
sk1_map = st_sfc(sk1, crs = 4326)
#Map
ggplot() +
  #geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root) +
  geom_sf(data = sk1_map, fill = "red", alpha = 0.5) +
  coord_sf(y=c(15.8, 18.4), x = c(-89, -87.3)) +
  labs(title = "Proposal 1") +
  theme_bw() + base_theme +
  theme(plot.title = element_text(size = 20))


#Proposal 2
sk2_map = st_sfc(sk2, crs = 4326)

#Map
ggplot() +
  #geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root) +
  geom_sf(data = sk2_map, fill = "red", alpha = 0.5) +
  coord_sf(y=c(15.8, 18.4), x = c(-89, -87.3)) +
  labs(title = "Proposal 2") +
  theme_bw() + base_theme +
  theme(plot.title = element_text(size = 20))

#Proposal 3
sk3_map = st_sfc(sk3, crs = 4326)

#Map
ggplot() +
  #geom_sf(data = MPAs) +
  geom_sf(data = coastline) +
  geom_sf(data = replenishment) +
  #geom_sf(data = reef2, alpha = 0.5) +
  #geom_sf(data = reef1, alpha = 0.5) +
  geom_sf(data = root) +
  geom_sf(data = sk3_map, fill = "red", alpha = 0.5) +
  coord_sf(y=c(15.8, 18.4), x = c(-89, -87.3)) +
  labs(title = "Proposal 3") +
  theme_bw() + base_theme +
  theme(plot.title = element_text(size = 20))

# plot_dta = rbind(data.frame(score = c(max(root_dta$prt_crl), min(root_dta$prt_crl)),
#                             cv = c(sum(root_dta$cv), min(root_dta$cv)),
#                             #lob = c(lob_catch, min(root_dta$lob)),
#                             rec = c(sum(root_dta$rec), min(root_dta$rec)),
#                             targ = c(sum(root_dta$targ), min(root_dta$targ)),
#                             connect = c(max(root_dta$tot_score), min(root_dta$tot_score))),
#                  sk1_outcomes)


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


