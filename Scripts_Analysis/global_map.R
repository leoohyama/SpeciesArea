library(patchwork)
library(tidyverse)

library(sf)
library(ggplot2)
library(maps)
library(maptools)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

setwd("~/Google Drive/Dissertation/Species_Area/SpeciesArea/")
df<-read.csv("s_area_trial.csv") # load data
df1<-read.csv("Modelling_data.csv") # load data

df$Ecoregion[df$Ecoregion=="Oceanic"]
biogeo_region <- st_read(
  "~/Downloads/official/wwf_terr_ecos.shp")


#biogeoregions
biogeo_region <- st_read(
  "Generalised_Biogeographic_Realms_2004/brpol_fin_dd.shp")
biogeo_region <- st_read(
  "~/Google Drive/Dissertation/Species_Area/SpeciesArea/Generalised_Biogeographic_Realms_2004/brpol_fin_dd.shp")
biogeo_region<-fortify(biogeo_region)
#world map
world<-ne_countries(returnclass = "sf")
#coastline borders
coastlines<-ne_coastline(returnclass = "sf")

###getting biogeo region only shapefile
biogeeographicv2<-st_intersection(world, biogeo_region)

#restructure data for map
unique(df[,c("Lat", "Long", "Study_ID")])
colnames(df1)
unique(df1[,c("Lat", "Lon", "Study.ID")])


df1$SR[df1$Study_ID == 27] <-81
biogeeographicv2$REALM
biogeeographicv2$REALM<-gsub("Oceanic", "Oceania", biogeeographicv2$REALM)
colnames(df1)
biogeeographicv3<-biogeeographicv2 %>%
  filter(!REALM == "Antarctic")
World_map<-ggplot() + 
  geom_sf(data = biogeeographicv3, aes(fill = REALM, color = REALM)) +
  guides(fill = guide_legend(override.aes = list(size=0.5))) +
  geom_sf(data = coastlines) +
  scale_fill_viridis_d(option = "D") +
  scale_colour_viridis_d(option = "D") +
  theme_minimal() +
  geom_point(data = df1, aes(x = Lon, y = Lat, 
                             size = Total.species.richness.on.islands 
                             ), fill = "orange" ,color = "white", pch=21, alpha =0.7, show.legend = F) +
  scale_size_continuous(range = c(1,14)) +
  labs(fill = "Biogeographic \nRealm", color = "Biogeographic \nRealm") +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "grey65"), legend.position = "top",
        plot.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size =14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) 
World_map

saveRDS(World_map, file = "worldmap.rds")
rm(World_map)
World_map<-readRDS("~/Google Drive/Dissertation/Species_Area/SpeciesArea/worldmap.rds")

ggsave("map1.tiff",plot = last_plot(), dpi =600, width = 25, height = 15, units = "cm")

##plot for proposal
World_map<-ggplot() + 
  geom_sf(data = biogeeographicv2, aes(fill = REALM, color = REALM)) +
  guides(fill = guide_legend(override.aes = list(size=0.5))) +
  geom_sf(data = coastlines) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  theme_classic() +
  geom_point(data = df1, aes(x = Long, y = Lat, size = SR), fill = "orange", color = "white", pch=21, alpha =0.7) +
  scale_size_continuous(range = c(1,14)) +
  labs(size = "No. of islands \nfrom study", fill = "Biogeographic \nRealm", color = "Biogeographic \nRealm") +
  theme_dark()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),  legend.position = "top",
    plot.background = element_rect(fill = "transparent",colour = NA),
    rect = element_rect(fill = "transparent"), # all rectangles
    legend.title = element_text(colour="white", size=12),
    legend.text = element_text(colour="white", size = 8),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.key.size = unit(1, "cm"),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10)
  )


##plot for esa talk
World_map<-ggplot() + 
  geom_sf(data = biogeeographicv2, aes(fill = REALM, color = REALM)) +
  guides(fill = guide_legend(override.aes = list(size=0.5))) +
  geom_sf(data = coastlines) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  theme_dark() +
  geom_point(data = df1, aes(x = Long, y = Lat, size = SR), fill = "orange", color = "white", pch=21, alpha =0.7) +
  scale_size_continuous(range = c(1,14)) +
  labs(x = "", y = "",size = "Species Richness", fill = "Biogeographic \nRealm", color = "Biogeographic \nRealm") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        rect = element_rect(fill = "transparent"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.size = unit(1, "cm"),
        legend.position = "top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.title = element_text(colour="white", size=10),
        legend.text = element_text(colour="white", size = 10)) 
ggsave(plot = World_map, file = "~/Desktop/graph2.png", 
       type = "cairo-png",  bg = "transparent",
       width = 24, height = 15, units = "cm", dpi = 800)


###quick summary stats

plot(df$Lat, df$Species.Richness)
ggplot(data =df) +
  geom_boxplot(aes(x = Ecoregion, y = Species.Richness)) +
  scale_y_log10()



#######After running script for mixed model
####until you get the object "slopes"
realms<-rownames(slopes)
unique(biogeeographicv2$REALM) #cross validate names of realms
realms<-c("Afrotropical", "Australasian", "Indo-Malay", "Nearctic", "Neotropical", "Oceanic", "Palearctic")
slopes<-as.tibble(slopes)
slopes$REALM<-realms

colnames(slopes)<-c("intercept", "slope", "REALM")
biogeeographic_slopes<-biogeeographicv2
unique(biogeeographic_slopes$REALM) #cross validate names of realms

biogeeographic_slopes<-left_join(biogeeographic_slopes, slopes, by = "REALM")


ggplot() + 
  geom_sf(data = biogeeographic_slopes, aes(fill = slope, color = slope)) +
  geom_sf(data = coastlines) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  theme_classic() +
  labs(size = "No. of Islands", fill = "Biogeographic \nRealm", color = "Biogeographic \nRealm") +
  theme(axis.title = element_blank())

ggplot() + 
  geom_sf(data = biogeeographic_slopes, aes(fill = slope, color = slope)) +
  geom_sf(data = coastlines) +
  scale_fill


