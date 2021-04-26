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
library(sp)

setwd("~/Google Drive/Dissertation/Species_Area/SpeciesArea/")
df<-read.csv("s_area_trial.csv") # load data
df1<-read.csv("Modelling_data.csv") # load data

df$Ecoregion[df$Ecoregion=="Oceanic"]


#biogeoregions shapefile
biogeo_region <- st_read(
  "~/Google Drive/Dissertation/Species_Area/SpeciesArea/Generalised_Biogeographic_Realms_2004/brpol_fin_dd.shp")
biogeo_region<-fortify(biogeo_region)
#world map
world<-ne_countries(returnclass = "sf")
#coastline borders
coastlines<-ne_coastline(returnclass = "sf")

###getting biogeo region only shapefile, intersection between realms and world map
biogeeographicv2<-st_intersection(world, biogeo_region)

#restructure data for map
unique(df[,c("Lat", "Long", "Study_ID")])

#Fix Species richness value
df$SR[df$Study_ID == 27] <-81
#Replace Oceanic with Oceania
biogeeographicv2$REALM<-gsub("Oceanic", "Oceania", biogeeographicv2$REALM)
#Get rid of Antarctica
biogeeographicv3<-biogeeographicv2 %>%
  filter(!REALM == "Antarctic")

st_crs(biogeeographicv3) <- 4326
st_crs(coastlines) <- 4326
crs(biogeeographicv3)
crs(coastlines)

#change projection to equal area
st_crs(biogeeographicv3) #this tells you what coordinate reference system you are in
new_proj <- "+proj=moll +datum=WGS84 +no_defs +over" #assign mollweide projection 
biogeeographicv4 <- st_transform(biogeeographicv3, crs = new_proj) # reproject 
st_crs(biogeeographicv4) #notice how we changed the projected CRS?

st_crs(coastlines) #this tells you what coordinate reference system you are in
new_proj <- "+proj=moll +datum=WGS84 +no_defs +over" #assign mollweide projection 
coastlines2 <- st_transform(coastlines, crs = new_proj) # reproject 
st_crs(coastlines2)


#make coordinate point data

df1<-st_as_sf(df1, coords = c("Lon", "Lat")) %>%
  st_set_crs(st_crs(coastlines))

#Plot world map
World_map<-ggplot() + 
  geom_sf(data = biogeeographicv4, aes(fill = REALM, color = REALM)) +
  guides(fill = guide_legend(override.aes = list(size=0.5))) +
  geom_sf(data = df1, aes(size =Total.species.richness.on.islands ),
          fill = "orange" ,color = "white", pch=21, alpha =0.7, show.legend = F) +
  scale_fill_viridis_d(option = "D") +
  scale_colour_viridis_d(option = "D") +
  theme_minimal() +
  scale_size_continuous(range = c(1,15)) +
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



#Save for plotting fill figure later
saveRDS(World_map, file = "worldmap.rds")
rm(World_map)
World_map<-readRDS("~/Google Drive/Dissertation/Species_Area/SpeciesArea/worldmap.rds")



ggsave("map1.tiff",plot = last_plot(), dpi =600, width = 25, height = 15, units = "cm")


