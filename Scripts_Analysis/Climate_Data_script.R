##This script extracts climate data from world clim based on Dataset1's coordinates
df<-read.csv("Modelling_data.csv", encoding = "UTF-8") #Read in dataset1

library(tidyverse)
library(spocc)
library(raster)

coords<-df %>% dplyr::select(Study.ID, Lat, Lon)
coords
lon_lat<-coords %>%
  dplyr::select(Lon, Lat)

a<-getData('worldclim', var = 'bio', res =10) #get climate data
MAT<-a[[1]] #get mean annual temp
MAP<-a[[12]] #get precipitation
MAT_values<-raster::extract(MAT, lon_lat) #extracting temperature 
MAP_values<-raster::extract(MAP, lon_lat) #extracting precipitation 
df<-data.frame(Mean_Annual_Temp = MAT_values,
               Mean_Annual_Precip = MAP_values) #dataframe all d
df
df<-cbind(df,coords)
write.csv(df, "world_clim_climate2.csv")
