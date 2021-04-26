library(patchwork)
library(sjPlot)
library(tidyverse)
library(lme4)
library(MuMIn)
library(ggeffects)
library(glmmTMB)
library(bbmle)

#Mixed effect models for insular and mainland systems

df<-read.csv("s_area_trial.csv") # load data

#Clean and prep data
df <- df %>%
  filter(!Island_or_Mainland == "Mainland") %>%
  filter(!is.na(Species.Richness)) %>%
  filter(! Species.Richness == 0)

#get variables in order
df$SAR_TYPE<-as.factor(df$SAR_TYPE)
df$Ecoregion<-as.factor(df$Ecoregion)
df$Island.Type<-as.factor(df$Island.Type)
df$Area_km <- df$Island.Area..m2. / 1e+6 #turn it into square km

#log transfrom area and SR
df$log_areas<-log(df$Island.Area..m2.)
df$response <- log(df$Species.Richness)
df$log_areas_km<-log(df$Area_km)

################################################################################
###let's separtate datasets and analyze only true islands##############################
################################################################################
island<-df %>% filter(! SAR_TYPE == "Mainland")
mainland<-df %>% filter(! SAR_TYPE == "Insular")
island %>% group_by(Island.Type) %>%
  summarise(tot=n())

###Models for insular
m1<-lmer(data =island, response~ log_areas_km + (1+log_areas_km|Ecoregion)) 
summary(m1)
###Models for mainland
m2<-lmer(data =mainland, response~ log_areas_km + (1+log_areas_km|Ecoregion)) 
isSingular(m2) #singularity in model

#extract marginal and conditonal effects
pr<-ggpredict(m1, c("log_areas_km", "Ecoregion"), type = "re")

pr2<-ggpredict(m1, c("log_areas_km"), type = "fe")

insular1<-data.frame(pr) #random ef
insular2<-data.frame(pr2) #fixed ef



insular_only_model_m<-ggplot() +
  geom_ribbon(data = insular2, aes(x =x, ymin = conf.low, ymax = conf.high),fill = "grey",alpha = 0.5) +
  geom_point(data = island, aes(x = log_areas_km, y = response, fill = Ecoregion), pch = 21,
             color = "black",alpha = 0.7, size = 1.5) +
  geom_line(data = insular2, aes(x = x, y = predicted), color = "black", size = 1) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_bw() +
  theme(axis.title = element_text(face= "bold", size =12),
        axis.text = element_text(size =10),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5), legend.position = "none")

insular_only_model_m


insular_only_model_r<-ggplot() +
  geom_point(data = island, aes(x = log_areas_km, y = response, fill = Ecoregion), pch = 21,
             color = "black",alpha = 0.7, size = 1.5) +
  geom_line(data = insular1, aes(x = x, y = predicted, color =group), size = 1) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_bw() +
  theme(axis.title = element_text(face= "bold", size =12),
        axis.text = element_text(size =10),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5), 
        legend.position = "none")

insular_only_model_r

#Save plots 
saveRDS(insular_only_model_r, "random_effect_plot.rds")
saveRDS(insular_only_model_m, "fixed_effect_plot.rds")

#load up plots and world map
insular_only_model_m<-readRDS("fixed_effect_plot")
insular_only_model_r<-readRDS("random_effect_plot")
World_map<-readRDS("worldmap.rds")
library(patchwork)
map<-(World_map | (insular_only_model_m / insular_only_model_r + plot_layout(guides = 'auto'))) + plot_layout(guides = 'auto', width = c(2,0.6)) + plot_annotation(tag_levels = "A")
map

#Save plots
ggsave("map1.pdf",plot = map, dpi =550, width = 30, height = 15, units = "cm")
ggsave("map1.jpg",plot = map, dpi =550, width = 30, height = 15, units = "cm")




