library(patchwork)
library(sjPlot)
library(tidyverse)
library(lme4)
library(MuMIn)
library(ggeffects)
library(glmmTMB)
library(bbmle)


df<-read.csv("s_area_trial.csv") # load data
#get rid of repeats
df <- df %>%
  filter(!Island_or_Mainland == "Mainland") %>%
  filter(!is.na(Species.Richness)) %>%
  filter(! Species.Richness == 0)


#count number of studies 
length(unique(df$Study_ID))

df %>% group_by(Ecoregion, Island.Type) %>%
  summarise(n())
#quick plot to see distribution of islands by study
df %>% group_by(Study_ID, SAR_TYPE) %>%
  summarise(n()) %>%
  group_by(SAR_TYPE) %>%
  summarise(n())

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
m2<-lmer(data =island, response~ Island.Type + log_areas_km + (1+log_areas_km|Ecoregion)) 
m3<-lmer(data =island, response~ log_areas_km + (1|Island.Type/Ecoregion)) 


#models for mainland
n1<-lmer(data =mainland, response~ log_areas + (1|Ecoregion)) 
n2<-lmer(data =mainland, response~ log_areas + (1+log_areas|Ecoregion)) 

#extract marginal and conditonal effects
pr<-ggpredict(m1, c("log_areas_km", "Ecoregion"), type = "re")

pr2<-ggpredict(m2, c("log_areas_km"), type = "fe")

insular1<-data.frame(pr) #random ef
insular2<-data.frame(pr2) #fixed ef



insular_only_model_m<-ggplot() +
  geom_ribbon(data = insular2, aes(x =x, ymin = conf.low, ymax = conf.high),fill = "white",alpha = 0.2) +
  geom_point(data = island, aes(x = log_areas_km, y = response, fill = Ecoregion), pch = 21,
             color = "black",alpha = 0.7, size = 1.5) +
  geom_line(data = insular2, aes(x = x, y = predicted), color = "black", size = 1) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
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
  theme_dark() +
  theme(axis.title = element_text(face= "bold", size =12),
        axis.text = element_text(size =10),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5), 
        legend.position = "none")

insular_only_model_r


saveRDS(insular_only_model_r, "random_effect_plot")
saveRDS(insular_only_model_m, "fixed_effect_plot")

insular_only_model_m<-readRDS("fixed_effect_plot")
insular_only_model_r<-readRDS("random_effect_plot")
World_map<-readRDS("worldmap.rds")
library(patchwork)
map<-(World_map | (insular_only_model_m / insular_only_model_r + plot_layout(guides = 'auto'))) + plot_layout(guides = 'auto', width = c(2,0.6)) + plot_annotation(tag_levels = "A")
map
ggsave("map1.pdf",plot = map, dpi =550, width = 30, height = 15, units = "cm")
ggsave("map1.jpg",plot = map, dpi =550, width = 30, height = 15, units = "cm")

ggsave('MIXEDMODEL.tiff', 
       height = 5, 
       width = 7,
       dpi = 500)



###random effect plot of m2
randoms<-ranef(m2, condVar = TRUE) #grab intercepts 
intercept<-data.frame(randoms) #convert to dataframe
intercept <- intercept %>%
  filter(grpvar == "SAR_TYPE")
random<-ggplot(intercept) +
  geom_point(aes(x = grp, y = (condval))) +
  geom_errorbar(aes(x = grp, ymin = condval - condsd, ymax = condval + condsd, width = 0)) + 
  theme_classic() + labs(y = expression(bold("Intercept")), x = expression(bold("SAR Type"))) + 
  geom_hline(yintercept = 0, alpha = 0.5) +
  theme(axis.text.x = element_text(size=13), 
        axis.text.y = element_text(size=13), axis.title = element_text(size = 15)) +
  scale_y_discrete(limits = rev((intercept$grp)))

################################################################################
###let's separtate datasets and analyze only true islands##############################
################################################################################
df3<-df1 %>% filter(! SAR_TYPE == "Mainland")
df4<-df1 %>% filter(! SAR_TYPE == "Insular")

df3$Island.Type<-as.factor(df3$Island.Type)
df3$Ecoregion<-as.factor(df3$Ecoregion)

m5<-lmer(data =df3, response~ log_areas +  (1|Ecoregion))  #true island model
m6<-lmer(data =df3, response~ log_areas +  (1+Island.Type|Ecoregion))  #true island model with slopes, correlation issues
m7<-lmer(data =df3, response~ log_areas +  (1|Island.Type))  #true island model
m8<-lmer(data =df3, response~ log_areas +  (1|Island.Type) +  (1|Ecoregion))  #true island model
m9<-lmer(data =df3, response~ log_areas +  (1+log_areas|Ecoregion))  #true island model with slopes
m10<-lmer(data =df3, response~ Island.Type + log_areas +  (1+log_areas|Ecoregion))  #true island model with slopes

AICtab(m5,m7,m6, m8, m9,m10,m11, weights = TRUE)


AICtab(m9,m10)

summary(m9)
summary(m10)

r.squaredGLMM(m10)
r.squaredGLMM(m9)


AICctab(m5,m7, m8, m9,m10, weights = TRUE)

summary(m8)
hist(residuals(m10))
qqnorm(residuals(m10))

coef(m10)$Ecoregion #this gives random slope values and intercepts!!!

pr3<-ggpredict(m10, c("log_areas","Ecoregion"), type = "re")
plot(pr3)

pr4<-ggpredict(m10, c("log_areas", "Island.Type"), type = "fe")
insular1<-data.frame(pr3)
insular2<-data.frame(pr4)
plot(pr4)

whatislands<-df3 %>% dplyr::filter(Ecoregion %in% c("Nearctic", "Neotropic"))
whatislands %>% filter(Ecoregion == "Neotropic" & Island.Type == "Oceanic") #37 islands oceanic
whatislands %>% filter(Ecoregion == "Neotropic" & Island.Type == "Continental") #25 islands continental

df3 %>% mutate(predictions = predict(m5)) %>%
  ggplot(.) + geom_line(aes(x= log_areas, y = predictions, color = Ecoregion)) 


insular_only_model_m<-ggplot() +
  geom_ribbon(data = insular2, aes(x =x, ymin = conf.low, ymax = conf.high), fill = "white",alpha = 0.2) +
  geom_point(data = df3, aes(x = log_areas, y = response, color = Ecoregion), alpha = 0.7) +
  geom_line(data = insular2, aes(x = x, y = predicted)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(axis.title = element_text(face= "bold", size =8),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


insular_only_model_r<-ggplot() +
  geom_point(data = df3, aes(x = log_areas, y = response, color = Ecoregion), alpha = 0.7) +
  geom_line(data = insular1, aes(x = x, y = predicted, color =group)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(axis.title = element_text(face= "bold", size =8),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


map<-(World_map | (insular_only_model_m / insular_only_model_r + plot_layout(guides = 'auto'))) + plot_layout(guides = 'auto', width = c(2,0.6)) + plot_annotation(tag_levels = "A")
ggsave("map1.tiff",plot = map, dpi =550, width = 30, height = 15, units = "cm")

##########Global map with slopes




#proposal graphs
ggplot() +
  geom_point(data = df3, aes(x = log_areas, y = response, color = Ecoregion), alpha = 0.7, size = 3) +
  geom_line(data = insular1, aes(x = x, y = predicted, color =group), size = 2) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size = 22, face = "bold", color = "white"),
        legend.title = element_text(size = 22, face = "bold", color = "white"),
        axis.title = element_text(face= "bold", size =22, color = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size =16, color ="white"),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size =16, color ="white"),
        panel.grid = element_line(color = "white"))


insular_only_model_m<-ggplot() +
  geom_ribbon(data = insular2, aes(x =x, ymin = conf.low, ymax = conf.high), fill = "white",alpha = 0.5) +
  geom_point(data = df3, aes(x = log_areas, y = response, color = Ecoregion), alpha = 0.8, size = 3) +
  geom_line(data = insular2, aes(x = x, y = predicted),size = 1.5) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size = 22, face = "bold", color = "white"), legend.position = "none",
        legend.title = element_text(size = 22, face = "bold", color = "white"),
        axis.title = element_text(face= "bold", size =22, color = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size =16, color ="white"),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size =16, color ="white"),
        panel.grid = element_line(color = "white"))
