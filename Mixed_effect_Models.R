library(patchwork)
library(sjPlot)
library(tidyverse)
library(lme4)
library(MuMIn)
library(ggeffects)
df<-read.csv("s_area_trial.csv") # load data

#we filter data 
df1<-df %>%
  filter(!Island_or_Mainland == "Mainland") %>%
  filter(!is.na(Species.Richness)) %>%
  filter(! Species.Richness == 0)

#log transfrom area and SR
df1$log_areas<-log10(df1$Island.Area..m2.)
df1$response <- log10(df1$Species.Richness)

###Models
m1<-lmer(data =df1, response~ log_areas + (Ecoregion|SAR_TYPE),
         control = lmerControl(optimizer ="Nelder_Mead"), REML = F) #conveergence issues
m2<-lmer(data =df1, response~ log_areas +  (1|Ecoregion) + (1|SAR_TYPE))  
m3<-glmer.nb(data =df1, Species.Richness ~ scale(Island.Area..m2.) +  (1|Ecoregion) + (1|SAR_TYPE)) #competing model
m4<-glmer.nb(data =df1, Species.Richness ~ scale(Island.Area..m2.) +  (SAR_TYPE|Ecoregion)) #convergence issues

ranef(m2) #random effects
isSingular(m2) #checking for singularity
summary(m2) #summary
r.squaredGLMM(m2) # rsquares
AICc(m2,m3) #competing model checking

#chcking model residuals
plot(m2)
hist(residuals(m2))

#extract marginal and conditonal effects
pr<-ggpredict(m2, c("log_areas", "Ecoregion", "SAR_TYPE"), type = "re")
pr2<-ggpredict(m2, c("log_areas"), type = "fe")
ranef(m2)
plot(pr2)
trial_data_main<-data.frame(pr) #randoms
fixed_effects<-data.frame(pr2) #fixed effects

trial_data_Neo<-data.frame(pr)
random_intercepts<-ggplot()+
  geom_line(data=trial_data_main, mapping = aes(x = x, y = predicted, color = group)) +
  facet_wrap(~facet) +
  geom_point(data = df1, mapping = aes(x = log_areas, y = response, color = Ecoregion),
             alpha = 0.6) +
  scale_color_viridis_d(option = "C") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(axis.title = element_text(face= "bold", size =8),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5))


marginal_effects<-ggplot() +
  geom_point(data = df1, aes(x = log_areas, y = response, color = SAR_TYPE),alpha = 0.6) +
  geom_ribbon(data = fixed_effects, aes(x= x , ymin= conf.low, ymax = conf.high), 
              alpha=0.3) +
  geom_line(data = fixed_effects, aes(x = x, y = predicted)) +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "SAR Type") +
  theme_bw() +
  theme(axis.title = element_text(face= "bold", size =8),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5))+
  scale_color_manual(values  = c("skyblue", "orange"))

random_intercepts + {
  SAR_type_plot + {
    marginal_effects
  }
} +
  plot_layout(ncol=1) +
  plot_annotation(tag_levels = "A")


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


###let's separtate datasets and analyze only true islands
df3<-df1 %>% filter(! SAR_TYPE == "Mainland")
df4<-df1 %>% filter(! SAR_TYPE == "Insular")

m5<-lmer(data =df3, response~ log_areas +  (1|Ecoregion))  #true island model
m6<-lmer(data =df4, response~ log_areas +  (1|Ecoregion))  #mainlan model
r.squaredGLMM(m5)
summary(m5)
r.squaredGLMM(m5)
summary(m6)
qqnorm(residuals(m5))

pr3<-ggpredict(m5, c("log_areas", "Ecoregion"), type = "re")
pr4<-ggpredict(m5, c("log_areas"), type = "fe")
insular1<-data.frame(pr3)
insular2<-data.frame(pr4)

insular_only_model<-ggplot() +
  geom_ribbon(data = insular2, aes(x =x, ymin = conf.low, ymax = conf.high), fill = "white",alpha = 0.3) +
  geom_point(data = df3, aes(x = log_areas, y = response, color = Ecoregion), alpha = 0.5) +
  geom_line(data = insular1, aes(x = x, y = predicted, color =group), lty="dashed") +
  geom_line(data = insular2, aes(x = x, y = predicted)) +
  scale_color_viridis_d(option = "C") +
  labs(x = "Log(Area)", y = "Log(Species Richness)", color = "Biogeographic \nRealm") +
  theme_dark() +
  theme(axis.title = element_text(face= "bold", size =8),
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size =5))

random_intercepts + {
  marginal_effects + {
    insular_only_model
  }
} +
  plot_layout(ncol=1) +
  plot_annotation(tag_levels = "A")


ggsave('MIXEDMODEL.png', 
       height = 5, 
       width = 7,
       dpi = 500)
