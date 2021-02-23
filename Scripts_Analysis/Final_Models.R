library(lme4)
library(piecewiseSEM)
library(bbmle)
library(MuMIn)
library(nlme)
library(tidyverse)
library(car)
library(viridis)

##Read Data
df<-read.csv("Modelling_data.csv")
df<-df %>% filter(!Study.ID %in% c("29"))
length(unique(df$Study.ID))
df %>% filter(SAR_type == "Mainland")
mean(df$Z)
df %>% group_by(Ecoregion) %>% summarise(n = n())

#looking at isolation
df %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = Isolation_km, y =Z))
df %>% drop_na(Isolation_km) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =log(Total.species.richness.on.islands), color = Island_Type))
df %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =, color = Island_Type))


isolation<-df %>% drop_na(Isolation_km,Exotic_percent) 
isolation1<-df %>% drop_na(Isolation_km) 

m1<-lm(data = isolation1, log(Z)~log(Isolation_km))
summary(m1)
hist(isolation1$Z)
m1<-glm(data = isolation1, Z~log(Isolation_km), family = Gamma(link="log"))
plot(m1)


m1<-lm(data = isolation1, log(Isolation_km)~Island_Type)
m2<-glm(data = isolation1, Isolation_km~Island_Type, family = Gamma(link = "log"))
AICc(m1,m2)
summary(m2)
r.squaredGLMM(m2)

hist(isolation1$Isolation_km)

m1<-glm(data = isolation, Exotic_percent~log(Isolation_km), family = Gamma(link = "log"))

summary(m1)  
hist(isolation$Exotic_percent)

#average island per study
df %>% group_by(SAR_type) %>%
  summarise(mean= mean(Number.of.islands.patches.in.study, na.rm =T))


#let's look at exotic percentage
df %>%
  drop_na(Exotic_percent) %>%
  dplyr::select(SAR_type, Exotic_percent) %>%
  group_by(SAR_type) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
df %>%
  drop_na(Exotic_percent) %>%
  dplyr::select(SAR_type, Ecoregion, Exotic_percent) %>%
  group_by(SAR_type, Ecoregion) %>%
  summarise(tot = n(), mean = mean(Exotic_percent))

df %>%
  drop_na(Exotic_percent) %>%
  dplyr::select(Island_Type, Exotic_percent) %>%
  group_by(Island_Type) %>%
  summarise(tot = n(), mean = mean(Exotic_percent))

df %>%
  drop_na(Exotic_percent) %>%
  dplyr::select(Exotic_percent) %>%
  arrange(desc(Exotic_percent))

#C analysis
c_df<-df %>% filter(!Study.ID == 29) %>%
  filter(!z_origin == "paper") %>%
  dplyr::select(SAR_type, Ecoregion,C)

boxplot(c_df$C ~ c_df$Ecoregion)



m1<-lm(data = c_df, C~SAR_type)
exp(2.71)
exp(1.845)
summary(m1)

glmgamma<-c_df %>% filter(! C < 0)
m2<-glm(data = glmgamma, C~SAR_type, family = Gamma(link = "log"))
summary(m2)
r.squaredGLMM(m2)
glmgamma$residuals<-residuals(m2)
boxplot(glmgamma$residuals ~ glmgamma$SAR_type)
plot(m2)

c_df %>%
  group_by(SAR_type, Ecoregion) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

c_df %>%
  group_by(SAR_type) %>%
  dplyr::select(SAR_type, C) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

c_df %>%
  group_by(SAR_type, Ecoregion) %>%
  summarise(n = n(), mean = mean(C)) %>%
  dplyr::select(Ecoregion, SAR_type, mean) %>%
  pivot_wider(names_from = SAR_type, values_from = mean) %>%
  mutate(ratio = Insular/ Mainland)

#anova 
anova_df<-df

boxplot(anova_df$Z ~ anova_df$SAR_type)
mean(anova_df$Z[anova_df$SAR_type == "Mainland"])
mean(anova_df$Z[anova_df$SAR_type == "Insular"])

#SE of the means
anova_df %>% dplyr::select(SAR_type, Z) %>% group_by(SAR_type) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
  


ggplot(data = anova_df) + geom_point(aes(x= Number.of.islands.patches.in.study, y = Z, color = SAR_type)) +
  scale_x_log10()

m1<-glm(data = anova_df, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
summary(m1)
r.squaredGLMM(m1)
piecewiseSEM::rsquared(m1)
library(ggeffects)
m1<-glm(data = anova_df, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
hist(anova_df$Z)
rsquared(m2)
r.squaredGLMM(m1)
mydf <- ggpredict(m1, terms = c("SAR_type"))
mydf<-data.frame(mydf)

ggplot() +
  geom_point(mydf, mapping = aes(x = x, y = predicted), color = "dodgerblue", 
             size = 8) +
  geom_errorbar(mydf,mapping = aes(x = x,ymin=conf.low, ymax=conf.high), width=.3, size = 3, color = "dodgerblue") +
  labs(y = "Model fits") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 22,angle = 0, face = 'bold', vjust = 0.5, color = "white"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 22, face = "bold", color = "white"),
    axis.line = element_line(color = "white", 
                             size = 1, linetype = "solid")
    )
##prepping data for models
df<-df %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:41] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:41] #scale precip
df$scale_lat<-scale((df$Lat))[1:41] #scale latitude
df$scale_lon<-scale((df$Lon))[1:41]


#model
m1<-lm(data=df, Z~SAR_type)
m2<-glm(data=df, Z~SAR_type, family = "Gamma"(link="log"))
m2<-glm(data=df, Z~Ecoregion, family = "Gamma"(link="log"))

summary(m2)
rsquared(m2)
###with OM as covariate
df1<-df %>%
  filter(!RANGE_present == "NO")



df1$Range.order.of.magnitude.<-as.numeric(as.character(df1$Range.order.of.magnitude.))
df1$scale_OM<-scale((df1$Range.order.of.magnitude.))[1:35]



m0<-lm(data = df1, Z~1)
m1<-glm(data = df1, Z~ scale_OM, family = "Gamma"(link="log"))
m2<-glm(data = df1, Z~ scale_OM + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df1, Z~ scale_OM + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df1, Z~ scale_OM + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df1, Z~ scale_OM * scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df1, Z~ scale_OM * scale_MAT, family = "Gamma"(link="log"))
m7<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m8<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0, m1,m2, m3,m4, m5,m6, m7, m8,  weights = T )
summary(m2)
r.squaredGLMM(m4)
r.squaredGLMM(m5)



###using SAR_type as covariate
##prepping data for models
df<-df %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:41] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:41] #scale precip
df$scale_lat<-scale((df$Lat))[1:41] #scale latitude
df$scale_lon<-scale((df$Lon))[1:41]


m0<-lm(data = df, Z~1)
m1<-glm(data = df, Z~ SAR_type, family = "Gamma"(link="log"))
m2<-glm(data = df, Z~ SAR_type + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df, Z~ SAR_type + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df, Z~ SAR_type + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0,m1,m2,m3,m4,m5,m6,  weights = T)
r.squaredGLMM(m6)
r.squaredGLMM(m4)

summary(m2)
rsquared(m2)
plot(m8)
vif(m2)

##############
##ggpredict
library(ggeffects)

mydf <- ggpredict(m2, terms = c("scale_MAP", "SAR_type"))
mydf<-data.frame(mydf)
figxyz<-ggplot() +
  geom_ribbon(mydf, mapping = aes(x = x, ymin = conf.low, ymax=conf.high, fill = group, color = NULL), 
              alpha =0.3) +
  geom_line(mydf,mapping = aes(x = x, y = predicted, color = group), size =1.1) +
  scale_fill_manual(values  = c("skyblue", "orange")) +
  scale_color_manual(values  = c("skyblue", "orange")) +
  labs(x = "Mean Annual Precipitation (Scaled)", y = "Z", fill = "Species\n-Area\nType", 
       color = "Species\n-Area\nType")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 14,angle = 0, face = 'bold', vjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  geom_point(data = df,mapping = aes(x=scale_MAP, y =Z , color= SAR_type), 
             size = 2)
figxyz
ggsave('figxyz.jpg', 
       height = 4, 
       width = 5.5,
       dpi = 500)


#graph for proposal

ggplot() +
  geom_ribbon(mydf, mapping = aes(x = x, ymin = conf.low, ymax=conf.high, fill = group, color = NULL), 
              alpha =0.5) +
  geom_line(mydf,mapping = aes(x = x, y = predicted, color = group), size =1.5) +
  scale_fill_manual(values  = c("skyblue", "orange")) +
  scale_color_manual(values  = c("skyblue", "orange")) +
  labs(x = "Mean Annual Precipitation (Scaled)", y = "Z", fill = "SAR Type", 
       color = "SAR Type")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size = 22, face = "bold", color = "white"),
        legend.title = element_text(size = 22, face = "bold", color = "white"),
    axis.title.y = element_text(size = 22,angle = 0, face = 'bold', vjust = 0.5, color = "white"),
        axis.title.x = element_text(size = 22, face = "bold", color = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 22, face = "bold", color = "white")) +
  geom_point(data = df,mapping = aes(x=scale_MAP, y =Z , color= SAR_type), size = 3)


###running all models with less than 8 studies 
##Read Data
df<-read.csv("Modelling_data.csv")
df2<-df %>% filter(!Study.ID %in% c("29")) %>%
  filter(!Number.of.islands.patches.in.study<10)

#looking at isolation
df2 %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = Isolation_km, y =Z))
df2 %>% drop_na(Isolation_km) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =log(Total.species.richness.on.islands), color = Island_Type))
df2 %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =, color = Island_Type))


isolation<-df2 %>% drop_na(Isolation_km,Exotic_percent) 
isolation1<-df2 %>% drop_na(Isolation_km) 

m1<-lm(data = isolation1, log(Z)~log(Isolation_km))
summary(m1)
hist(isolation1$Z)
m1<-glm(data = isolation1, Z~log(Isolation_km), family = Gamma(link="log"))
plot(m1)


m1<-lm(data = isolation1, log(Isolation_km)~Island_Type)
m2<-glm(data = isolation1, Isolation_km~Island_Type, family = Gamma(link = "log"))
AICc(m1,m2)
summary(m1)
r.squaredGLMM(m2)

hist(isolation1$Isolation_km)

m1<-glm(data = isolation, Exotic_percent~log(Isolation_km), family = Gamma(link = "log"))

summary(m1)  
hist(isolation$Exotic_percent)

#average island per study
df %>% group_by(SAR_type) %>%
  summarise(tots= n())


#C analysis
c_df2<-df2 %>% filter(!z_origin == "paper") %>%
  dplyr::select(SAR_type, C)


m1<-lm(data = c_df2, C~SAR_type)
summary(m1)

c_df %>%
  group_by(SAR_type, Ecoregion) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

c_df %>%
  group_by(SAR_type) %>%
  dplyr::select(SAR_type, C) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

c_df %>%
  group_by(SAR_type, Ecoregion) %>%
  summarise(n = n(), mean = mean(C)) %>%
  dplyr::select(Ecoregion, SAR_type, mean) %>%
  pivot_wider(names_from = SAR_type, values_from = mean) %>%
  mutate(ratio = Insular/ Mainland)

#anova 
anova_df<-df2

boxplot(anova_df$Z ~ anova_df$SAR_type)
mean(anova_df$Z[anova_df$SAR_type == "Mainland"])
mean(anova_df$Z[anova_df$SAR_type == "Insular"])

#SE of the means
anova_df %>% dplyr::select(SAR_type, Z) %>% group_by(SAR_type) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))



ggplot(data = anova_df) + geom_point(aes(x= Number.of.islands.patches.in.study, y = Z, color = SAR_type)) +
  scale_x_log10()

m1<-glm(data = anova_df, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
summary(m1)
r.squaredGLMM(m1)
piecewiseSEM::rsquared(m1)
library(ggeffects)
m1<-glm(data = anova_df, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
hist(anova_df$Z)
rsquared(m2)
r.squaredGLMM(m1)
mydf <- ggpredict(m1, terms = c("SAR_type"))
mydf<-data.frame(mydf)

ggplot() +
  geom_point(mydf, mapping = aes(x = x, y = predicted), color = "dodgerblue", 
             size = 8) +
  geom_errorbar(mydf,mapping = aes(x = x,ymin=conf.low, ymax=conf.high), width=.3, size = 3, color = "dodgerblue") +
  labs(y = "Model fits") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 22,angle = 0, face = 'bold', vjust = 0.5, color = "white"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 22, face = "bold", color = "white"),
    axis.line = element_line(color = "white", 
                             size = 1, linetype = "solid")
  )
##prepping data for models
df2<-df2 %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df2$scale_MAP<-scale(df2$Mean_Annual_Precip, center = T)[1:nrow(df2)] #scale temp
df2$scale_MAT<-scale(df2$Mean_Annual_Temp, center = T)[1:nrow(df2)] #scale precip
df2$scale_lat<-scale((df2$Lat))[1:nrow(df2)] #scale latitude
df2$scale_lon<-scale((df2$Lon))[1:nrow(df2)]


#model
m1<-lm(data=df2, Z~SAR_type)
m2<-glm(data=df2, Z~SAR_type, family = "Gamma"(link="log"))
m2<-glm(data=df2, Z~Ecoregion, family = "Gamma"(link="log"))

summary(m2)
rsquared(m2)
###with OM as covariate
df1<-df2 %>%
  filter(!RANGE_present == "NO")



df1$Range.order.of.magnitude.<-as.numeric(as.character(df1$Range.order.of.magnitude.))
df1$scale_OM<-scale((df1$Range.order.of.magnitude.))[1:nrow(df1)]



m0<-lm(data = df1, Z~1)
m1<-glm(data = df1, Z~ scale_OM, family = "Gamma"(link="log"))
m2<-glm(data = df1, Z~ scale_OM + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df1, Z~ scale_OM + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df1, Z~ scale_OM + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df1, Z~ scale_OM * scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df1, Z~ scale_OM * scale_MAT, family = "Gamma"(link="log"))
m7<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m8<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0, m1,m2, m3,m4, m5,m6, m7, m8,  weights = T )
summary(m4)
r.squaredGLMM(m6)
r.squaredGLMM(m3)
r.squaredGLMM(m5)
r.squaredGLMM(m7)
r.squaredGLMM(m8)

###using SAR_type as covariate
##prepping data for models
df3<-df2 %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df3$scale_MAP<-scale(df3$Mean_Annual_Precip, center = T)[1:nrow(df3)] #scale temp
df3$scale_MAT<-scale(df3$Mean_Annual_Temp, center = T)[1:nrow(df3)] #scale precip
df3$scale_lat<-scale((df3$Lat))[1:nrow(df3)] #scale latitude
df3$scale_lon<-scale((df3$Lon))[1:nrow(df3)]


m0<-lm(data = df3, Z~1)
m1<-glm(data = df3, Z~ SAR_type, family = "Gamma"(link="log"))
m2<-glm(data = df3, Z~ SAR_type + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df3, Z~ SAR_type + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df3, Z~ SAR_type + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df3, Z~ SAR_type + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df3, Z~ SAR_type + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0,m1,m2,m3,m4,m5,m6,  weights = T)
r.squaredGLMM(m6)
r.squaredGLMM(m5)

summary(m4)

##############
##ggpredict
library(ggeffects)

mydf <- ggpredict(m2, terms = c("scale_MAP", "SAR_type"))
mydf<-data.frame(mydf)
figxyz<-ggplot() +
  geom_ribbon(mydf, mapping = aes(x = x, ymin = conf.low, ymax=conf.high, fill = group, color = NULL), 
              alpha =0.3) +
  geom_line(mydf,mapping = aes(x = x, y = predicted, color = group), size =1.1) +
  scale_fill_manual(values  = c("skyblue", "orange")) +
  scale_color_manual(values  = c("skyblue", "orange")) +
  labs(x = "Mean Annual Precipitation (Scaled)", y = "Z", fill = "Species\nArea\nType", 
       color = "Species\nArea\nType")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 14,angle = 0, face = 'bold', vjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  geom_point(data = df,mapping = aes(x=scale_MAP, y =Z , color= SAR_type), 
             size = 2)
figxyz
ggsave('figxyz.jpg', 
       height = 4, 
       width = 5.5,
       dpi = 500)


#graph for proposal

ggplot() +
  geom_ribbon(mydf, mapping = aes(x = x, ymin = conf.low, ymax=conf.high, fill = group, color = NULL), 
              alpha =0.5) +
  geom_line(mydf,mapping = aes(x = x, y = predicted, color = group), size =1.5) +
  scale_fill_manual(values  = c("skyblue", "orange")) +
  scale_color_manual(values  = c("skyblue", "orange")) +
  labs(x = "Mean Annual Precipitation (Scaled)", y = "Z", fill = "SAR Type", 
       color = "SAR Type")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size = 22, face = "bold", color = "white"),
        legend.title = element_text(size = 22, face = "bold", color = "white"),
        axis.title.y = element_text(size = 22,angle = 0, face = 'bold', vjust = 0.5, color = "white"),
        axis.title.x = element_text(size = 22, face = "bold", color = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 22, face = "bold", color = "white")) +
  geom_point(data = df,mapping = aes(x=scale_MAP, y =Z , color= SAR_type), size = 3)


