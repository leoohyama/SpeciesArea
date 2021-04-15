library(lme4)
library(piecewiseSEM)
library(bbmle)
library(MuMIn)
library(nlme)
library(tidyverse)
library(car)
library(viridis)
library(ggeffects)



##Read Data
df<-read.csv("/Users/leoohyama/Google Drive/Dissertation/Species_Area/SpeciesArea/Modelling_data.csv")


#looking at isolation via plots
df %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = Isolation_km, y =Z))
df %>% drop_na(Isolation_km) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =log(Total.species.richness.on.islands), color = Island_Type))


isolation<-df %>% drop_na(Isolation_km,Exotic_percent) #get rid of NAs 
isolation1<-df %>% drop_na(Isolation_km) #get rid of NAs 


#model assessing slope ~ isolation
hist(isolation1$Z)
m1<-glm(data = isolation1, Z~log(Isolation_km), family = Gamma(link="log"))
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

#Calculating average number of islands per study
df %>% group_by(SAR_type) %>%
  summarise(mean= mean(Number.of.islands.patches.in.study, na.rm =T))


#Assessing non-native ant proportions
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

#Analyzing intercepts
c_df<-df %>% 
  filter(!z_origin == "paper") %>%
  dplyr::select(SAR_type, Ecoregion,C)

boxplot(c_df$C ~ c_df$Ecoregion)

c_df %>%
  dplyr::select(SAR_type, C) %>%
  group_by(SAR_type) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
#OLS assessing C ~ SAR_type
m1<-lm(data = c_df, C~SAR_type)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

#Summary statistics for intercept
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
slope_data<-df

#plot of sloep by sar_type
boxplot(slope_data$Z ~ slope_data$SAR_type)

#means of slope values for both insular and mainland
mean(slope_data$Z[slope_data$SAR_type == "Mainland"])
mean(slope_data$Z[slope_data$SAR_type == "Insular"])

#SE of the means
slope_data %>% dplyr::select(SAR_type, Z) %>% group_by(SAR_type) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
  
hist(slope_data$Z)
#modeling slope ~ sar_type
m1<-glm(data = slope_data, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))
summary(m1)
r.squaredGLMM(m1)
#######################################################
##prepping data for models with climatic covariates
df<-df %>%
  mutate(range = (Max_A - Min_A)) 

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:41] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:41] #scale precip
df$scale_lat<-scale((df$Lat))[1:41] #scale latitude
df$scale_lon<-scale((df$Lon))[1:41]

###with OM as covariate, get rid of any studyIDs with no range in area
df1<-df %>%
  filter(!RANGE_present == "NO")

df1$Range.order.of.magnitude.<-as.numeric(as.character(df1$Range.order.of.magnitude.))
df1$scale_OM<-scale((df1$Range.order.of.magnitude.))[1:35]


#Models for OM covariate 
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
vif(m2)
r.squaredGLMM(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

###using SAR_type as covariate
##prepping data for models
df<-df %>%
  mutate(range = (Max_A - Min_A)) 

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:41] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:41] #scale precip
df$scale_lat<-scale((df$Lat))[1:41] #scale latitude
df$scale_lon<-scale((df$Lon))[1:41]

#Models with SAR_type as a covariate
m0<-lm(data = df, Z~1)
m1<-glm(data = df, Z~ SAR_type, family = "Gamma"(link="log"))
m2<-glm(data = df, Z~ SAR_type + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df, Z~ SAR_type + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df, Z~ SAR_type + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0,m1,m2,m3,m4,m5,m6,  weights = T)

summary(m2)
vif(m2)
summary(m2)
r.squaredGLMM(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

##############
##Model_predictions

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


###running all models with studies of 10 or more islands 
##Read Data
df<-read.csv("Modelling_data.csv")
df2<-df %>% 
  filter(!Number.of.islands.patches.in.study<10)

#looking at isolation
df2 %>% drop_na(Isolation_km, Exotic_percent) %>%
  ggplot(.) + geom_point(aes(x = Isolation_km, y =Z))
df2 %>% drop_na(Isolation_km) %>%
  ggplot(.) + geom_point(aes(x = log(Isolation_km), y =log(Total.species.richness.on.islands), color = Island_Type))



isolation<-df2 %>% drop_na(Isolation_km,Exotic_percent) 
isolation1<-df2 %>% drop_na(Isolation_km) 

hist(isolation1$Z)
m1<-glm(data = isolation1, Z~log(Isolation_km), family = Gamma(link="log"))
r.squaredGLMM(m1)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))


#Intercepts for 10 or more island studies
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

#By SAR_type 10 or more island studies
sar_type_df_10<-df2

boxplot(sar_type_df_10$Z ~ sar_type_df_10$SAR_type)
mean(sar_type_df_10$Z[sar_type_df_10$SAR_type == "Mainland"])
mean(sar_type_df_10$Z[sar_type_df_10$SAR_type == "Insular"])

#SE of the means
sar_type_df_10 %>% dplyr::select(SAR_type, Z) %>% group_by(SAR_type) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

hist(sar_type_df_10$Z)

m1<-glm(data = sar_type_df_10, Z~as.factor(SAR_type), family = "Gamma"(link="log"))
summary(m1)
r.squaredGLMM(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

##prepping data for models including climate covariate with 10 islands or more
df2<-df2 %>%
  mutate(range = (Max_A - Min_A)) 

df2$scale_MAP<-scale(df2$Mean_Annual_Precip, center = T)[1:nrow(df2)] #scale temp
df2$scale_MAT<-scale(df2$Mean_Annual_Temp, center = T)[1:nrow(df2)] #scale precip
df2$scale_lat<-scale((df2$Lat))[1:nrow(df2)] #scale latitude
df2$scale_lon<-scale((df2$Lon))[1:nrow(df2)]

###with OM as covariate
df1<-df2 %>%
  filter(!RANGE_present == "NO")

df1$Range.order.of.magnitude.<-as.numeric(as.character(df1$Range.order.of.magnitude.))
df1$scale_OM<-scale((df1$Range.order.of.magnitude.))[1:nrow(df1)]


#Models
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
r.squaredGLMM(m4)
par(mfrow=c(2,2))
plot(m4)
par(mfrow=c(1,1))


###using SAR_type as covariate
##prepping data for models
df3<-df2 %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df3$scale_MAP<-scale(df3$Mean_Annual_Precip, center = T)[1:nrow(df3)] #scale temp
df3$scale_MAT<-scale(df3$Mean_Annual_Temp, center = T)[1:nrow(df3)] #scale precip
df3$scale_lat<-scale((df3$Lat))[1:nrow(df3)] #scale latitude
df3$scale_lon<-scale((df3$Lon))[1:nrow(df3)]

#Models 
m0<-lm(data = df3, Z~1)
m1<-glm(data = df3, Z~ SAR_type, family = "Gamma"(link="log"))
m2<-glm(data = df3, Z~ SAR_type + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df3, Z~ SAR_type + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df3, Z~ SAR_type + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df3, Z~ SAR_type + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df3, Z~ SAR_type + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0,m1,m2,m3,m4,m5,m6,  weights = T)
summary(m4)
r.squaredGLMM(m4)
par(mfrow=c(2,2))
plot(m4)
par(mfrow=c(1,1))