library(lme4)
library(piecewiseSEM)
library(bbmle)
library(MuMIn)
library(nlme)
library(tidyverse)
library(car)

##Read Data
df<-read.csv("Modelling_data.csv")

##prepping data for models
df<-df %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:34] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:34] #scale precip
df$scale_lat<-scale((df$Lat))[1:34] #scale latitude
df$scale_lon<-scale((df$Lon))[1:34]

m1<-lm(data=df, Z~SAR_type)
m2<-glm(data=df, Z~SAR_type, family = "Gamma"(link="log"))
m2<-glm(data=df, Z~Ecoregion, family = "Gamma"(link="log"))

summary(m2)
rsquared(m2)
###with OM as covariate
df1<-df %>%
  filter(!RANGE_present == "NO")



df1$Range.order.of.magnitude.<-as.numeric(as.character(df1$Range.order.of.magnitude.))
df1$scale_OM<-scale((df1$Range.order.of.magnitude.))[1:27]


m0<-lm(data = df1, Z~1)
m1<-glm(data = df1, Z~ scale_OM + scale_OM , family = "Gamma"(link="log"))
m2<-glm(data = df1, Z~ scale_OM + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df1, Z~ scale_OM + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df1, Z~ scale_OM + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df1, Z~ scale_OM * scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df1, Z~ scale_OM * scale_MAT, family = "Gamma"(link="log"))
m7<-glm(data = df1, Z~ scale_OM + Ecoregion, family = "Gamma"(link="log"))
m8<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m9<-glm(data = df1, Z~ scale_OM + Ecoregion + scale_MAT, family = "Gamma"(link="log"))

AICctab(m0, m1,m2, m3,m4, m5,m6, m7, m8,m9,  weights = T)

summary(m1)
rsquared(m1)


###using SAR_type as covariate
##prepping data for models
df<-df %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)

df$scale_MAP<-scale(df$Mean_Annual_Precip, center = T)[1:34] #scale temp
df$scale_MAT<-scale(df$Mean_Annual_Temp, center = T)[1:34] #scale precip
df$scale_lat<-scale((df$Lat))[1:34] #scale latitude
df$scale_lon<-scale((df$Lon))[1:34]



m0<-lm(data = df, Z~1)
m2<-glm(data = df, Z~ SAR_type + scale_MAP, family = "Gamma"(link="log"))
m3<-glm(data = df, Z~ SAR_type + scale_lat, family = "Gamma"(link="log"))
m4<-glm(data = df, Z~ SAR_type + scale_MAT, family = "Gamma"(link="log"))
m5<-glm(data = df, Z~ SAR_type * scale_MAP, family = "Gamma"(link="log"))
m6<-glm(data = df, Z~ SAR_type * scale_MAT, family = "Gamma"(link="log"))
m7<-glm(data = df, Z~ SAR_type + Ecoregion, family = "Gamma"(link="log"))
m8<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAP, family = "Gamma"(link="log"))
m9<-glm(data = df, Z~ SAR_type + Ecoregion + scale_MAT, family = "Gamma"(link="log"))


AICctab(m0,m2,m3,m4, m5,m6, m7, m8,m9,  weights = T)
summary(m2)
rsquared(m9)
plot(m2)
vif(m2)

##############
##ggpredict
library(ggeffects)

mydf <- ggpredict(m2, terms = c("scale_MAP", "SAR_type"))
mydf<-data.frame(mydf)
ggplot() +
  geom_ribbon(mydf, mapping = aes(x = x, ymin = conf.low, ymax=conf.high, fill = group, color = NULL), 
              alpha =0.3) +
  geom_line(mydf,mapping = aes(x = x, y = predicted, color = group), size =0.8) +
  scale_fill_manual(values  = c("skyblue", "orange")) +
  scale_color_manual(values  = c("skyblue", "orange")) +
  labs(x = "Mean Annual Precipitation (Scaled)", y = "Z", fill = "SAR Type", 
       color = "SAR Type")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 10,angle = 0, face = 'bold', vjust = 0.5),
        axis.title.x = element_text(size = 10, face = "bold")) +
  geom_point(data = df,mapping = aes(x=scale_MAP, y =Z , color= SAR_type))
ggsave('figxyz.tiff', 
       height = 4, 
       width = 5.5,
       dpi = 500)







