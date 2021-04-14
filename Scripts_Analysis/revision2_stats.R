library(tidyverse)
library(emmeans)
#load datasets
dat <- read.csv("~/Downloads/Ohyama_Island_Datav2.csv")
df<-read.csv("/Users/leoohyama/Google Drive/Dissertation/Species_Area/SpeciesArea/Modelling_data.csv")


mean_areas<-dat %>% group_by(Study_ID) %>%
  summarise(mean_area = mean(Island_area),
            med_area = median(Island_area))

car::vif(m1)

ma_df<-left_join(df, mean_areas, by = c("Study.ID" = "Study_ID"))

ma_df$mean_area <- ma_df$mean_area / 1e+6
ma_df$med_area <- ma_df$med_area / 1e+6
m1<-glm(data=ma_df, Z~mean_area+SAR_type, family = Gamma(link = "log"))
m1<-lm(data=ma_df, log(mean_area)~SAR_type)
summary(m1)

boxplot(log(ma_df$mean_area)~ma_df$SAR_type)
chisq.test(table(log(ma_df$mean_area), ma_df$SAR_type))


num.intercepts
vif2(m1)
hist(sqrt(ma_df$C))
m1<-glm(data=ma_df, sqrt(C)~SAR_type)
summary(m1)

plot(m1)

nrow(drop_na(ma_df, mean_area))
m1<-glm(data=ma_df, Z~log(mean_area)+SAR_type, family = Gamma(link = "log"))
m1<-glm(data=ma_df, Z~log(mean_area), family = Gamma(link = "log"))
plot(log(ma_df$mean_area), ma_df$Z)
summary(m1)
car::vif(m1)
emmeans(m1, specs = pairwise ~ SAR_type, type = "response")
df$range=df$Max_A-df$Min_A


m1<-glm(data=ma_df, C~log(mean_area) + SAR_type)
summary(m1)

#T-test for reported vs. original Z
df<-read.csv("/Users/leoohyama/Google Drive/Dissertation/Species_Area/SpeciesArea/Modelling_data.csv")
z_comp<-df %>% drop_na(Original_z_reported)
t.test(z_comp$Original_z_reported, z_comp$Z, paired = TRUE, alternative = "two.sided")
hist(z_comp$Original_z_reported)
shapiro.test(z_comp$Original_z_reported)
shapiro.test(z_comp$Z)


#glms for insular vs mainland maximum areas

hist(log(df$Max_A))
m1<-glm(data=df, log(Max_A)~ SAR_type)
summary(m1)
plot(m1)
