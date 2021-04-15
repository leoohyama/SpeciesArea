#Revision, 2nd Round, Added Stats
library(tidyverse)
library(emmeans)
#load datasets
dat <- read.csv("~/Downloads/Ohyama_Island_Datav2.csv")
df<-read.csv("/Users/leoohyama/Google Drive/Dissertation/Species_Area/SpeciesArea/Modelling_data.csv")

#calculating mean area of each dataset
mean_areas<-dat %>% group_by(Study_ID) %>%
  summarise(mean_area = mean(Island_area),
            med_area = median(Island_area))
#joining this with dataset1
ma_df<-left_join(df, mean_areas, by = c("Study.ID" = "Study_ID"))

#convert to km2
ma_df$mean_area <- ma_df$mean_area / 1e+6

#Assessing potential collinearity between mean area and SAR type 
chisq.test(table(log(ma_df$mean_area), ma_df$SAR_type))
#model with new covariate of mean area
m1<-glm(data=ma_df, Z~mean_area+SAR_type, family = Gamma(link = "log"))
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

#GLM for mean area in assessing C
m1<-glm(data=ma_df, C~log(mean_area) + SAR_type)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

#T-test for reported vs. original Z
df<-read.csv("/Users/leoohyama/Google Drive/Dissertation/Species_Area/SpeciesArea/Modelling_data.csv")
z_comp<-df %>% drop_na(Original_z_reported) %>%
  dplyr::select(Original_z_reported, Z) %>%
  pivot_longer(c(Original_z_reported, Z), names_to = "Type")
#Assess normality prior to test
shapiro.test(z_comp$value)

#Run t.test between reported and recalculated z values
t.test.res <- t.test(value ~ Type, data = z_comp, paired = TRUE)
t.test.res

