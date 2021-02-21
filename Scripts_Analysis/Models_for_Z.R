####Running Models to find z scores

#load rar data file

df<-read.csv("s_area_trial.csv")
df$Area_km <- df$Island.Area..m2. / 1e+6 #turn it into square km
library(tidyverse)
colnames(df)
df1<-df %>%
  filter(!Island_or_Mainland == "Mainland") %>%
  filter(Study_ID =="14b") %>%
  filter(! Species.Richness == 0)
m1<-lm(data = df1, log(Species.Richness)~log(Area_km))
summary(m1)

plot(log(df1$Island.Area..m2.), log(df1$Species.Richness))
