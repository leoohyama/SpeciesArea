####Running Models to find z scores

#load rar data file

df<-read.csv("s_area_trial.csv")
library(tidyverse)
colnames(df)
df1<-df %>%
  filter(!Island_or_Mainland == "Mainland") %>%
  filter(Study_ID ==13)
m1<-lm(data = df1, log(Species.Richness)~log(Island.Area..m2.))
summary(m1)

