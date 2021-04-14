##########################################################
#########Mixed Effect Piecewise Models - Leo###############
##########################################################

par(mfrow=c(1,1))
library(dplyr)

dat <- read.csv("~/Downloads/Ohyama_Island_Datav2.csv")

dat$Area <- dat$Island_area / 1e+6

Global <- 
  dat %>%
  select(Area, Species.Richness, Study_ID, SAR_TYPE, Biogeo_realm) 


insular <- Global %>%
  filter(SAR_TYPE == "Insular")%>%
  select(Area, Species.Richness, Biogeo_realm)


######################################################
####fit segmented model with no random effect
library(segmented)

#add a log area column
insular <- mutate(insular, log_area = log(Area))

l11 <- lm(Species.Richness ~ log_area, data = insular)

s11 <-  segmented(l11, seg.Z = ~log_area, psi = 5)

plot(s11)#matches well with sars package function which is good!

##########################################################


###############################################################
###########Fit Mixed Effect version#################################
#######################################################################

#source the mixed effect segmented package functions
source("~/Downloads/source-segmented.lme.r")

library(nlme)


#id is the grouping variable (Biogeo_realm for us)
insular <- mutate(insular, id = Biogeo_realm)

#create a log area column (done above)
#insular <- mutate(insular, log_area = log(Area))

#for it to work it needs to be ordered by realm and then area within
#realm
insular <- insular[order(insular$Biogeo_realm, insular$log_area), ]

#fit a mixed effect model with random intercept by id (realm)
#same as
#l11 <- lme(Richness ~ log_area, random=~1|Biogeo_realm, data = insular)
l1 <- lme(Species.Richness ~ log_area, random=list(id = pdDiag(~1)),
          data = insular)


#fit the segmented version (just random intercept)
s1 <- segmented.lme(l1, Z = log_area, 
                    random = list(id =  pdDiag(~1)))

plot(s1, n.plot=c(4,2), pop = TRUE, leg = "left", xlab = "Log(area)")


#fit the segmented version (random intercept & random threshold)
s2 <- segmented.lme(l1, Z = log_area, 
                    random = list(id =  pdDiag(~1 + G0)))

plot(s2, n.plot=c(4,2), pop = TRUE, leg = "left", xlab = "Log(area)")


#compare AIC with standard segmented model
AIC(s1); AIC(s2); AIC(s11)

summary(s2)
