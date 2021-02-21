library(lme4)

#here i make fake data according to your data
df<-data.frame(SqrtW = runif(100),
               length = runif(100:199),
               WaterCh = c(rep("Blackwater", 50), rep("Clearwater",50)),
               Habitat = c(rep("Floodplain", 50), rep("River", 50)),
               Binomial = c(rep("SpA", 20),rep("SpB", 20),rep("SpC", 20),rep("SpD", 20),rep("SpE", 20)))

df<-expand.grid(
            WaterCh=c("Blackwater", "Clearwater")
            , Habitat = c("Floodplain", "River"),
            Binomial = c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5"))
df2<-expand.grid(
  WaterCh=c("Blackwater", "Clearwater")
  , Habitat = c("Floodplain", "River"),
  Binomial = c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5"))
df3<-expand.grid(
  WaterCh=c("Blackwater", "Clearwater")
  , Habitat = c("Floodplain", "River"),
  Binomial = c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5"))
df<-rbind(df,df2,df3)

df$SqrtW =  rchisq(60,34)
df$length = rchisq(60,67)

df<-data.frame(df) #this is the fake dataset

m1<-lmer(data = df, SqrtW~ length + WaterCh*Habitat + (1|Binomial)) #run your model
summary(m1)


newdat <- expand.grid(
  length=seq(min(df$length), max(df$length), length.out = 10)
  , WaterCh=c("Blackwater", "Clearwater")
  , Habitat = c("Floodplain", "River"),
  SqrtW = 0
)  #create new dataset based off of the data
newdat$SqrtW <- predict(m1,newdat,re.form=NA) #predict values using new dataset
mm <- model.matrix(terms(m1),newdat) #Create model matrix from the model structure

#check dimensions
dim(vcov(m1)) 
dim(mm)

#calculate errors
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))

cmult <- 2 ## could use 1.96
newdat <- data.frame(
  newdat
  , plo = newdat$SqrtW-cmult*sqrt(pvar1)#calculate 95 CI
  , phi = newdat$SqrtW+cmult*sqrt(pvar1)
)

#plot 
g0 <- ggplot(newdat, aes(x=length, y=SqrtW, colour=Habitat))+ geom_line() +
  geom_ribbon(aes(x = length, ymin = plo, ymax = phi, fill = Habitat,
                  color = Habitat), alpha = 0.5) +
  facet_wrap(~WaterCh)
g0


