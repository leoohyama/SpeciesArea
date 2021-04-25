
######################################################################
#########Ant SAR Multi-Model Inference#################################
###############################################################

library(sars)
library(dplyr)
library(ggplot2)
library(tidyr)
library(foreach)
library(parallel)
library(doParallel)
library(cluster)
library(RColorBrewer)
library(segmented)
library(nlme)

dat <- read.csv("D:\\documents\\Work\\On-going projects\\Ant SARs\\Ohyama_Island_Datav2.csv")
dat<-read.csv("Tom_Matthews_Code/Ohyama_Island_Datav2(1).csv")
dat$Area <- dat$Area / 1e+6 #turn areas into km2

#create global, insular and mainland datasets
Global <- 
  dat %>%
    select(Area, Richness, Study_ID, SAR_TYPE, Biogeo_realm) 


insular <- Global %>%
    filter(SAR_TYPE == "Insular")%>%
  select(Area, Richness, Biogeo_realm)
  

mainland <- Global %>%
  filter(SAR_TYPE == "Mainland") %>%
  select(Area, Richness, Biogeo_realm)

##################################################################
############Threshold Model Fits######################################
########################################################

thre_all <- vector("list", length = 4)
names(thre_all) <- c("inst_T_A","inst_T_R",
                     "mai_T_A","mai_T_R")

#fit one-threshold continuous and zero slope models to insular and mainland datasets
#using log area and log area and log richness
thre_all[[1]] <- sar_threshold(insular, interval = 0.1, mod = c("ContOne", "ZslopeOne"),
                         logAxes = "area")

thre_all[[2]] <- sar_threshold(insular, interval = 0.1, mod = c("ContOne", "ZslopeOne"),
                         logAxes = "both")

thre_all[[3]] <- sar_threshold(mainland, interval = 0.1, mod = c("ContOne", "ZslopeOne"),
                         logAxes = "area")

thre_all[[4]] <- sar_threshold(mainland, interval = 0.1, mod = c("ContOne", "ZslopeOne"),
                         logAxes = "both")

##threshold results: insular(log-log), mainland(log-log), insular(semi-log),
#mainland (semi-log)
summary(thre_all[[2]]); summary(thre_all[[4]]);summary(thre_all[[1]]);summary(thre_all[[3]])

#model fit table (note models sorted by BIC)
sta <- rbind(summary(thre_all[[2]])$Model_table, summary(thre_all[[4]])$Model_table,
      summary(thre_all[[1]])$Model_table,summary(thre_all[[3]])$Model_table) %>%
  as.data.frame()

sta <- sta %>%
  select(AICc, R2, Th1)

###########################################
########Figure 5#############################
#############################################

#Individual Best Model fits

#insular log-log
f1 <- sar_threshold(insular, interval = 0.1, mod = c("ContOne"), non_th_models = F,
                    logAxes = "both")

#insular semi-log
f2 <- sar_threshold(insular, interval = 0.1, mod = c("ContOne"), non_th_models = F,
                    logAxes = "area")

#mainland log-log
f3 <- sar_threshold(mainland, interval = 0.1, mod = c("ContOne"), non_th_models = F,
                    logAxes = "both")

#mainland semi-log
f4 <- sar_threshold(mainland, interval = 0.1, mod = c("ZslopeOne"), non_th_models = F,
                    logAxes = "area")

#Create Point colours
#colour blind friendly pallette: https://urldefense.proofpoint.com/v2/url?u=https-3A__thenode.biologists.com_data-2Dvisualization-2Dwith-2Dflying-2Dcolors_research_&d=DwIFaQ&c=sJ6xIWYx-zLMB3EPkvcnVg&r=IrHkU7fxgRceYNdeCFncvFFNuhguCYv8ta8BbTlpSu0&m=yNFLrqAd_-e_uCIPgqRujH4OF8U7VwS8q3q8bmtTUTg&s=GVUmymh6rXFO1BGpkg6uRuD3IwaHxkSqBUaM0oyEt24&e= )
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

##It is necessary to re-order the dataframes by increasing area,
#as we need to give the plot function the point colours, and 
#the models order points by increasing area

#re-order insular by increasing area
insular_ord <- insular[order(insular$Area),]
#re-order mainland by increasing area
mainland_ord <- mainland[order(mainland$Area),]

#create a vector of colours corresponding to correct realms
ins_col <- vapply(insular_ord$Biogeo_realm, function(x){
  switch(x,
         "Nearctic" = Okabe_Ito[1],
         "Palearctic" = Okabe_Ito[2],
         "Indomalaya" = Okabe_Ito[3],
         "Afrotropic" = Okabe_Ito[4],
         "Oceania" = Okabe_Ito[5],
         "Australasia" = Okabe_Ito[6],
         "Neotropic" = Okabe_Ito[7])
}, FUN.VALUE = character(1))

#same for mainland
mai_col <- vapply(mainland_ord$Biogeo_realm, function(x){
  switch(x,
         "Nearctic" = Okabe_Ito[1],
         "Palearctic" = Okabe_Ito[2],
         "Afrotropic" = Okabe_Ito[4],
         "Neotropic" = Okabe_Ito[7])
}, FUN.VALUE = character(1))


##make plots

jpeg(file = "Threshold_best_models_ins_col.jpeg", width = 30, height = 15, units = "cm", res = 300)

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.8, 4.1, 2))
plot(f1, ModTitle = "a) Insular - log-log", cex.main = 2, cex.lab = 1.8, cex.axis = 1.5,
     pcol = ins_col, lcol = "black")
plot(f2, ModTitle = "b) Insular - semi-log", cex.main = 2, cex.lab = 1.8, cex.axis = 1.5,
     pcol = ins_col, lcol = "black")
legend(-9, 210, legend=c("Nearctic", "Palearctic", "Indomalaya", "Afrotropic", 
                         "Oceania", "Australasia", "Neotropic"),
       col=Okabe_Ito[1:7], pch = 16, cex=1.8)

dev.off()

jpeg(file = "Threshold_best_models_mai_col.jpeg", width = 30, height = 15, units = "cm", res = 300)

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.8, 4.1, 2))
plot(f3, ModTitle = "a) Mainland - log-log", cex.main = 2, cex.lab = 1.8, cex.axis = 1.5,
     pcol = mai_col, lcol = "black")
plot(f4, ModTitle = "b) Mainland - semi-log", cex.main = 2, cex.lab = 1.8, cex.axis = 1.5,
     pcol = mai_col, lcol = "black")

dev.off()


###################################################################
############Mixed Effect Piecewise Models - segmented########################
##########################################################################

##segmented model without mixed effect
insular <- mutate(insular, log_area = log(Area))

l11 <- lm(Richness ~ log_area, data = insular)

s11 <-  segmented(l11, seg.Z = ~log_area, psi = 5)

##Mixed effect piecewise model
#source segmented.lme functions. n.b. these have been slightly edited
#to allow the plots to work
source("D:\\documents\\Work\\On-going projects\\Ant SARs\\source-segmented.lme.r")

#id is the grouping variable (Biogeo_realm for us)
insular <- mutate(insular, id = Biogeo_realm)

#create a log area column
insular <- mutate(insular, log_area = log(Area))

#ordered by realm
insular <- insular[order(insular$Biogeo_realm), ]

#fit a mixed effect model with random intercept by id (realm)
l1 <- lme(Richness ~ log_area, random=list(id = pdDiag(~1)),
          data = insular)
summary(l1)

#fit the segmented version (just random intercept)
s1 <- segmented.lme(l1, Z = log_area, 
                     random = list(id =  pdDiag(~1)))

#plot these model fits
jpeg(file = "Mixed_effect_segmented_int.jpeg", 
     width = 20, height = 20, units = "cm", res = 300)

#throws warnings about collapsing to unique values. This comes from
#splinefun inside the plot segmented lme function, where it
#is getting rid of duplicate x-values for plotting
plot(s1, n.plot=c(4,2), pop = TRUE, leg = "left", xlab = "Log(area)")

dev.off()


#fit the segmented version (random intercept & random threshold)
s2 <- segmented.lme(l1, Z = log_area, 
                    random = list(id =  pdDiag(~1 + G0)))

#plot model fits
jpeg(file = "Mixed_effect_segmented_int_BP.jpeg", 
     width = 20, height = 20, units = "cm", res = 300)

plot(s2, n.plot=c(4,2), pop = TRUE, leg = "left", xlab = "Log(area)")

dev.off()



##################################################
#########SAR_AVERAGE##########################
##############################################

#filter out all datasets with < 7 islands;
un <- unique(dat$Study_ID)

x <- vector(length = length(un))

for (i in 1:length(un)){
  
  dum <- filter(dat, Study_ID == un[i])
  x[i] <- nrow(dum)
}

w <- which(x < 7)
wr <- un[w]
wr2 <- which(dat$Study_ID %in% wr)
dat2 <- dat[-wr2,]#new dataset with all datasets >= 7 islands

##create blank lists for results
un2 <- unique(dat2$Study_ID)

#register parallel backend
cores = 11
cl = parallel::makeCluster(cores)
on.exit(parallel::stopCluster(cl))
doParallel::registerDoParallel(cl)

#run the sar average functions using exhaustive grid_start
#n.b. hash / un-hash the two lines if using or not using residual checks
                       
res_sel <- foreach(i=seq(from=1, to=length(un2), by=1),
                                           .errorhandling = c("pass"))  %dopar% {                       
                       
  library(dplyr)
  library(sars)
  
  dum <- filter(dat2, Study_ID == un2[i])
  dum2 <- select(dum, Area, Richness)
  
   sar_average(data = dum2, grid_start = "exhaustive", grid_n = 5000, 
               verb = FALSE, homoTest = "none", normaTest = "none")
     
  #  sar_average(data = dum2, grid_start = "exhaustive", grid_n = 5000, 
            #    verb = FALSE, normaTest = "lillie", homoTest = "cor.area",
             #  homoCor = "pearson")
}

##Process sar_average results and generate summary tables and plots

#create a large results matrix, one row for each dataset
res <- matrix(NA, ncol = 66, nrow = length(res_sel))
colnames(res) <- c("pow_rank", "pow_aic", "best_shape", "best_asym", "koba_rank", "monod_rank", "power_rank", "negexpo_rank", 
                   "linear_rank", "p2_rank", "epm2_rank", "loga_rank", "epm1_rank", "powerR_rank", 
                   "heleg_rank", "logistic_rank","weibull3_rank", "p1_rank", "asymp_rank", "chapman_rank", 
                   "gompertz_rank", "ratio_rank", "weibull4_rank", "betap_rank",
                   "koba_weight", "monod_weight", "power_weight", "negexpo_weight", "linear_weight", "p2_weight",      
                   "epm2_weight", "loga_weight", "epm1_weight", "powerR_weight", "heleg_weight", "logistic_weight",     
                   "weibull3_weight", "p1_weight", "asymp_weight", "chapman_weight", "gompertz_weight", 
                   "ratio_weight", "weibull4_weight", "betap_weight", 
                   "koba_shape", "monod_shape", "power_shape", "negexpo_shape", "linear_shape", "p2_shape",      
                   "epm2_shape", "loga_shape", "epm1_shape", "powerR_shape", "heleg_shape", "logistic_shape",     
                   "weibull3_shape", "p1_shape", "asymp_shape", "chapman_shape", "gompertz_shape", 
                   "ratio_shape", "weibull4_shape", "betap_shape", "Pow_Checks", "Nisl")

##iterate across the results list and extract model rank, weight, shape etc
for (j in 1:length(res_sel)){

r <- res_sel[[j]]

if (length(r[[1]]) == 1) next

sr <- summary(r)

#rank of power model 
pow_rank <- which(sr$Model_table$Model == "power")
if (length(pow_rank) == 0) pow_rank <- NA #for datasets where power model did not fit

#power model AICc weight
pow_aic <- sr$Model_table$Weight[pow_rank]
if (anyNA(pow_aic)) pow_aic <- NA

#best model shape
best_shape <- sr$Model_table$Shape[1]

#best model asymp
best_asym <- sr$Model_table$Asymptote[1]

res[j,1:4] <- c(pow_rank, pow_aic, best_shape, best_asym)

#rank for each individual model
mods <- c("koba" = NA, "monod" = NA, "power" = NA, "negexpo" = NA, 
"linear" = NA, "p2" = NA, "epm2" = NA, "loga" = NA, "epm1" = NA, "powerR" = NA, 
"heleg" = NA, "logistic" = NA,"weibull3" = NA, "p1" = NA, "asymp" = NA, "chapman" = NA, 
"gompertz" = NA, "ratio" = NA, "weibull4" = NA, "betap" = NA)

for (i in 1:nrow(sr$Model_table)){
  m <- match(sr$Model_table$Model[i], names(mods))
  mods[m] <- i
}

res[j,5:24] <- mods

#weight for each individual model
mods2 <- c("koba" = NA, "monod" = NA, "power" = NA, "negexpo" = NA, 
          "linear" = NA, "p2" = NA, "epm2" = NA, "loga" = NA, "epm1" = NA, "powerR" = NA, 
          "heleg" = NA, "logistic" = NA,"weibull3" = NA, "p1" = NA, "asymp" = NA, "chapman" = NA, 
          "gompertz" = NA, "ratio" = NA, "weibull4" = NA, "betap" = NA)

for (i in 1:nrow(sr$Model_table)){
  m <- match(sr$Model_table$Model[i], names(mods2))
  mods2[m] <- sr$Model_table$Weight[i]
}

res[j,25:44] <- mods2

#shape of each individual model
mods3 <- c("koba" = NA, "monod" = NA, "power" = NA, "negexpo" = NA, 
           "linear" = NA, "p2" = NA, "epm2" = NA, "loga" = NA, "epm1" = NA, "powerR" = NA, 
           "heleg" = NA, "logistic" = NA,"weibull3" = NA, "p1" = NA, "asymp" = NA, "chapman" = NA, 
           "gompertz" = NA, "ratio" = NA, "weibull4" = NA, "betap" = NA)

for (i in 1:nrow(sr$Model_table)){
  m <- match(sr$Model_table$Model[i], names(mods3))
  mods3[m] <- sr$Model_table$Shape[i]
}

res[j,45:64] <- mods3

#did power model pass validation checks
res[j,65] <- ifelse("Power" %in% res_sel[[j]]$details$mod_names, 1, 0)

#number of islands
res[j,66] <-  length(res_sel[[j]]$mmi)

}#eo j

res <- as.data.frame(res)

if (!all(res$pow_rank == res$power_rank, na.rm = TRUE)){
  stop("power model divergence")
}

#calculate mean rank and weight for each model across datasets
mod_plot <- apply(res[,5:44], 2, as.numeric) %>%
  apply(2, mean, na.rm = TRUE) %>%
  round(2)

#create df with model name and mean weight and rank
plot_df <- data.frame("Model" = names(mod_plot), "Values" = as.vector(mod_plot),
                      "Type" = c(rep("Rank", 20), c(rep("Weight", 20))))

#shorten to just model names
plot_df$Model <- gsub("_rank", "", plot_df$Model)
plot_df$Model <- gsub("_weight", "", plot_df$Model)

#create a wide df to make two plots in ggplot
plot_df_w <- pivot_wider(plot_df, names_from = Type, values_from = Values)

#plot mean ranks ordering by increasing mean rank. Horizontal bars
g1 <- ggplot(plot_df_w) + geom_col(aes(reorder(Model, -Rank), Rank)) + coord_flip() +
  theme_bw() + ylab("Mean Rank") + xlab("Model") + ggtitle("a) Mean Rank - No Residual Checks") +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14),
        plot.title = element_text(size=14))

#plot mean Aicc Weight ordering by decreasing mean weight. Horizontal bars
g2 <- ggplot(plot_df_w) + geom_col(aes(reorder(Model, Weight), Weight)) + coord_flip() +
  theme_bw() + ylab("Mean AICc Weight") + xlab("Model") + 
  ggtitle("b) Mean AICc Weight - No Residual Checks")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=14))

jpeg("multi_model_bars_AP.jpeg", width = 30, height = 15, units = "cm", res= 300)

gridExtra::grid.arrange(g1, g2, nrow = 1)

dev.off()



