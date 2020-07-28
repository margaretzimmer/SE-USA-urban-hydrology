closeAllConnections()
rm(list=ls())

#install.packages("measurements")
#install.packages("rcompanion")
#library(vegan)
#library(fitdistrplus)
#library(gclus)
library(leaps)
library(rcompanion)
#library(MASS)
library(measurements) 
library(lmtest)
library(ggplot2)

#flow <- read.csv(file = "mean_of_means_annual_flow_metrics_3Sep19.csv")
flow <- read.csv(file = "normalized_flowcv_and_notnormalizedcv_4May20.csv")
dat <- read.csv(file = "parameters_flowmetrics_pareddown_9May20.csv")

#aa <- (dat$BFI_avg - mean(dat$BFI_avg))/sd(dat$BFI_avg)
#dat$BFI_avg <- aa

#bb <- (dat$fall_sum - mean(dat$fall_sum))/sd(dat$fall_sum)
#dat$fall_sum <- bb

#Tukey transformation makes this normal
cc <- transformTukey(dat$summer_sum)
ncc <- (cc-mean(cc))/sd(cc)
dat$summer_sum <- ncc

#dd <- (dat$winter_sum - mean(dat$winter_sum))/sd(dat$winter_sum)
#dat$winter_sum <- dd

ee <- (dat$totalppt_2004_2018_cm - mean(dat$totalppt_2004_2018_cm))/sd(dat$totalppt_2004_2018_cm)
dat$totalppt_2004_2018_cm <- ee

a<-transformTukey(dat$BasinGIS_km2)
na <- (a-mean(a))/sd(a)
dat$BasinGIS_km2 <- na

b <- transformTukey(dat$r50_DevelopedOpen2011)
nb <- (b-mean(b))/sd(b)
dat$r50_DevelopedOpen2011 <- nb

###Tukey does a better job- still not normal, but less not normal
#b0 <- transformTukey(dat$r50_DevelopedLow2011)
#b1 <- (b0-mean(b0))/sd(b0)
#dat$r50_DevelopedLow2011 <- b1

#######b1 <- log(dat$DevelopedLow2011+1)
#######b2 <- (b1-mean(b1))/sd(b1)
#######dat$DevelopedLow2011 <- b2

##Tukey does a better job
b3 <- transformTukey(dat$r50_DeciduousForest2011)
b4 <- (b3-mean(b3))/sd(b3)
dat$r50_DeciduousForest2011 <- b4

#Tukey makes it less not normal
#b5 <- transformTukey(dat$r50_UrbanLMH2011)
#b6 <- (b5-mean(b5))/sd(b5)
#dat$r50_UrbanLMH2011 <- b6

#Tukey is closer to normal
b7 <- transformTukey(dat$r50_UrbanTotal2011)
b8 <- (b7-mean(b7))/sd(b7)
dat$r50_UrbanTotal2011 <-b8

#Tukey is normal, log+1 isn't
b9 <- transformTukey(dat$r50_ForestTotal2011)
b10 <- (b9-mean(b9))/sd(b9)
dat$r50_ForestTotal2011 <-b10

#Tukey is closer to normal
#b11 <- transformTukey(dat$r50_WetlandsTotal2011)
#b12 <- (b11-mean(b11))/sd(b11)
#dat$r50_WetlandsTotal2011 <- b12

c <- transformTukey(dat$r50_CanopyCover2011)
nc <- (c-mean(c))/sd(c)
dat$r50_CanopyCover2011 <- nc

#Tukey makes it normal
c1 <- transformTukey(dat$r50_Imperviousness2011)
c2 <- (c1-mean(c1))/sd(c1)
dat$r50_Imperviousness2011 <-c2

c0 <- (dat$DevelopedOpen2011- mean(dat$DevelopedOpen2011))/sd(dat$DevelopedOpen2011)
dat$DevelopedOpen2011 <- c0

d <- transformTukey(dat$DevelopedLow2011)
nd <- (d-mean(d))/sd(d)
dat$DevelopedLow2011 <- nd

e<-transformTukey(dat$DevelopedMed2011)
ne <- (e-mean(e))/sd(e)
dat$DevelopedMed2011 <- ne

f <- transformTukey(dat$DevelopedHigh2011)
nf <- (f-mean(f))/sd(f)
dat$DevelopedHigh2011 <- nf

g <- transformTukey(dat$DeciduousForest2011)
ng <- (g-mean(g))/sd(g)
dat$DeciduousForest2011 <- ng

h <- transformTukey(dat$EvergreenForest2011)
nh <- (h-mean(h))/sd(h)
dat$EvergreenForest2011 <- nh

ii <- transformTukey(dat$ShrubScrub2011)
nii <- (ii-mean(ii))/sd(ii)
dat$ShrubScrub2011 <- nii

j <- transformTukey(dat$GrasslandHerb2011)
nj <- (j-mean(j))/sd(j)
dat$GrasslandHerb2011 <- nj

#Tukey makes it less not-normal
#jj <- transformTukey(dat$PastureHay2011)
#njj <- (jj-mean(jj))/sd(jj)
#dat$PastureHay2011<-njj

k <- transformTukey(dat$WoodyWetlands2011)
nk <- (k-mean(k))/sd(k)
dat$WoodyWetlands2011 <- nk

#Tukey makes it less not normal
kk <- transformTukey(dat$UrbanLMH2011)
nkk <- (kk-mean(kk))/sd(kk)
dat$UrbanLMH2011 <- nkk

#kk2 <- (dat$UrbanTotal2011- mean(dat$UrbanTotal2011))/sd(dat$UrbanTotal2011)
#dat$UrbanTotal2011 <- kk2

#Tukey makes it less not normal
#k1 <- transformTukey(dat$AgricultureTotal2011)
#k2 <- (k1-mean(k1))/sd(k1)
#dat$AgricultureTotal2011 <-k2

l <- transformTukey(dat$ForestTotal2011)
nl <- (l-mean(l))/sd(l)
dat$ForestTotal2011 <- nl

m <- transformTukey(dat$WetlandsTotal2011)
nm <- (m-mean(m))/sd(m)
dat$WetlandsTotal2011 <- nm

mm <- (dat$Canopy2011-mean(dat$Canopy2011))/sd(dat$Canopy2011)
dat$Canopy2011 <- mm

n <- transformTukey(dat$Imperviousness2011)
nn <- (n-mean(n))/sd(n)
dat$Imperviousness2011 <- nn

o <- transformTukey(dat$NatWetlandsInvTotal)
no <- (o-mean(o))/sd(o)
dat$NatWetlandsInvTotal <- no

p <- transformTukey(dat$PopDensityBLK2010)
np <- (p-mean(p))/sd(p)
dat$PopDensityBLK2010 <- np

##Tukey makes it less not normal
pp <- transformTukey(dat$HousDensityBLK2010)
npp <- (pp-mean(pp))/sd(pp)
dat$HousDensityBLK2010 <- npp

ppp <- (dat$RoadDensity2014 - mean(dat$RoadDensity2014))/sd(dat$RoadDensity2014)
dat$RoadDensity2014 <- ppp

s <- transformTukey(dat$StreamDensity)
ns <- (s-mean(s))/sd(s)
dat$StreamDensity <- ns

t<- transformTukey(dat$Slope)
nt <- (t-mean(t))/sd(t)
dat$Slope <- nt

#Tukey makes it less not normal
#tt <- transformTukey(dat$Permeability)
#ntt <- (tt-mean(tt))/sd(tt)
#dat$Permeability <- ntt

u<-transformTukey(dat$SandContent)
nu <- (u-mean(u))/sd(u)
dat$SandContent <- nu

#Tukey makes it less not normal
#uu <- transformTukey(dat$WWTP.SEWER_DESIGNFLOW_MGD)
#nuu <- (uu-mean(uu))/sd(uu)
#dat$WWTP.SEWER_DESIGNFLOW_MGD <- nuu

v<-transformTukey(dat$PctHydrGrpB)
nv <- (v-mean(v))/sd(v)
dat$PctHydrGrpB <- nv

w <- transformTukey(dat$PctHydrGrpB_D)
nw <- (w-mean(w))/sd(w)
dat$PctHydrGrpB_D <- nw

x <- transformTukey(dat$PctHydrGrpC)
nx <- (x-mean(x))/sd(x)
dat$PctHydrGrpC <- nx

y <- transformTukey(dat$PctHydrGrpD)
ny <- (y-mean(y))/sd(y)
dat$PctHydrGrpD <- ny

z <- transformTukey(dat$PctHydrGrpD_A_D_B_D_C_D)
nz <- (z-mean(z))/sd(z)
dat$PctHydrGrpD_A_D_B_D_C_D <- nz

x <- dat[,38]
rosnerTest(x, k=1,warn=F)

d <- dat[,4:35]
d <- d[,-24]

names(d)[names(d) == "Slope"] <- "TopographicGradient"
names(d)[names(d) == "summer_sum"] <- "MeanSummerPrecipitation"
names(d)[names(d) == "totalppt_2004_2018_cm"] <- "MeanAnnualPrecipitation"


d.cor <- cor(d)
d.rcorr <- rcorr(as.matrix(d))


pdf("correlation_matrix_14May20a.pdf")
corrplot(d.cor, tl.col = "black",tl.cex = 0.7)
dev.off()

tiff("correlation_matrix_14May20a.tiff")
corrplot(d.cor, tl.col = "black",tl.cex = 0.7)
dev.off()


#### CALCULATE OLS####
#Set explanatory and flowmetric variables first from list above
#m1=lm(flowmetric~explanatory)
#summary(m1)
#plot(flowmetric~explanatory)
#res=residuals(m1)
#pred=predict(m1)

# pred vs. actual Y- looking for a symmetrical distribution around the 45 deg line
#plot(pred~flowmetric)

#Breusch-Pagan test for heteroskedasiticity. If p-value is less than 0.05, reject the null and heteroskedasticity is present
#bptest(m1)

#test residuals are normal
#shapiro.test(res)

#get slope
#coefficients(m1)


#write.csv(dat, file="norm_data_17Oct19.csv")

#now make dataframes with the flow metric and significant explanatory variables

### mean median flow ###
#removed permeability 5/9/20
explan_Med <- data.frame(cbind(dat$Slope, dat$totalppt_2004_2018_cm, dat$PctHydrGrpC, dat$BasinGIS_km2,
                                   dat$StreamDensity, dat$PctHydrGrpB_D))
colnames(explan_Med) <- c("Slope", "totalppt_2004_2018_cm", "PctHydrGrpC", "BasinGIS_km2",
                               "StreamDensity", "PctHydrGrpB_D")

#L_med=leaps(x=explan_Med,y=flow$Median.Flow,method="adjr2",nbest=4,names=c("Slope", "totalppt_2004_2018_cm", "PctHydrGrpC", "BasinGIS_km2",
#                                                                                               "Permeability", "StreamDensity", "PctHydrGrpB_D"))

#write.csv(cbind(L_med$which,L_med$adjr2),"2008MedianFlow_leaps.csv")

expln_med <- regsubsets(flow$norm_median~., data=explan_Med,nvmax = 5,method="exhaustive")
med.sum <- summary(expln_med)
#names(meanmed.sum)
med.sum$rsq
med.sum$cp
med.sum$bic
med.sum$adjr2
plot(expln_med, scale = "adjr2") 
coef(expln_med,3)

######

##mean 70th

#took out winter precip 5/9/20
explan_70th <- data.frame(cbind(dat$Slope, dat$totalppt_2004_2018_cm,
                                    dat$PctHydrGrpC, dat$PctHydrGrpB_D, dat$PctHydrGrpD_A_D_B_D_C_D))
colnames(explan_70th) <- c("Slope", "totalppt_2004_2018_cm",  "PctHydrGrpC",
                              "PctHydrGrpB_D", "PctHydrGrpD_A_D_B_D_C_D")

#L_70th=leaps(x=explan_70th,y=flow$X70th.exceed.flow,method="adjr2",nbest=4,names=c("Slope", "totalppt_2004_2018_cm", "Winter Precip", "PctHydrGrpC","PctHydrGrpB/D", "PctHydrGrpB_D"))

expln_70th <- regsubsets(flow$norm_70~., data=explan_70th,nvmax = 5,method="exhaustive")
sum.70th <- summary(expln_70th)
#names(meanmed.sum)
sum.70th$rsq
sum.70th$cp
sum.70th$bic
sum.70th$adjr2
plot(expln_70th, scale = "adjr2") 
coef(expln_70th,3)

#write.csv(cbind(L_mean70th$which,L_mean70th$adjr2),"Mean70thFlow_leaps.csv")



########
## mean 90th
explan_90th <- data.frame(cbind(dat$Slope, dat$totalppt_2004_2018_cm,
                                    dat$PctHydrGrpB_D, dat$PctHydrGrpD_A_D_B_D_C_D))
colnames(explan_90th) <- c("Slope", "totalppt_2004_2018_cm",
                               "PctHydrGrpB_D", "PctHydrGrpD_A_D_B_D_C_D")

#L_90th=leaps(x=explan_90th,y=flow$X90th.exceed.flow,method="adjr2",nbest=4,names=c("Slope", "totalppt_2004_2018_cm", "PctHydrGrpB/D", "PctHydrGrpB_D"))
                                                                                               
expln_90th <- regsubsets(flow$norm_90~., data=explan_90th,nvmax = 4,method="exhaustive")
sum.90th <- summary(expln_90th)
#names(meanmed.sum)
sum.90th$rsq
sum.90th$cp
sum.90th$bic
sum.90th$adjr2
plot(expln_90th, scale = "adjr2") 
coef(expln_90th,3)

#write.csv(cbind(L_mean90th$which,L_mean90th$adjr2),"Mean90thFlow_leaps.csv")
########
## mean 99th
explan_99th <- data.frame(cbind(dat$Slope, dat$totalppt_2004_2018_cm,dat$PctHydrGrpB_D, dat$PctHydrGrpC,dat$PctHydrGrpD_A_D_B_D_C_D))
colnames(explan_99th) <- c("Slope", "totalppt_2004_2018_cm","PctHydrGrpB_D", "PctHydrGrpC", "PctHydrGrpD_A_D_B_D_C_D")

#L_99th=leaps(x=explan_99th,y=flow$X99th.exceed.flow,method="adjr2",nbest=4,names=c("Slope", "totalppt_2004_2018_cm", "PctHydrGrpB/D", "PctHydrGrpC", "PctHydrGrpB_D"))


expln_99th <- regsubsets(flow$norm_99~., data=explan_99th,nvmax = 5,method="exhaustive")
sum.99th <- summary(expln_99th)
#names(meanmed.sum)
sum.99th$rsq
sum.99th$cp
sum.99th$bic
sum.99th$adjr2
plot(expln_99th, scale = "adjr2") 
coef(expln_99th,3)

#write.csv(cbind(L_99th$which,L_99th$adjr2),"Mean99thFlow_leaps.csv")
########
## mean BFI
#removed fall precip, urban total, ag total, r 50 urban lmh  5/9/20
explan_BFI <- data.frame(cbind(dat$ShrubScrub2011, dat$ForestTotal2011, 
                                   dat$DevelopedOpen2011, dat$DeciduousForest2011,  dat$GrasslandHerb2011,
                                   dat$r50_DevelopedOpen2011, dat$PopDensityBLK2010, dat$SandContent,
                                   dat$PctHydrGrpB, dat$BasinGIS_km2, dat$r50_UrbanTotal2011, dat$totalppt_2004_2018_cm,
                                   dat$DevelopedLow2011, dat$Imperviousness2011, dat$RoadDensity2014, dat$EvergreenForest2011))
                            
colnames(explan_BFI) <- c("ShrubScrub2011","ForestTotal2011",
                              "DevelopedOpen2011","DeciduousForest2011","GrasslandHerb2011",
                              "50_DevelopedOpen2011","PopDensityBLK2010","SandContent",
                              "PctHydrGrpB","BasinGIS_km2","r50_UrbanTotal2011","totalppt_2004_2018_cm",
                              "DevelopedLow2011","Imperviousness2011","RoadDensity2014","EvergreenForest2011")

explan_BFI_noLMH <- data.frame(cbind(dat$ShrubScrub2011, dat$UrbanTotal2011, dat$ForestTotal2011, dat$fall_sum,
                               dat$DevelopedOpen2011, dat$DeciduousForest2011, dat$AgricultureTotal2011, dat$GrasslandHerb2011,
                               dat$r50_DevelopedOpen2011, dat$PopDensityBLK2010, dat$SandContent,
                               dat$PctHydrGrpB, dat$BasinGIS_km2, dat$r50_UrbanTotal2011, dat$totalppt_2004_2018_cm,
                               dat$DevelopedLow2011, dat$Imperviousness2011, dat$RoadDensity2014, dat$EvergreenForest2011))

colnames(explan_BFI_noLMH) <- c("ShrubScrub2011","UrbanTotal2011","ForestTotal2011","fall_sum",
                          "DevelopedOpen2011","DeciduousForest2011","AgricultureTotal2011","GrasslandHerb2011",
                          "50_DevelopedOpen2011","PopDensityBLK2010","SandContent",
                          "PctHydrGrpB","BasinGIS_km2","r50_UrbanTotal2011","totalppt_2004_2018_cm",
                          "DevelopedLow2011","Imperviousness2011","RoadDensity2014","EvergreenForest2011")

explan_BFI_noimp <- data.frame(cbind(dat$ShrubScrub2011, dat$UrbanTotal2011, dat$ForestTotal2011, dat$fall_sum,
                               dat$DevelopedOpen2011, dat$DeciduousForest2011, dat$AgricultureTotal2011, dat$GrasslandHerb2011,
                               dat$r50_DevelopedOpen2011, dat$PopDensityBLK2010, dat$SandContent,
                               dat$PctHydrGrpB, dat$BasinGIS_km2, dat$r50_UrbanTotal2011, dat$UrbanLMH2011, dat$totalppt_2004_2018_cm,
                               dat$DevelopedLow2011,dat$RoadDensity2014, dat$EvergreenForest2011))

colnames(explan_BFI_noimp) <- c("ShrubScrub2011","UrbanTotal2011","ForestTotal2011","fall_sum",
                          "DevelopedOpen2011","DeciduousForest2011","AgricultureTotal2011","GrasslandHerb2011",
                          "50_DevelopedOpen2011","PopDensityBLK2010","SandContent",
                          "PctHydrGrpB","BasinGIS_km2","r50_UrbanTotal2011","UrbanLMH2011","totalppt_2004_2018_cm",
                          "DevelopedLow2011","RoadDensity2014","EvergreenForest2011")
                             

#L_BFI=leaps(x=explan_BFI,y=flow$BFI,method="adjr2",nbest=4,names=c("ShrubScrub2011","UrbanTotal2011","ForestTotal2011","fall_sum",
#                                                                                      "DevelopedOpen2011","DeciduousForest2011","AgricultureTotal2011","GrasslandHerb2011",
#                                                                                      "50_DevelopedOpen2011","PopDensityBLK2010","SandContent",
 #                                                                                     "PctHydrGrpB","BasinGIS_km2","r50_UrbanTotal2011","UrbanLMH2011","totalppt_2004_2018_cm",
#                                                                                      "DevelopedLow2011","Imperviousness2011","RoadDensity2014","EvergreenForest2011"))
                                                                                      

expln_BFI <- regsubsets(flow$norm_BFI~., data=explan_BFI,nvmax = 5,method="exhaustive")
BFI.sum <- summary(expln_BFI)
names(BFI.sum)
BFI.sum$rsq
BFI.sum$cp
BFI.sum$bic
BFI.sum$adjr2
plot(expln_BFI, scale = "adjr2") 
coef(expln_BFI,4)

#write.csv(cbind(L_meanBFI$which,L_meanBFI$adjr2),"MeanBFIFlow_leaps.csv")

## mean ANnual Baseflow
#removed 5/9/20 fall precip, urban total, ag, r50 wetlands
explan_Baseflow <- data.frame(cbind(dat$BasinGIS_km2,dat$totalppt_2004_2018_cm,
                                        dat$ShrubScrub2011,dat$r50_DevelopedOpen2011,dat$Slope,
                                        dat$DevelopedOpen2011,dat$r50_UrbanTotal2011,dat$GrasslandHerb2011,
                                        dat$WetlandsTotal2011,dat$WoodyWetlands2011,
                                        dat$ForestTotal2011,dat$PopDensityBLK2010,
                                        dat$HousDensityBLK2010,dat$DevelopedLow2011,dat$DeciduousForest2011,
                                        dat$EvergreenForest2011,dat$r50_ForestTotal2011,
                                        dat$RoadDensity2014,dat$PctHydrGrpC))
  
colnames(explan_Baseflow) <- c("BasinGIS_km2","totalppt_2004_2018_cm",
                                         "ShrubScrub2011","r50_DevelopedOpen2011","Slope",
                                         "DevelopedOpen2011","r50_UrbanTotal2011","GrasslandHerb2011",
                                         "WetlandsTotal2011","WoodyWetlands2011",
                                         "ForestTotal2011","PopDensityBLK2010",
                                         "HousDensityBLK2010","DevelopedLow2011","DeciduousForest2011",
                                         "EvergreenForest2011","r50_ForestTotal2011",
                                         "RoadDensity2014","PctHydrGrpC")

explan_Baseflow_nopop <- data.frame(cbind(dat$BasinGIS_km2,dat$totalppt_2004_2018_cm,dat$fall_sum,
                                    dat$ShrubScrub2011,dat$r50_DevelopedOpen2011,dat$Slope,
                                    dat$DevelopedOpen2011,dat$r50_UrbanTotal2011,dat$GrasslandHerb2011,
                                    dat$UrbanTotal2011,dat$WetlandsTotal2011,dat$WoodyWetlands2011,
                                    dat$ForestTotal2011,dat$AgricultureTotal2011,
                                    dat$HousDensityBLK2010,dat$DevelopedLow2011,dat$DeciduousForest2011,
                                    dat$EvergreenForest2011,dat$r50_WetlandsTotal2011,dat$r50_ForestTotal2011,
                                    dat$RoadDensity2014,dat$PctHydrGrpC))

colnames(explan_Baseflow_nopop) <- c("BasinGIS_km2","totalppt_2004_2018_cm","fall_sum",
                               "ShrubScrub2011","r50_DevelopedOpen2011","Slope",
                               "DevelopedOpen2011","r50_UrbanTotal2011","GrasslandHerb2011",
                               "UrbanTotal2011","WetlandsTotal2011","WoodyWetlands2011",
                               "ForestTotal2011","AgricultureTotal2011",
                               "HousDensityBLK2010","DevelopedLow2011","DeciduousForest2011",
                               "EvergreenForest2011","r50_WetlandsTotal2011","r50_ForestTotal2011",
                               "RoadDensity2014","PctHydrGrpC")

#removed fall precip, ag, r50 wetlands,urban total
explan_Baseflow_nohs <- data.frame(cbind(dat$BasinGIS_km2,dat$totalppt_2004_2018_cm,
                                    dat$ShrubScrub2011,dat$r50_DevelopedOpen2011,dat$Slope,
                                    dat$DevelopedOpen2011,dat$r50_UrbanTotal2011,dat$GrasslandHerb2011,
                                    dat$WetlandsTotal2011,dat$WoodyWetlands2011,
                                    dat$ForestTotal2011,dat$PopDensityBLK2010,
                                    dat$DevelopedLow2011,dat$DeciduousForest2011,
                                    dat$EvergreenForest2011,dat$r50_ForestTotal2011,
                                    dat$RoadDensity2014,dat$PctHydrGrpC))

colnames(explan_Baseflow_nohs) <- c("BasinGIS_km2","totalppt_2004_2018_cm",
                               "ShrubScrub2011","r50_DevelopedOpen2011","Slope",
                               "DevelopedOpen2011","r50_UrbanTotal2011","GrasslandHerb2011",
                               "WetlandsTotal2011","WoodyWetlands2011",
                               "ForestTotal2011","PopDensityBLK2010",
                               "DevelopedLow2011","DeciduousForest2011",
                               "EvergreenForest2011","r50_ForestTotal2011",
                               "RoadDensity2014","PctHydrGrpC")

explan_Baseflow_nopopDL <- data.frame(cbind(dat$BasinGIS_km2,dat$totalppt_2004_2018_cm,dat$fall_sum,
                                          dat$ShrubScrub2011,dat$r50_DevelopedOpen2011,dat$Slope,
                                          dat$DevelopedOpen2011,dat$r50_UrbanTotal2011,dat$GrasslandHerb2011,
                                          dat$UrbanTotal2011,dat$WetlandsTotal2011,dat$WoodyWetlands2011,
                                          dat$ForestTotal2011,dat$AgricultureTotal2011,
                                          dat$HousDensityBLK2010,dat$DeciduousForest2011,
                                          dat$EvergreenForest2011,dat$r50_WetlandsTotal2011,dat$r50_ForestTotal2011,
                                          dat$RoadDensity2014,dat$PctHydrGrpC))

colnames(explan_Baseflow_nopopDL) <- c("BasinGIS_km2","totalppt_2004_2018_cm","fall_sum",
                                     "ShrubScrub2011","r50_DevelopedOpen2011","Slope",
                                     "DevelopedOpen2011","r50_UrbanTotal2011","GrasslandHerb2011",
                                     "UrbanTotal2011","WetlandsTotal2011","WoodyWetlands2011",
                                     "ForestTotal2011","AgricultureTotal2011",
                                     "HousDensityBLK2010","DeciduousForest2011",
                                     "EvergreenForest2011","r50_WetlandsTotal2011","r50_ForestTotal2011",
                                     "RoadDensity2014","PctHydrGrpC")

#L_Baseflow=leaps(x=explan_Baseflow,y=flow$sumBaseflow,method="adjr2",nbest=4,names=c("BasinGIS_km2","totalppt_2004_2018_cm","fall_sum",
#                                                                                                    "ShrubScrub2011","r50_DevelopedOpen2011","Slope",
 #                                                                                                   "DevelopedOpen2011","r50_UrbanTotal2011","GrasslandHerb2011",
 #                                                                                                   "UrbanTotal2011","WetlandsTotal2011","WoodyWetlands2011",
 #                                                                                                   "ForestTotal2011","AgricultureTotal2011","PopDensityBLK2010",
#                                                                                                    "HousDensityBLK2010","DevelopedLow2011","DeciduousForest2011",
#                                                                                                    "EvergreenForest2011","r50_WetlandsTotal2011","r50_ForestTotal2011",
#                                                                                                    "RoadDensity2014","PctHydrGrpC"))


expln_base <- regsubsets(flow$norm_base~., data=explan_Baseflow_nohs,nvmax = 5,method="exhaustive")
base.sum <- summary(expln_base)
#names(meanmed.sum)
base.sum$rsq
base.sum$cp
base.sum$bic
base.sum$adjr2
plot(expln_base, scale = "adjr2") 
coef(expln_base,5)

#write.csv(cbind(L_meanBaseflow$which,L_meanBaseflow$adjr2),"MeanBaseflowFlow_leaps.csv")
####


##for multiyear calcs:

####
## CV of 50th
explan_CV50th <- data.frame(cbind(dat$Imperviousness2011,dat$DevelopedMed2011,dat$UrbanLMH2011,
                                  dat$RoadDensity2014,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                  dat$PopDensityBLK2010,dat$DevelopedHigh2011,dat$ForestTotal2011,
                                  dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))

explan_CV50th_noimp <- data.frame(cbind(dat$DevelopedMed2011,dat$UrbanLMH2011,
                                        dat$RoadDensity2014,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                        dat$PopDensityBLK2010,dat$DevelopedHigh2011,dat$ForestTotal2011,
                                        dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))

explan_CV50th_noimp_nodevmed <- data.frame(cbind(dat$UrbanLMH2011,
                                        dat$RoadDensity2014,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                        dat$PopDensityBLK2010,dat$DevelopedHigh2011,dat$ForestTotal2011,
                                        dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))

explan_CV50th_noimp_nodevmed_nopop <- data.frame(cbind(dat$UrbanLMH2011,
                                                 dat$RoadDensity2014,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                                 dat$DevelopedHigh2011,dat$ForestTotal2011,
                                                 dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))
explan_CV50th_noimp_nodevmed_nopop_noroad <- data.frame(cbind(dat$UrbanLMH2011,
                                                       dat$DevelopedLow2011,dat$UrbanTotal2011,
                                                       dat$DevelopedHigh2011,dat$ForestTotal2011,
                                                       dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))
explan_CV50th_noimp_nodevmed_nopop_noroad_nolmh <- data.frame(cbind(dat$DevelopedLow2011,dat$UrbanTotal2011,
                                                              dat$DevelopedHigh2011,dat$ForestTotal2011,
                                                              dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))

explan_CV50th_noimp_nodevmed_nopop_noroad_nolmh_noag <- data.frame(cbind(dat$DevelopedLow2011,dat$UrbanTotal2011,
                                                                    dat$DevelopedHigh2011,dat$ForestTotal2011,
                                                                    dat$EvergreenForest2011,dat$GrasslandHerb2011))



explan_CV50th_nodhigh <- data.frame(cbind(dat$Imperviousness2011,dat$DevelopedMed2011,dat$UrbanLMH2011,
                                          dat$RoadDensity2014,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                          dat$PopDensityBLK2010,dat$ForestTotal2011,
                                          dat$EvergreenForest2011,dat$GrasslandHerb2011,dat$AgricultureTotal2011))

explan_CV50th_onlyforest_ever_grass <- data.frame(cbind(dat$ForestTotal2011,
                                          dat$EvergreenForest2011,dat$GrasslandHerb2011))


colnames(explan_CV50th) <- c("Imperviousness2011","DevelopedMed2011","UrbanLMH2011",
                             "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                             "PopDensityBLK2010","DevelopedHigh2011","ForestTotal2011",
                             "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")

colnames(explan_CV50th_noimp) <- c("DevelopedMed2011","UrbanLMH2011",
                                   "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                                   "PopDensityBLK2010","DevelopedHigh2011","ForestTotal2011",
                                   "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")

colnames(explan_CV50th_noimp_nodevmed) <- c("UrbanLMH2011",
                                   "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                                   "PopDensityBLK2010","DevelopedHigh2011","ForestTotal2011",
                                   "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")


colnames(explan_CV50th_noimp_nodevmed_nopop) <- c("UrbanLMH2011",
                                            "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                                            "DevelopedHigh2011","ForestTotal2011",
                                            "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")


colnames(explan_CV50th_noimp_nodevmed_nopop_noroad) <- c("UrbanLMH2011",
                                                  "DevelopedLow2011","UrbanTotal2011",
                                                  "DevelopedHigh2011","ForestTotal2011",
                                                  "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")

colnames(explan_CV50th_noimp_nodevmed_nopop_noroad_nolmh_noag) <- c("DevelopedLow2011","UrbanTotal2011",
                                                         "DevelopedHigh2011","ForestTotal2011",
                                                         "EvergreenForest2011","GrasslandHerb2011")

colnames(explan_CV50th_onlyforest_ever_grass) <- c("ForestTotal2011","EvergreenForest2011","GrasslandHerb2011")

colnames(explan_CV50th_nodhigh) <- c("Imperviousness2011","DevelopedMed2011","UrbanLMH2011",
                                     "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                                     "PopDensityBLK2010","ForestTotal2011",
                                     "EvergreenForest2011","GrasslandHerb2011","Agriculture2011")

L_CV50th=leaps(x=explan_CV50th,y=flow$CVMedianAnnualFlow,method="adjr2",nbest=4,names=c("Imperviousness2011","DevelopedMed2011","UrbanLMH2011",
                                                                                        "RoadDensity2014","DevelopedLow2011","UrbanTotal2011",
                                                                                        "PopDensityBLK2010","DevelopedHigh2011","ForestTotal2011",
                                                                                        "EvergreenForest2011","GrasslandHerb2011"))

expln_CV50th <- regsubsets(flow$CVMedianAnnualFlow~., data=explan_CV50th_onlyforest_ever_grass,nvmax = 5,method="exhaustive")
CV50th.sum <- summary(expln_CV50th)
names(CV50th.sum)
CV50th.sum$rsq
plot(expln_CV50th, scale = "adjr2") 
coef(expln_CV50th,3)




write.csv(cbind(L_CV50th$which,L_CV50th$adjr2),"CV50th_leaps.csv")
####
## CV of 70th
explan_CV70th <- data.frame(cbind(dat$Canopy2011,dat$DevelopedMed2011,dat$Imperviousness2011,
                                  dat$RoadDensity2014,dat$UrbanLMH2011,dat$DevelopedHigh2011,
                                  dat$PopDensityBLK2010,dat$DevelopedLow2011,dat$UrbanTotal2011,
                                  dat$HousDensityBLK2010,dat$ForestTotal2011,dat$EvergreenForest2011,
                                  dat$DeciduousForest2011,dat$AgricultureTotal2011))

explan_CV70th <- data.frame(cbind(dat$Canopy2011,dat$UrbanTotal2011,dat$EvergreenForest2011))

colnames(explan_CV70th) <- c("Canopy2011","DevelopedMed2011","Imperviousness2011",
                             "RoadDensity2014","UrbanLMH2011","DevelopedHigh2011",
                             "PopDensityBLK2010","DevelopedLow2011","UrbanTotal2011",
                             "HousDensityBLK2010","ForestTotal2011","EvergreenForest2011",
                             "DeciduousForest2011","Agriculture")

colnames(explan_CV70th) <- c("Canopy2011","UrbanTotal2011",
                             "EvergreenForest2011")

L_CV70th=leaps(x=explan_CV70th,y=flow$CV70thAnnualFlow,method="adjr2",nbest=4,names=c("Canopy2011","DevelopedMed2011","Imperviousness2011",
                                                                                      "RoadDensity2014","UrbanLMH2011","DevelopedHigh2011",
                                                                                      "PopDensityBLK2010","DevelopedLow2011","UrbanTotal2011",
                                                                                      "HousDensityBLK2010","ForestTotal2011","EvergreenForest2011",
                                                                                      "DeciduousForest2011"))

expln_CV70th <- regsubsets(flow$CV70thAnnualFlow~., data=explan_CV70th,nvmax = 5,method="exhaustive")
CV70th.sum <- summary(expln_CV70th)
names(CV70th.sum)
CV70th.sum$rsq
plot(expln_CV70th, scale = "adjr2") 
coef(expln_CV70th,3)

write.csv(cbind(L_CV70th$which,L_CV70th$adjr2),"CV70th_leaps.csv")
####
## CV of 90th
explan_CV90th <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$Imperviousness2011,
                                  dat$DevelopedHigh2011,dat$RoadDensity2014,dat$UrbanLMH2011,
                                  dat$DevelopedLow2011,dat$UrbanTotal2011))



colnames(explan_CV90th) <- c("DevelopedMed2011","Canopy2011","Imperviousness2011",
                             "DevelopedHigh2011","RoadDensity2014","UrbanLMH2011",
                             "DevelopedLow2011","UrbanTotal2011")

explan_CV90th_noimp <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,
                                        dat$DevelopedHigh2011,dat$RoadDensity2014,dat$UrbanLMH2011,
                                        dat$DevelopedLow2011,dat$UrbanTotal2011))

colnames(explan_CV90th_noimp) <- c("DevelopedMed2011","Canopy2011",
                                   "DevelopedHigh2011","RoadDensity2014","UrbanLMH2011",
                                   "DevelopedLow2011","UrbanTotal2011")

explan_CV90th_nourban <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$Imperviousness2011,
                                          dat$DevelopedHigh2011,dat$RoadDensity2014,dat$UrbanLMH2011,
                                          dat$DevelopedLow2011))

colnames(explan_CV90th_nourban) <- c("DevelopedMed2011","Canopy2011","Imperviousness2011",
                                     "DevelopedHigh2011","RoadDensity2014","UrbanLMH2011",
                                     "DevelopedLow2011")

explan_CV90th_nolmh <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$Imperviousness2011,
                                        dat$DevelopedHigh2011,dat$RoadDensity2014,
                                        dat$DevelopedLow2011,dat$UrbanTotal2011))

colnames(explan_CV90th_nolmh) <- c("DevelopedMed2011","Canopy2011","Imperviousness2011",
                                   "DevelopedHigh2011","RoadDensity2014",
                                   "DevelopedLow2011","UrbanTotal2011")

L_CV90th=leaps(x=explan_CV90th,y=flow$CV90thAnnualFlow_nolmh,method="adjr2",nbest=4,names=c("DevelopedMed2011","Canopy2011","Imperviousness2011",
                                                                                      "DevelopedHigh2011","RoadDensity2014","UrbanLMH2011",
                                                                                      "DevelopedLow2011","UrbanTotal2011"))




expln_CV90th <- regsubsets(flow$CV90thAnnualFlow~., data=explan_CV90th,nvmax = 5,method="exhaustive")
CV90th.sum <- summary(expln_CV90th)
names(CV90th.sum)
CV90th.sum$rsq
plot(expln_CV90th, scale = "adjr2") 
coef(expln_CV90th,1)


write.csv(cbind(L_CV90th$which,L_CV90th$adjr2),"CV90th_leaps.csv")
####
## CV of 99th
explan_CV99th <- data.frame(cbind(dat$Canopy2011,dat$DevelopedMed2011))

colnames(explan_CV99th) <- c("Canopy2011","DevelopedMed2011")

L_CV99th=leaps(x=explan_CV99th,y=flow$CV99thAnnualFlow,method="adjr2",nbest=4,names=c("Canopy2011","DevelopedMed2011"))
expln_CV99th <- regsubsets(flow$CV99thAnnualFlow~., data=explan_CV99th,nvmax = 5,method="exhaustive")
CV99th.sum <- summary(expln_CV99th)
names(CV99th.sum)
CV99th.sum$rsq
plot(expln_CV99th, scale = "adjr2") 
coef(expln_CV99th,1)


write.csv(cbind(L_CV99th$which,L_CV99th$adjr2),"CV99th_leaps.csv")

####
## CV of Baseflow
explan_CVBaseflow <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$RoadDensity2014,
                                      dat$UrbanLMH2011,dat$Imperviousness2011,dat$DevelopedLow2011,
                                      dat$PopDensityBLK2010,dat$DevelopedHigh2011,dat$UrbanTotal2011,
                                      dat$HousDensityBLK2010,dat$EvergreenForest2011,dat$ForestTotal2011))

colnames(explan_CVBaseflow) <- c("DevelopedMed2011","Canopy2011","RoadDensity2014",
                                 "UrbanLMH2011","Imperviousness2011","DevelopedLow2011",
                                 "PopDensityBLK2010","DevelopedHigh2011","UrbanTotal2011",
                                 "HousDensityBLK2010","EvergreenForest2011","ForestTotal2011")

explan_CVBaseflow_noimp <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$RoadDensity2014,
                                            dat$UrbanLMH2011,dat$DevelopedLow2011,
                                            dat$PopDensityBLK2010,dat$DevelopedHigh2011,dat$UrbanTotal2011,
                                            dat$HousDensityBLK2010,dat$EvergreenForest2011,dat$ForestTotal2011))

colnames(explan_CVBaseflow_noimp) <- c("DevelopedMed2011","Canopy2011","RoadDensity2014",
                                       "UrbanLMH2011","DevelopedLow2011",
                                       "PopDensityBLK2010","DevelopedHigh2011","UrbanTotal2011",
                                       "HousDensityBLK2010","EvergreenForest2011","ForestTotal2011")

explan_CVBaseflow_nodhigh <- data.frame(cbind(dat$DevelopedMed2011,dat$Canopy2011,dat$RoadDensity2014,
                                              dat$UrbanLMH2011,dat$Imperviousness2011,dat$DevelopedLow2011,
                                              dat$PopDensityBLK2010,dat$UrbanTotal2011,
                                              dat$HousDensityBLK2010,dat$EvergreenForest2011,dat$ForestTotal2011))

explan_CVBaseflow_nodhighmedforest <- data.frame(cbind(dat$Canopy2011,dat$RoadDensity2014,
                                              dat$UrbanLMH2011,dat$Imperviousness2011,dat$DevelopedLow2011,
                                              dat$PopDensityBLK2010,dat$UrbanTotal2011,
                                              dat$HousDensityBLK2010,dat$EvergreenForest2011))

explan_CVBaseflow_canopyroadever <- data.frame(cbind(dat$Canopy2011,dat$RoadDensity2014,dat$EvergreenForest2011))

colnames(explan_CVBaseflow_nodhighmedforest) <- c("Canopy2011","RoadDensity2014",
                                         "UrbanLMH2011","Imperviousness2011","DevelopedLow2011",
                                         "PopDensityBLK2010","UrbanTotal2011",
                                         "HousDensityBLK2010","EvergreenForest2011")

colnames(explan_CVBaseflow_canopyroadever) <- c("Canopy2011","RoadDensity2014","EvergreenForest2011")


colnames(explan_CVBaseflow_nodhigh) <- c("DevelopedMed2011","Canopy2011","RoadDensity2014",
                                         "UrbanLMH2011","Imperviousness2011","DevelopedLow2011",
                                         "PopDensityBLK2010","UrbanTotal2011",
                                         "HousDensityBLK2010","EvergreenForest2011","ForestTotal2011")

L_CVBaseflow=leaps(x=explan_CVBaseflow,y=flow$CVMeanAnnualBaseflow,method="adjr2",nbest=4,names=c("DevelopedMed2011","Canopy2011","RoadDensity2014",
                                                                                                  "UrbanLMH2011","Imperviousness2011","DevelopedLow2011",
                                                                                                  "PopDensityBLK2010","DevelopedHigh2011","UrbanTotal2011",
                                                                                                  "HousDensityBLK2010","EvergreenForest2011","ForestTotal2011"))
expln_CVBase <- regsubsets(flow$CVMeanAnnualBaseflow~., data=explan_CVBaseflow_canopyroadever,nvmax = 5,method="exhaustive")
CVBase.sum <- summary(expln_CVBase)
names(CVBase.sum)
CVBase.sum$rsq
plot(expln_CVBase, scale = "adjr2") 
coef(expln_CVBase,3)


write.csv(cbind(L_CVBaseflow$which,L_CVBaseflow$adjr2),"CVBaseflow_leaps.csv")

### BFI

explan_CVBFI <- data.frame(cbind(dat$BasinGIS_km2,dat$totalppt_2004_2018_cm,dat$Slope,
                                      dat$PctHydrGrpB,dat$PctHydrGrpB_D,dat$PctHydrGrpC,dat$PctHydrGrpD_A_D_B_D_C_D))

colnames(explan_CVBFI) <- c("Basinarea","Totalppt","slope", "soil B","soil BD", "soil C", "soil DADBCDC")


expln_CVBFI <- regsubsets(flow$CVMeanAnnualBFI~., data=explan_CVBFI,nvmax = 5,method="exhaustive")
CVBFI.sum <- summary(expln_CVBFI)
names(CVBFI.sum)
CVBFI.sum$rsq
plot(expln_CVBFI, scale = "adjr2") 
coef(expln_CVBFI,5)

