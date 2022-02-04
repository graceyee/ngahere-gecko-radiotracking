###-----------Ngahere gecko radio-tracking study-----------###

rm(list=ls())

##---exploratory analysis---##

data <- read.csv("geckoPCA noNA.csv",header=TRUE)
attach(data)
names(data)

#boxplots of important variables between release types and means
boxplot(Distance~release_type,xlab="Release type",ylab="Distance travelled (m)", data=data)
aggregate(Distance ~  release_type, data, mean)

boxplot(Ht_ground~release_type, xlab="Release type",ylab="Height above ground (m)", data=data)
aggregate(Ht_ground ~  release_type, data, mean)

boxplot(Tree_ht~release_type, xlab="Release type",ylab="Tree height (m)", data=data)
aggregate(Tree_ht ~  release_type, data, mean)

boxplot(Direction~release_type, xlab="Release type",ylab="Direction from prev. fix (degrees)", data=data)
aggregate(Direction ~  release_type, data, mean)

boxplot(Temp~release_type, xlab="Release type",ylab="Temperature (deg.C)", data=data)
aggregate(Temp ~  release_type, data, mean)

boxplot(Rainfall~release_type, xlab="Release type",ylab="Rainfall (mm)", data=data)
aggregate(Rainfall ~  release_type, data, mean)

#normality
hist(data$SVL)
boxplot(data$SVL)

hist(data$Temp)
boxplot(data$Temp)

hist(data$Rainfall)
boxplot(data$Rainfall)

#scatterplots
plot(SVL,Distance,ylab="distance", xlab="SVL")
plot(Temp,Distance,ylab="distance", xlab="Temp")
plot(Rainfall,Distance,ylab="distance", xlab="Rainfall")




##---Student's t-test to test whether the average difference between the male and female SVL, VTL and mass significantly differed from zero---##




data1 <- read.csv("geckoLMsex.csv",header=TRUE)
t.test(data1$mSVL, data1$fSVL, paired=FALSE)
t.test(data1$mVTL, data1$fVTL, paired=FALSE)
t.test(data1$mmass, data1$fmass, paired=FALSE)

sd(data1$fmass, na.rm=TRUE) /  
  sqrt(length(data1$fmass[!is.na(data1$fmass)])) #estimate standard error

plot(data1$mSVL, data1$fSVL, paired=FALSE)
plot(data1$mVTL, data1$fVTL, paired=FALSE)
plot(data1$mmass, data1$fmass, paired=FALSE)

data2 <- read.csv("Summary_Sheet.csv",header=TRUE)
attach(data2)
names(data2)
data2$mass <- as.factor(data2$mass)
plot(mass, SVL, data=data2)




##---Dispersal and Microhabitat Selection - LMM and type II Wald chi sq test---##




library(glmmTMB)
library(lme4)
data <- read.csv("LM offset time.csv",header=TRUE)
a <- lmer(Distance~release_type+Sex+SVL+Temp+Rainfall+release_type*Sex+ (1|ID)+(1|Fix)+ (1|Date)+(1|Time)+ offset(log(hours)), data = data, na.action=na.omit) #straight-line distances between fixes LMM
b <- lmer(Ht_ground~release_type+Sex+SVL+Temp+Rainfall+release_type*Sex+ (1|ID)+(1|Fix)+ (1|Date)+(1|Time)+ offset(log(hours)), data = data, na.action=na.omit) #height above ground LMM

data0<-read.csv("distance between initial and final hours.csv",header=TRUE)
c <- lm(Distance~release_type+sex+SVL+release_type*sex + offset(log(hours)), data = data0, na.action=na.omit) #distance between intial and final fix LM

library(car)
Anova(a) #change to a, b, or c depending on which model results you want from above
summary(a)
confint(a)
af<-anova(a)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))#calculate the % variance explained for each predictor variable




##---Home Ranges - LMM and type II Wald chi sq test---##




data1<-read.csv("95MCP.csv",header=TRUE)
data2<-read.csv("95kernel.csv",header=TRUE)
a <- lmer(Area~release_type+SEX+SVL+release_type*SEX+(1|Fix), data = data1, REML= FALSE, na.action=na.omit)
b <- lmer(Area~release_type+SEX+SVL+release_type*SEX+(1|Fix), data = data2, REML= FALSE, na.action=na.omit)

Anova(a) #change to a or b depending on which model results you want from above
summary(a)
confint(a)
af<-anova(a)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))#calculate the % variance explained for each predictor variable




#---Activity - Multinomial Data---#




require(foreign)
require(nnet)
library(lme4)
data <- read.csv("multinomial data.csv",header=TRUE)

m1<-lm(not.moving~releasetype,data=data)

summary(m1)

y <-cbind(data$not.moving,data$moving,data$basking,data$feeding)
y

m1<-lm(cbind(not.moving,moving,basking,feeding)~releasetype,data=data)
summary(m1)

m1<-lm(y~releasetype,data=data)
summary(m1)

activ<-glm(y~releasetype+(1|ID), data = data)#???? can't seem to get to work
head(y)

data<-read.csv("geckoLM.csv", header=TRUE)

options(na.action = "na.fail") #removes NAs from analysis
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(car)

head(data)

data$Tree.spp <- relevel(data$Tree.spp, ref = "kanuka") #kanuka is the intercept
data$Activity <- relevel(data$Activity, ref = "moving") #moving is the intercept


test2<-glm(Rainfall ~ release_type,data=data,na.action = na.omit)
summary(test2)
test <- multinom(Activity ~ release_type+Sex,data=data,na.action=na.omit)
summary(test)

coef(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p




#---Home Ranges - PCA---#




library(MASS)
library(heplots)
library(vegan)

data <- read.csv("geckoPCA noNA.csv",header=TRUE)

#turn factors to numeric for all data columns
indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

pca<-prcomp(data[,c(6:19)], scale. = TRUE)

#screeplot to show the variances of each PC axis
screeplot(pca, bstick = TRUE, ylab = "Variance")
summary(pca)
pca$rotation

PC1<-predict(pca)[,1]
PC2<-predict(pca)[,2]

data$PC1<-PC1
data$PC2<-PC2

str(data)

SR<-subset(data,release_type=="2")
HR<-subset(data,release_type=="1")

PC1_h <- data$PC1[which(data$release_type=="1")]
PC2_h <- data$PC2[which(data$release_type=="1")] 
PC1_s <- data$PC1[which(data$release_type=="2")]
PC2_s <- data$PC2[which(data$release_type=="2")]

#create a convex hull around plot points
Plot_ConvexHull<-function(xcoord, ycoord, lcolor, ltype){  
  hpts <- chull(x = xcoord, y = ycoord)  
  hpts <- c(hpts, hpts[1])  
  lines(xcoord[hpts], ycoord[hpts], col = lcolor, lty = ltype)
}  

par(mar=c(5,5,4,3)) #setting graph margins
plot(PC1,PC2, type="n", bty="n", las=1, cex.lab=1.35, ylim=c(-4,6),xlim=c(-5,8),
     ylab="Principal component 2 (11.04 %)", xlab="Principal component 1 (17.11 %)", family="serif") 

Plot_ConvexHull(xcoord = PC1_h, ycoord = PC2_h, lcolor = "black", lty=1)
Plot_ConvexHull(xcoord = PC1_s, ycoord = PC2_s, lcolor = "black", lty=1)

points(PC1_h, PC2_h, type = "p", pch = 0)
points(PC1_s, PC2_s, type = "p", pch = 16)

arrows(min(PC1), 0, max(PC1), 0, lty = 1, lwd = 1,code=3,length=0.1)
arrows(0, min(PC2), 0, max(PC2), lty = 1, lwd = 1,code=3,length=0.1)  

Text1<-"Height above ground"
Text2<-"Distance travelled"
text(x=0, y=6, label=Text1, srt=0)
text(x=6, y=0, label=Text2, srt=0)

legend("bottomright",leg=c("Penned", "Hard-release"), 
       lty=0, pch=c(16, 2, 0), text.font=3, 
       col="black", bty="n",cex=0.9, pt.cex=1.25)



#---COLLINEARITY TEST - variance inflation factors---#



data <- read.csv("geckoPCA noNA.csv",header=TRUE)
pairs(data)#creates SCATTERPLOTS for all variable pairs in the data set
library(car)
vif1 <- vif(lm(Distance~release_type+Sex+SVL+Temp+release_type*Sex, data = data, na.action=na.omit))
summary (vif1)
vif1 #VIF values > 10 indicate a "collinearity problem"
d<-lm(Distance~release_type+Sex+SVL+Temp+release_type*Sex, data = data, na.action=na.omit)