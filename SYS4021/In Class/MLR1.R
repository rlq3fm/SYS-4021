#    			
#			
#	 Multiple Linear Regression 1
#
#******************************************************

# Set working directory

# or a Windows user
traindir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data/Train Data"
sourcedir <-"C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class"

##load data
setwd(sourcedir)
source("AccidentInput.R")

#load libraries
library(ggplot2)
library(GGally)
library(devtools) # for ggbiplot
library(ggbiplot)
library(psych)


acts <- file.inputl(traindir)

totacts <- combine.data(acts)

##Build a data frame with only extreme accidents for ACCDMG

dmgbox <-boxplot(totacts$ACCDMG)

ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

#remove 9/11
xdmg <- xdmg[-183,]

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Setup categorical variables
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))

xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

xdmgnd$Cause <- factor(xdmgnd$Cause)

# Convert Type to a factor and give more meaningful labels
xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

# Type of train
xdmgnd$TYPEQ <- as.numeric(xdmgnd$TYPEQ)

# Now convert to factor with meaningful labels
xdmgnd$TYPEQ <- factor(xdmgnd$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))


# Type of train
#***********************************************************
#  	Possible predictors of damage	
#***********************************************************

# SPM
#Scatter plot matricies for quantitative predictors and single metric.


pairs.panels(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

ggpairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])


# PCA
# Principal components with the correlation matrix for extreme data with 1 metric and quantitative predictors.

source("PCAplots.R")

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )
biplot(pred.pca)

#with ggbiplot
ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])


## Which predictors are most correlated with accident damage?


###############################
# Categorical plots

# heatmap
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")

## Which accident causes and types have the highest numbers of extreme accidents?

# Type & TRNSPD
library(lattice)

xyplot(log(ACCDMG)~TRNSPD | Type, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Type, scales = "free")


# Cause & TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause, scales = "free")


##What is notable about the relationship between train speed and accident
##damages for different accident causes and types?

#More complex xyplots

# Cause X Type and TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause * Type, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Type, scales = "free")


# Create the Derail variable & 
# then look at ACCDMG with Cause & Derail
xdmgnd$Derail <- (xdmgnd$Type == "Derailment")
xdmgnd$Derail

# plot xy with interactions of Derail and Cause
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Derail, scales = "free")


## How might these results inform your hypotheses?
## Use the multivariate visualizations as evidence to form at least 1 hypothesis.


####################################
# Linear Models
####################################

# Build linear regression models with different combinations of quantitative predictors to provide evidence for your hypothesis

# Single predictor
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
summary(xdmgnd.lm1)
#null hypothesis for model utility? 
#Btemp = 0
#fail to reject the null hypothesis because temp is not significant at p<0.05 level

names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2)


# Two predictors
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)

#null hypothesis for the model utilty?
#Btemp = Btranspd = 0
# reject null hypothesis


#null hypothesis for the t-test for temperature?
#Btemp = 0


summary(xdmgnd.lm2)
names(xdmgnd.lm2)
coef(xdmgnd.lm2)



#Linear regression model with 3 predictors
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm3)

coef(xdmgnd.lm3)


# Interpret your model coefficients.  Do they make sense?



# Interpret your developed models using the model utility test and t-test.



# Write out the null and alternative hypothesis for each of the tests.  



# Do you reject or fail to reject H0 for each test?




####################################
#	Now repeat for TOTKLD + TOTINJ
####################################
