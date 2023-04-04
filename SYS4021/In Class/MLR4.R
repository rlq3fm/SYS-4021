#
#      	    
#			Multiple Linear Regression
#	 Transformations, Qualitative Variables & ANCOVA
#
#******************************************************

# Set working directory

# or a Windows user
traindir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data/Train Data"
sourcedir <-"C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class"

setwd(sourcedir)
source("AccidentInput.R")

setwd(traindir)

acts <- file.inputl(traindir)

totacts <- combine.data(acts)

# Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

## Remove 9/11
xdmg <- xdmg[-185,]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

## ******************************************* 
## Interaction Plots ##
## *******************************************

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

# Create Derail variable
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$Type == "Derailment")] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

# Plot interaction between Derailment and Cause

interaction.plot(Derail, xdmgnd$Cause,log(xdmgnd$ACCDMG))

# With ggplot
ggplot() +
  aes(x = Derail, y = log(xdmgnd$ACCDMG), group = xdmgnd$Cause, color = xdmgnd$Cause) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")


# Interaction plots with quantitative variables
Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),median(xdmgnd$TRNSPD),max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),1,max(xdmgnd$CARS)), include.lowest = T, right=F,labels = c("low hzd", "high hzd"))

Tons <- cut(xdmgnd$TONS, c(min(xdmgnd$TONS),median(xdmgnd$TONS),max(xdmgnd$TONS)), include.lowest = T, labels = c("low tons", "high tons"))

# Plot interaction between Speed and Cars
interaction.plot(Speed, Cars, log(xdmgnd$ACCDMG))

# First option with seeing points
qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Cars) +
  geom_point(colour = "gray")+
  geom_smooth(method = "lm") 

# Second option without points
ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Cars, color = Cars) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")
ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Cars on Accident Damage")


# Create a Freight variable
Freight <- rep(0, nrow(xdmgnd))
Freight <- (xdmgnd$TYPEQ == "Freight")
Freight[is.na(Freight)] = 0
Freight <- as.factor(Freight)
contrasts(Freight) 

# Plot interaction between Freight and Speed
interaction.plot(Freight, Speed, log(xdmgnd$ACCDMG))


ggplot() +
  aes(x = Freight, y = log(xdmgnd$ACCDMG), group = Speed, color = Speed) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Freight on Accident Damage")


# Plot interaction between Derailments and Speed
interaction.plot(Derail, Speed, log(xdmgnd$ACCDMG))

# With ggplot
ggplot() +
  aes(x = Derail, y = log(xdmgnd$ACCDMG), group = Speed, color = Speed) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)") +
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Derailment on Accident Damage")

# Plot interaction between Speed and Tons
interaction.plot(Speed, Tons, log(xdmgnd$ACCDMG))


# Plot interaction between Speed and Tons with ggplot
ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Tons, color = Tons) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Tons on Accident Damage")


## ******************************************* 
## Build linear regression models in R: lm ##
## *******************************************

# Build 2 multiple linear regression models with the data fram xdmgnd: 
# xdmgnd.lm1  ACCDMG ~ TEMP + TRNSPD + TONS + CARS + HEADEND1
# xdmgnd.lm2  ACCDMG ~ TEMP + TONS + CARS 

xdmgnd.lm1<-lm(ACCDMG ~ TEMP + TRNSPD  + CARS + HEADEND1,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG ~ TEMP + CARS ,data=xdmgnd)

# Display regression results for each model
summary(xdmgnd.lm1)
summary(xdmgnd.lm2)

#  create models with interactions between all of the quantitative predictors ----
xdmgnd.lm3<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2,data=xdmgnd)

summary(xdmgnd.lm3) 

xdmgnd.lm.test<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1+HEADEND1*TRNSPD),data=xdmgnd)

# Is this the complete second order model?

#  I() allows your model to contain normal mathematical sysmbols 
#  Create complete second order model                            
xdmgnd.lm4<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2+I(TEMP^2)+I(TRNSPD^2)+I(CARS^2)+I(HEADEND1^2),data=xdmgnd)
summary(xdmgnd.lm4)

# How many parameters and coefficients are in each of the models?


# Create a stepwise regression model on xdmgnd.lm4
xdmgnd.lm4.step <- step(xdmgnd.lm4)
summary(xdmgnd.lm4.step)


# partial F test ----
anova(xdmgnd.lm3,xdmgnd.lm4)

# Which model is better based on the partial F test, the larger or smaller?


# Interaction Plot Example
trnspdbox<-boxplot(xdmgnd$TRNSPD)
TRNSPD.factor<-xdmgnd$TRNSPD
TRNSPD.factor[which(xdmgnd$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xdmgnd$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)


# Create derailment variable
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

# create an interaction plot for TRNSPD and Derailments
interaction.plot(TRNSPD.factor,Derail, xdmgnd$ACCDMG)



##************************* 
## Qualitative Variables ##
##************************* 

# Create a qualitative variable for Cause 
Cause <- rep(NA, nrow(xdmgnd))
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"  ##Miscellaneous
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"  ##Track
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"  ##Signal or communication
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"  ##Human
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"  ##Electrical or mechanical

# This new variable, Cause, has to be a factor
Cause <- as.factor(Cause)

# Lets look at the default treatment coding
contrasts(Cause)

# What is the base case for Cause?



# Write a model to predict ACCDMG in terms of Cause 
xdmgnd.lm5<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm5) 

tapply(as.numeric(xdmgnd$ACCDMG), as.factor(Cause), mean)

# How do we interperet the model xdmgnd. lm5?



#Change based case to H ----
contrasts(Cause)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(Cause)) <-matrix(c("E","M","S","T"),ncol=4)
contrasts(Cause)

xdmgnd.lm6<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm6)

#How do we interperet the model xdmgnd.lm6?


#More qualitative variables:
xdmgnd.lm7<-lm(ACCDMG~as.factor(WEATHER),data=xdmgnd)
summary(xdmgnd.lm7) #What is the base case?


## *****************************************Exercise 1*****************************************

# Recode the qualitative variable Weather using the categories 
# in accident descriptions pdf document


# What is the base case?


# Build a model with the qualitative variable weather



# What is your conclusion?



# Build a new model to test the hypothesis: 
# "Accidents of type derailment do not increase 
# the severity of rail road accidents."




# What is the base case?




# What is your conclusion?





## Create 2 ANCOVA models:

#attach the dataset to the R search path. 
attach(xdmgnd) 

#remove it from the search path
# detach(xdmgnd) 

#now you can use variable names directly
summary(ACCDMG) 


# Build a main effects model Cause + TEMP + TRNSPD +  CARS + HEADEND1
xdmgnd.lm1 <-lm(ACCDMG~Cause + TEMP + TRNSPD +  CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm1)

# Build a main effects + interaction model Cause + TEMP + TRNSPD + CARS + HEADEND1 
xdmgnd.lm2<-lm(ACCDMG~(Cause+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm2)

#Perform a Partial F Test: xdmgnd.lm1 vs. xdmgnd.lm2
anova(xdmgnd.lm1,xdmgnd.lm2)

# Which model do you choose?


## Use stepwise regression on your main effects + interaction model xdmgnd.lm1
xdmgnd.lm1.step <- step(xdmgnd.lm2)

# What is your new model?
summary(xdmgnd.lm1.step)


xdmgnd.lm.Derail<-lm(ACCDMG~(Derail+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm.Derail)


xdmgnd.lm1<-lm(ACCDMG~(Derail+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm1)

xdmgnd.lm1.step <- step(xdmgnd.lm1)

anova(xdmgnd.lm1, xdmgnd.lm1.step)


## *****************************************Exercise 2*****************************************
# Choose 1 of your project 1 hypotheses (or assignment 2) based on an actionable, 
# controllable variable.  Build a model with both qualitative
# and quantitative variables to test your hypothesis.  Make sure 
# to address the following steps:
# 1.  Variable selection for linear models.
# 2.  Treatment of categorical variables for your linear model.
# 3.  Measure the performance of the models.
# 4.  Adjust your models based on analytical and graphical diagnostics.
# 5.  Reassess models based on adjustments.
# 6.  Compare your best models.
# 7.  Use your best model to reject or accept your hypothesis and provide a 
#     recommendation supported by statistical evidence.

