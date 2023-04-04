#***************************************************************
#
#      In-Class Grid Stability Classification Contest
#   		   
#***************************************************************

# Group Members: TJ Gwilliam, Sofia Zajec, Cecilia Smith, Reese Quillian

#***************************************************************
#
#  Read in the data
#
#***************************************************************

# Set working directory
sourcedir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class"
datadir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data/kaggle competition/"
setwd(sourcedir)

# Read in the data
Sonar.train <- read.csv(paste(datadir,"/Sonar_train.csv", sep=""), sep = ",", header = T)
Sonar.test <- read.csv(paste(datadir,"/Sonar_test.csv", sep=""), sep = ",", header = T)

# Source potentially useful functions
source("pc.glm.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("TestSet.R")


# variable 61 = response variable

#*************************************************************************************
#
# Build the best GLM you can to predict whether an object is a rock
# (0) or meteor (1)
#
#*************************************************************************************

# Build models to training set:

# null model
sonar.null <- glm(V61~1, data = Sonar.train, family = binomial)
summary(sonar.null)

# full main effects model:
sonar.glm.main <- glm(V61~., Sonar.train, family = binomial)
summary(sonar.glm.main)

anova(sonar.null, sonar.glm.main, test = "Chi")

# main effects model has predictive power


# same model utility test but with log transformed predictors
# log transformation
Lsonar.train <- log(Sonar.train[,-61] + .1)
Lsonar.train$V61 <- Sonar.train$V61

# main effects with log transformed predictors
Lsonar.glm.main <- glm(V61~., data = Lsonar.train, family = binomial)
Lsonar.null <- glm(V61~1, data = Lsonar.train, family = binomial)

anova(Lsonar.null, Lsonar.glm.main, test = "Chi")


#checking for influential points
# Find the most influential points
# in both sonar.glm.main and Lsonar.glm.main

which(cooks.distance(sonar.glm.main) > 0.5)

which(cooks.distance(Lsonar.glm.main) > 0.5)
# no influential points to remove


# Stepwise selection:
#main effects
sonar.step <- step(sonar.glm.main, trace = 0) 
length(sonar.step$coeff) - 1 #down to 23 variables

# log transform
Lsonar.step <- step(Lsonar.glm.main, trace = 0)
length(Lsonar.step$coeff) - 1 # only 21 variables


# pca model with variables that account for 95% of variance
sonar.pca <- princomp(Sonar.train[,-61], cor = T)
sonarpca.glm95 <- pc.glm(sonar.pca, 95, Sonar.train[,61])

# model utility test
sonarpca.null <- pc.null(sonar.pca, 95, Sonar.train[,61])
anova(sonarpca.null, sonarpca.glm95, test = "Chi")


# 98%
sonarpca.glm98 <- pc.glm(sonar.pca, 98, Sonar.train[,61])

# partial likelihoods
anova(Lsonar.step, sonar.step, test="Chi")
anova(sonarpca.glm95, sonarpca.glm98, test = "Chi") # go with glm95 model

summary(sonarpca.glm95)

#AIC
AIC(sonar.glm.main)
AIC(Lsonar.glm.main)
AIC(sonar.step)
AIC(Lsonar.step) # lowest
AIC(sonarpca.glm95)
AIC(sonarpca.glm98) #highest


# Predictions on test data 
sonar.main.pred <- predict(sonar.glm.main, type = "response", newdata = Sonar.test)
Lsonar.main.pred <- predict(Lsonar.glm.main, type = "response", newdata = Sonar.test)
sonar.step.pred <- predict(sonar.step, type = "response", newdata = Sonar.test)
Lsonar.step.pred <- predict(Lsonar.step, type = "response", newdata = Sonar.test)
sonarpca95.pred <- predict.pc.glm(sonarpca.glm95, sonar.pca, Sonar.test[,1:60])

# there is no response variable in the test dataset.
# aka can't do confusion matrices OR ROC

# 2 models to submit:
### sonarpca95.pred
### Lsonar.step

# Convert probabilities to 0/1 classification based on threshold T
T <- 0.5
Sonar.classifications1 <- matrix(0,nrow=length(sonarpca95.pred),ncol=2)
Sonar.classifications1[,1] <- c(1:length(sonarpca95.pred))
Sonar.classifications1[which(sonarpca95.pred > T),2] <- 1

# new threshold
T <- 0.7
Sonar.classifications2 <- matrix(0,nrow=length(sonarpca95.pred),ncol=2)
Sonar.classifications2[,1] <- c(1:length(sonarpca95.pred))
Sonar.classifications2[which(sonarpca95.pred > T),2] <- 1

# Write predictions to files
colnames(Sonar.classifications1) = c("Id","Predicted")
write.csv(Sonar.classifications1, paste(datadir,"/Group6Submission1.csv", sep=""), row.names=FALSE)

colnames(Sonar.classifications2) = c("Id","Predicted")
write.csv(Sonar.classifications2, paste(datadir,"/Group6Submission2.csv", sep=""), row.names=FALSE)
