

#************************************************************
#
#				SPAM Filter 2 
#			Generalized Linear Models of SPAM 
#
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************

sourcedir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class"
datadir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data"

setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

#*****************************
#
# GLM
#
#*****************************

# Use the glm() function to obtain the glm for the spam filter.

spam.glm.main <- glm(V58~., data = spam, family = binomial)

# notice the warning message!

# Use summary to evaluate the model

summary(spam.glm.main)

# What is the contribution of variable 51 to the model?
# notice that we add one to the variable number (51)
# to account for the intercept

(exp(spam.glm.main$coefficients[52])-1)*100

# for a unit change in V51, there is a 48% decrease in the odds of spam

# Explain this result.


spam[1,] # first observation is 0
predict(spam.glm.main, newdata = spam[1,]) # log-odds of being spam for observation 1
exp(predict(spam.glm.main, newdata = spam[1,])) # odds of being spam for observation 1
# odds of being spam for observation 1 if V51 increases by 1 unit
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1))) 

# confirming odds decrease by 48%
# 1.624551*(1-.48191) = 0.842

# Can we use an F test for model utility?
# Perform a model utility test for the glm
# Model Utility Test using Chi2 statistic

# What do you conclude?


spam.null <- glm(V58~1, data = spam, family = binomial)

summary(spam.null)

anova(spam.null, spam.glm.main, test = "Chi")

# main effects model has predictive power and is significant

# Create a model with just the capital letters, 
# V55 - V58  as predictors

spam.cap <- glm(V58~., data = spam[,55:58], family = binomial)

# How does this model do on the model utility test?
# significant

anova(spam.null, spam.cap, test = "Chi")

# Use the likelihood ratio or partial chi square test to compare the full main effects model with the capital letters model

anova(spam.cap, spam.glm.main, test = "Chi") #additional predictors significant


# Can we use the t-test for individual coefficients? NO; no normal assumption
  # drop variables one at a time

# Use the full main effect model and test the coefficient on 
# V57

spam.no57 <- update(spam.glm.main, .~.-V57, data = spam) #var 57 dropped

anova(spam.no57, spam.glm.main, test = "Chi")
# model 1 has variable 57, model 2 does not
# conclude that variable 57 is significant


# What is the contribution of variable 57 to the model?
# notice that we add one to the variable number (57)
# to account for the intercept

(exp(spam.glm.main$coefficients[58])-1)*100
# if var 57 increase by 1 unit, odds of spam increase by .08

# Explain this result.
spam[1,]
predict(spam.glm.main, newdata = spam[1,])
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:56)], V57 = 279)))


# compare the drop1 chi square test to the approximate Gaussian test in summary.
# This will take some time.

library(MASS)

drop1(spam.glm.main, response~., test = "Chi", data = spam)
# shows which variables are significant

#  Compare the step model with capital letter predictors to the capital letter model

step.cap <- step(spam.cap, data = spam, family = binomial)
summary(step.cap)
# kept only 55 and 56
# even though 57 was significant (add predictive value) but does not improve AIC
  # judgement call; be consistent with framework

# Run stepwise for the complete model
# Do this at home when you have time.
# Compare a stepwise model that starts with the full main effects model to the main effects model.

# **********************************************************
# Repeat the above analysis with log transformed predictors
#***********************************************************


#*****************************
#
# GLM with Interactions
#
#*****************************

# Compare a main effects model with all variables to this same model that also includes
# the interaction terms between V5, V6, V7. 
# Do the comparison with a partial likelihood test.
# (note: do not do a complete interacton model! Unless you have time.)
# Which model do we choose?

spam.glm <- glm(V58~., data = spam, family = binomial)

spam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = spam, family = binomial)
summary(spam.glm2)

spam.glm3 <- glm(V58~. + (V5+V6+V7)^3, data = spam, family = binomial)
summary(spam.glm3) # third order predictors


anova(spam.glm, spam.glm2, test = "Chi")
# adding (V5+V6+V7)^2 does add value

# Now compare a main effects model with log transformed predictors 
# to this same model that also includes
# the interaction terms between the log transformed variables V5, V6, V7. 
# Use an offset of 0.1
# Do this comparison with a partial likelihood test.
# (note: Again do not do a complete interacton model! Unless you have time.) 
# Which model do you choose?

Lspam <- log(spam[,-58] +.1)

Lspam$V58 <- spam[,58]

Lspam.glm <- glm(V58~., data = Lspam, family = binomial)


Lspam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = Lspam, family = binomial)

anova(Lspam.glm, Lspam.glm2, test = "Chi")
# in this case, we no longer see that interactions are significant if
# we log-transform our data

