# ******************************************************
#
#					    
#			 		Multivariate Viz
#
# ******************************************************


# ***************************
# 0.1 installing and loading the library for the visualization
# ***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session


# ***************************
# 0.2 installing and loading the library for this session
# ***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#install.packages('psych')
library(psych)
#install.packages('lattice')
library(lattice)


# **********************************************************
# 1. Reading in data
# **********************************************************

#***************************
# 1.1 Set working directory
#***************************

# Set working directory

traindir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/Lab 1/Data/TrainData"
sourcedir <-"C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/Lab 1/"


# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()

# ***************************
# 1.2 loading all years data
# ***************************
# Source AccidentInput
setwd(sourcedir)
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2020
# with columns that are consistent for all of these years

# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame
totacts <- combine.data(acts)

# Get dimensions of the combined dataframe
dim(totacts)


# *************************************************
#
#		2. More Data Cleaning
#
# *************************************************

# Variable names
names(totacts)
colnames(totacts)

# View the data types
str(totacts)

# Look at the type for TYPE using summary
summary(totacts$TYPE)

#Now, let's put more meaningful labels on TYPE variable
class(totacts$TYPE)
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))

# Use table() to see the frequencies
table(totacts$TYPE)


# ***********************************
#
# 	3. Visualization
#
# ***********************************

# ***************************
# 3.1 bar plot
# ***************************
# Use barplot() to graph this 
#stat="bin".
#I change the stat of geom_bar() from count (the default) to identity. 
#This lets me map the height of the bars to the raw values of a y variable.
# If you want the heights of the bars to represent values in the data, use stat="identity" 
#and map a value to the y aesthetic."
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")

# Add color, a title, and change the text size and rotate text
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity",fill= "steelblue")+ 
  ggtitle("Accident Frequency by Type") +
  labs(x = "Type of Accident")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))



# Looks at TYPEQ
# First convert to numeric, using as.numeric()
totacts$TYPEQ <- as.numeric(totacts$TYPEQ)

# Now convert to factor- use actual categories from data dictionary to be more informative
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(as.data.frame(table(totacts$TYPEQ)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  labs(x = "Type of Consist")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

# Look at CAUSE with summary
# CAUSE: primary cause of incident
summary(totacts$CAUSE)

# Create a new variable called Cause
# that uses labels for cause.
# Add it to totacts.
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor
totacts$Cause <- factor(totacts$Cause)

# use table() and barplot() to see it.
table(totacts$Cause)

# Look at histograms of TEMP with different breaks
# Breaks with gap of 10
ggplot(as.data.frame(totacts$TEMP), aes(x=totacts$TEMP)) + 
  geom_histogram(breaks=seq(-50, 120, by=10)) + 
  labs(x = "Temperature", y = "Frequency") 

# Breaks with gap of 15
ggplot(as.data.frame(totacts$TEMP), aes(x=totacts$TEMP)) + 
  geom_histogram(breaks=seq(-50, 120, by=15)) + 
  labs(x = "Temperature", y = "Frequency") 

# Breaks with gap of 30
ggplot(as.data.frame(totacts$TEMP), aes(x=totacts$TEMP)) + 
  geom_histogram(breaks=seq(-50, 120, by=30)) + 
  labs(x = "Temperature", y = "Frequency") 


# Change the color and title
ggplot(as.data.frame(totacts$TEMP), aes(x=totacts$TEMP)) + 
  geom_histogram(fill= "steelblue",breaks=seq(-50, 120, by=15)) + 
  ggtitle("Temperature Frequency") +
  labs(x = "Temperature)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


# Now do the summary for totacts, but remove all narratives
# and any other variables you won't use for this project
cbind(1:ncol(totacts), names(totacts))
totacts %>% select(starts_with("NARR"))
tmp = totacts[,c(122:136)]
new.df <- totacts[,-c(122:136)]


# What's the narrative of the worst accident for total injured?



# ***************************
# 3.1 scatter plot
# ***************************

# Scatter plots
#total damages each year
df <- data.frame(year=2001:2022,damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))
ggplot(data=df, aes(x=year, y=damages)) + 
  geom_line() + 
  geom_point()


# without panel functions for 2020
pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts[[21]])

# with panel function- a little more detail
pairs.panels(acts[[20]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# Do this for all accidents
pairs.panels(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# Save as png to avoid problems in the document- make sure not to save in directory with data files
png("metrics.png")
pairs.panels(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])
dev.off()


# ***************************
# 3.2 Trellis Categorical Plots
# ***************************
# Plotting damage per year
ggplot(data = totacts, aes(x = as.factor(YEAR), y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(x = "Year", y = "Damage ($)")

# Which accident has the most damage?
which(totacts$ACCDMG == max(totacts$ACCDMG))
totacts %>% filter(ACCDMG == max(ACCDMG))
totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG))]

worst_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG))


# what type of accident had the most damage?
totacts$TYPE[which(totacts$ACCDMG == max(totacts$ACCDMG))]
worst_dmg %>% select(TYPE)

# Find out the worst accident for total killed and injured
max_totinj = which(totacts$TOTINJ == max(totacts$TOTINJ))
totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]

totacts$TOTINJ[max_totinj]
totacts[max_totinj,]
totacts[max_totinj,122:136]
totacts$ACCDMG[max_totinj]

worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ))
worst_inj %>% select(TOTINJ)


# What's the narrative of the worst accident for total injured?
worst_inj %>% select(starts_with("NARR"))
worst_inj %>% select(ACCDMG)

# Find the worst accidents in 2018.  What happened?
acts <- totacts %>% filter(YEAR == 18)
#damage
totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG[which(totacts$YEAR==18)]))]
acts %>% filter(ACCDMG == max(ACCDMG))
#injury
totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ[which(totacts$YEAR==18)]))]
acts %>% filter(TOTINJ == max(TOTINJ))
#killed
totacts$TOTKLD[which(totacts$TOTKLD == max(totacts$TOTKLD[which(totacts$YEAR==18)]))]
acts %>% filter(TOTKLD == max(TOTKLD))

#Find the accidents with the most injuries in 2018.  What happened in these accidents?  
#Explore the news to understand underlying causes and what could have been done to prevent 
#these accidents.
acts %>% arrange(desc(TOTINJ)) %>% relocate(TOTINJ) %>% head(5)
which(totacts$TOTINJ > 50 & totacts$YEAR==18)

#Find the accident with the most damage in 2018.  What do you notice about the accidents with the most damage and those with the most injuries?
which(totacts$ACCDMG == max(totacts$ACCDMG[which(totacts$YEAR==18)]))
which(totacts$ACCDMG > 5e6 & totacts$YEAR==18)

acts %>% filter(ACCDMG == max(ACCDMG))
acts %>% arrange(desc(ACCDMG)) %>% relocate(ACCDMG) %>% head(5)

acts %>% arrange(desc(ACCDMG))

# Plotting accident cause vs. damage
ggplot(data = totacts, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(y = "Damage ($)", x = "Accident Cause")

# Plot scaled (log) accident damage
bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)

# Plot cause vs. scaled (log) accident damage
ggplot(data = totacts, aes(x = Cause, y = log(ACCDMG+1))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Log(Accident Damage)") +
  labs(y = "log(Damage ($))", x = "Accident Cause")



# Plot cause vs. no. killed 
ggplot(data = totacts, aes(x = Cause, y = TOTKLD)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Total Killed")+
  labs(y = "Total Killed", x = "Accident Cause")

# Plot cause vs. injured
ggplot(data = totacts, aes(x = Cause, y = TOTINJ)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Injured") +
  labs(y = "Total Injured", x = "Accident Cause")


# X-Y plots conditioned on cause for both killed and injured
qplot(ACCDMG, TOTKLD, data = totacts) + 
  facet_wrap(~Cause, scales = "free") +
  ggtitle("Damage vs. Killed Conditioned on Cause") + 
  labs(x =  "Total Accident Damage", y  = "Total Killed") +
  theme(plot.title = element_text(hjust = 0.5))
