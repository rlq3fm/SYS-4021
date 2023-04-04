#********************************************************************************
#             
#					Univariate Graphics
#
#********************************************************************************

#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session


#***************************
# 0.2 installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

# For example, an Mac user
#traindir <- "~/Google Drive/UVA/Courses/LSM/Fall2022/Data/TrainData/"
#sourcedir <-"~/Google Drive/UVA/Courses/LSM/Fall2022/Source/"

# or a Windows user
traindir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data/Train Data"
sourcedir <-"C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class"


# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()

# list the files
dir()


#***************************
# 1.2 loading the data
#***************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2020
accident_data_20 <- read.csv("RailAccidents20.csv")

#***************************
# 1.3 information about the selected year accident data
#***************************

head(accident_data_20) # the first 6 rows (observations) of data
dim(accident_data_20) #number of rows (observation) and number of columns (variables)
summary(accident_data_20) #summary of each column (variable)
str(accident_data_20) #type of each column (variable) of data

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
str(accident_data_20[,c("ACCDMG", "TOTKLD", "CARS", "STATION")])

colnames(accident_data_20) # number of columns (variable)
class(accident_data_20$TOTKLD) #type of one specific variable 
var(accident_data_20$TOTKLD) #variance of one specific variable (total killed) - quantitative
mean(accident_data_20$TOTKLD) #mean of one specific variable (total killed) - quantitative
table(accident_data_20$TOTKLD) #frequency of each value in the selected variable

class(accident_data_20$STATION) #type of one specific variable 
levels(accident_data_20$STATION) #differnt values of categorical variable (station) - qualitative
table(accident_data_20$STATION) #frequency of each value in the selected variable


# You can round your answer using the round() function
print(mean(accident_data_20$TOTKLD))
print(round(mean(accident_data_20$TOTKLD)))

#***************************
# 1.4 loading all years data
#***************************

# You will need to read in all 22 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:
setwd(sourcedir)
source("AccidentInput.R")

# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

setwd(traindir)
my.files <- list.files(traindir)

# 2 different ways of loading multiple files in a directory

# the first uses the lapply function
acts <- lapply(my.files, read.csv)

# the second uses file.inputl function from AccidentInput.R
acts <- file.inputl(traindir) 

# the argument for the list.files and file.inputl functions is the
# specification of the path to your file.  In my case traindir

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.
acts[[1]]$YEAR #it shows 1 --> 2001
acts[[5]]$YEAR #it shows 5 --> 2005

# Before we put all the data into one data frame
# we must clean the data
##################################################
#
#	2. Data Cleaning
#
##################################################

#***************************
# 2.1 columns in all years
#***************************
# Are the number of columns different from year to year?
ncol(acts[[1]]) #145 columns
ncol(acts[[8]]) #145 columns
ncol(acts[[7]]) #145 columns

# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[22]]))
comvar

#***************************
# 2.1 combine all years of data fater selecting a common columns
#***************************
# Now combine the data frames for all 20 years
# Use combine.data() funtions which takes as arguments the files and the list 
# of common variables
totacts <- combine.data(acts, comvar)

#check the number of rows and columns in this merged dataset
dim(totacts)

# the total accident should be combination of all data
rows = 0
for(acc_year in acts){
  print(dim(acc_year))
  rows = rows + dim(acc_year)[1]
  print(rows)
}
# the following numbers should be the same
print(rows) #total number of observations
dim(totacts)[1] #total number of observations


# How many entries (observations) does each year of accident have?
totacts %>% group_by(YEAR) %>% 
  summarise(entries = n()) %>% 
  arrange(desc(entries))


#***********************************
#
# 	3. Visualization
#
#***********************************


#***************************
# 3.1 Histogram
#***************************

# These examples are for year 2011 
# Histograms are both in default way and also with ggplot2 package

# for 2011
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Different bin widths

# Bin FD
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.FD(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin Scott
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.scott(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin 20
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 20) + ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin 2
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 2) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


# other years
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Accident damage 2010-2021

# filter the selected years
selected_years = totacts %>% filter(YEAR %in% c(10:21))
# histogram of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2010-2021") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)

# histogram of accident damage based on each year with bins = 10
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 10) + 
  ggtitle("Total Accident Damage in 2010-2021") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Damage in constant scales

# histogram of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2010-2021") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  xlim(c(0,1.7e7)) + ylim(c(0,1000)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~YEAR)


#***************************
# 3.3 Boxplots of ACCDMG
#***************************
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = "*", outlier.size = 5) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = 4, outlier.size = 2) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

## accident damge 2006
ggplot(as.data.frame(acts[[6]]$ACCDMG), aes(x=acts[[6]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2006") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

#4 plots on a single graph
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
# box plot of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# box plot of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  facet_wrap(~YEAR)


# box plot of accident damage based on each year
#With a constant scale
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  xlim(c(0,1.7e7)) +
  facet_wrap(~YEAR)


# box plot of accident damage based on each year
selected_years = totacts %>% filter(YEAR %in% c(9:19))
#With a constant scale
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  facet_wrap(~YEAR)


#change the color for the outlier
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) +
  geom_boxplot(col= "steelblue",outlier.shape=1) + 
  ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

#***************************
# 3.4 QQ plot of ACCDMG
#***************************

#Does our data come from a Guassian distribution
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(sample=acts[[11]]$ACCDMG)) + 
  stat_qq() + 
  stat_qq_line() +
  ggtitle("Total Accident Damage") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(acts[[11]]$TEMP), aes(sample=acts[[11]]$TEMP)) + 
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("Accident Temperature") + 
  theme(plot.title = element_text(hjust = 0.5))


#***************************
# 3.4 Density plot of ACCDMG
#***************************

d <- density(acts[[11]]$ACCDMG,
             kernel = "gaussian")
plot(d,  main = "gaussian kernel", col = 'red')

#density plot on top of histogram
h <- hist(acts[[20]]$ACCDMG, breaks = "FD", plot = FALSE)
ggplot(data=acts[[20]], aes(ACCDMG)) + 
  geom_histogram(aes(y =..density..),
                 breaks=h$breaks,
                 col="green",
                 fill="green",
                 alpha=.2) +
  geom_density(fill="red", color=1, alpha=0.4) +
  labs(title="Density and Histogram for ACCDMG", x="ACCDMG", y="Count") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))

#***************************
# 3.5 Bar plot of Accident Types
#***************************

# Each number represents a different type of accident as given in the data dictionary, 
#for e.g. 1-Derailment, 2-Head on Collision
ggplot(acts[[11]], aes(x=acts[[11]]$TYPE)) +
  geom_bar(stat="count", width=0.7, fill="steelblue", col = 'black') +
  labs(title="Bar plot of Accident Type", x="accident type")+
  scale_fill_brewer(palette="Paired")

