

#******************************************************
#
#					Session 5
#				
#				Principal Components
#
#******************************************************


#***********************************************************
#
#			Install and load the necessary libraries
#
#***********************************************************

# Load ggplot2 and psych libraries
library(ggplot2)
library(psych)

# libraries needed for ggbiplot and loadingsplot in PCAplots.R
library(data.table)
library(plyr)
library(scales)
library(grid)
library(ggpubr)

#***********************************************************
#
#			Load and format the data
#
#***********************************************************

traindir <- "C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/Data/Train Data"
sourcedir <-"C:/Users/Student/OneDrive - University of Virginia/Documents/SYS4021/In Class/"

# Source AccidentInput
setwd(sourcedir)
source("AccidentInput.R")
source("PCAplots.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame

totacts <- combine.data(acts)

#***********************************************************
#
#			Get the extreme accident data
#
#***********************************************************

# For ACCDMG
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# find only those above the upper whisker
upper <- ggplot_build(dmgbox)$data[[1]]$ymax
xdmg <- totacts[totacts$ACCDMG > upper,]

# For Casualties (TOTINJ + TOTKLD)

xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# Remove 9/11
xdmg <- xdmg[-183,]


#***********************************************************
#
#			Remove duplicates
#
#***********************************************************

# Remove duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#Reset rownames (observation #s) for sequential numbering- otherwise they will remain the #s from totacts

rownames(xdmgnd) <- NULL

#***********************************************************
#
#		Principal Components Analysis
#
#***********************************************************
?princomp


# Principal Components of metrics for extreme accidents

xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG","TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

#***********************************************************
#
#		Biplot
#
#***********************************************************

# View the data in the first 2 PCs

biplot(xdmgnd.pca, main="Biplot of Extreme Accident Metrics")

# Using ggbiplot

ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd)[,1])
ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd)[,1], plot.obs=FALSE)
ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd)[,1], plot.obs=FALSE, 
         xlim=c(-0.3,2), ylim=c(-0.7,0.7))

# Remove outliers in components 1 and 2

xdmgnd_no <- xdmgnd[-c(5732,7292,5186,7275,5733,5179),]
rownames(xdmgnd_no) <- NULL
xdmgnd_no.pca <- princomp(xdmgnd_no[, c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

# View the first 2 PCs without ouliers - do they change your conclusions?

biplot(xdmgnd_no.pca)
ggbiplot(xdmgnd_no.pca, varname.size = 5, labels=row(xdmgnd_no)[,1])
ggbiplot(xdmgnd_no.pca, varname.size = 5, labels=row(xdmgnd_no)[,1],
         plot.obs=FALSE, xlim=c(-0.3,2), ylim=c(-0.7,0.7))

#***********************************************************
#
#		Scree plot and cumulative variance
#
#***********************************************************

scree.cov <- ggscreeplot(xdmgnd.pca)
scree.cov$var
scree.cov$plot

# Cumulative variance
cum.cov <- cumplot(xdmgnd.pca)
cum.cov$cumvar
cum.cov$plot

#***********************************************************
#
#		Loadings plots
#
#***********************************************************

# Loadings in the first 2 PCs

loadplot.cov <- loadingsplot(xdmgnd.pca)
loadplot.cov$loadings
loadplot.cov$plot


