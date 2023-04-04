

#******************************************************************
#
#			Input Functions for Accident Data
#
#*******************************************************************

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

# function to input files into a list

file.inputl <- function(my.path)
{
	my.dir <- getwd()
	setwd(my.path)
	my.files <- list.files(my.path)
	
	acts <- lapply(my.files, read.csv)

	return(acts)

}

# Function to create a data frame as the combination of multiple data frames in a list.

combine.data <- function(Data.List, Vars)
	{
		DF <- rbind(Data.List[[1]][, Vars])
		for(i in 2:length(Data.List))
	{
		DF <- rbind(DF, acts[[i]][,Vars])
	}
	DF
	}



