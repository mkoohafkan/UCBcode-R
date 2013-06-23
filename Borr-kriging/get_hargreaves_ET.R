# get_hargreaves_ET.R
# 
# Author: Michael
###############################################################################

source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

# function
hargreaves <- function(tmin, tmax, tavg){ 
	# Temperature is in Celsius
	TD <- tmax - tmin
	Ra <- 14.40 # mm/day, April 15 2011
	KT <- 0.00185*TD*TD - 0.0433*TD + 0.4023
	res <- 0.0135*KT*Ra*sqrt(TD)*(tavg + 17.8)
	return(res)
}

# get filepaths
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'
xvar <- "projx"
yvar <- "projy"
minvar <- "temperature_min"
maxvar <- "temperature_max"
avgvar <- "temperature_avg"

minlabels <- scratchlabels(minvar)
maxlabels <- scratchlabels(maxvar)
avglabels <- scratchlabels(avgvar)
hETlabels <- scratchlabels('hET')

# create directories
for(label in hETlabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

# prep
for(i in 1:length(hETlabels)){
	# min temp
	load(paste(basepath, minlabels[[i]], "_krigeobj.Rdata", sep=""))
	x <- krigeobj[[xvar]]
	y <- krigeobj[[yvar]]
	mindata <- krigeobj[[minvar]] - 273.15
	rm(krigeobj)
	# max temp
	load(paste(basepath, maxlabels[[i]], "_krigeobj.Rdata", sep=""))
	maxdata <- krigeobj[[maxvar]] - 273.15
	rm(krigeobj)
	# avg temp
	load(paste(basepath, avglabels[[i]], "_krigeobj.Rdata", sep=""))
	avgdata <- krigeobj[[avgvar]] - 273.15
	rm(krigeobj)	
	
	# calculate hET
	hET <- hargreaves(mindata, maxdata, avgdata)
	hETframe <- data.frame(x, y, hET)
	names(hETframe) <- c(xvar, yvar, 'hET')
	save(hETframe, file=paste(basepath, hETlabels[[i]], "_hET.Rdata", sep=""))

	# plot it
	hETrange <- c(min(hETframe[['hET']]), mean(hETframe[['hET']]), 
			max(hETframe[['hET']]))
	hETraster <- ETplot(hETframe, hETrange)
	ggsave(filename=paste(basepath, hETlabels[[i]], "_hargreavesplot.pdf", sep=""), 
			plot=hETraster, width=8, height=8, dpi=300)
}