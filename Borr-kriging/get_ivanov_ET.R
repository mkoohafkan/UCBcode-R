# get_ivanov_ET.R
# 
# Author: Michael
###############################################################################

source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

# function
ivanov <- function(Tavg, RHavg){
	# PET = 0.000036*(25 + T)^2 * (100 - RH) (mm/day)
	# T is in Celsius
	# RH is in percent (e.g. 63)
	ET <- 0.000036*((25 + Tavg)^2)*(100 - RHavg)
	return(ET)
}

# get filepaths
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'
xvar <- "projx"
yvar <- "projy"
tempvar <- "temperature_avg"
rhvar <- "humidity_avg"

# get directories
iETlabels <- scratchlabels('iET')
templabels <- scratchlabels(tempvar)
rhlabels <- scratchlabels(rhvar)

# create directories
for(label in iETlabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

for(i in 1:length(iETlabels)){
	# temp
	load(paste(basepath, templabels[[i]], "_krigeobj.Rdata", sep=""))
	x <- krigeobj[[xvar]]
	y <- krigeobj[[yvar]]
	tempdata <- krigeobj[[tempvar]] - 273.15
	rm(krigeobj)
	# RH
	load(paste(basepath, rhlabels[[i]], "_krigeobj.Rdata", sep=""))
	rhdata <- krigeobj[[rhvar]]
	rm(krigeobj)
	# calculate iET
	iET <- ivanov(tempdata, rhdata)
	iETframe <- data.frame(x, y, iET)
	names(iETframe) <- c(xvar, yvar, 'iET')
	save(iETframe, file=paste(basepath, iETlabels[[i]], "_iET.Rdata", sep=""))	
	
	# plot it
	iETrange <- c(min(iETframe[['iET']]), mean(iETframe[['iET']]), 
			max(iETframe[['iET']]))
	iETraster <- ETplot(iETframe, iETrange)
	ggsave(filename=paste(basepath, iETlabels[[i]], "_ivanovplot.pdf", sep=""), 
			plot=iETraster, width=8, height=8, dpi=300)
	
}