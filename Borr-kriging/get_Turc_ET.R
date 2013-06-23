# get_turc_ET.R
# 
# Author: Michael
###############################################################################

source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

#function
turc <- function(Tavg, RHavg, Rg){
	# Rg = global (aka shortwave?) radiation
	# for daily, Rg in MJ m-2/day
	a <- 0.31 
	b <- 2.094
	C <- rep(NA, length(Tavg))
	C[RHavg >= 50] <- 1
	C[RHavg < 50] <- 1 + (50 - RHavg)/70
	
	tET = a*C*(Rg + b)*(Tavg/(Tavg + 15)) # >= 0.1 mm/day
}

# get filepaths
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'
xvar <- "projx"
yvar <- "projy"
tempvar <- "temperature_avg"
rhvar <- "humidity_avg"
# get directories
tETlabels <- scratchlabels('tET')
templabels <- scratchlabels(tempvar)
rhlabels <- scratchlabels(rhvar)

# create directories
for(label in tETlabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

# get solar radiation
solargrid <- gridtoDF(paste(basepath, 'solargrid.txt', sep=""), 'solrad')
# solar radiation: 3.6 * W*hr/m^2/day / 1000 = MJ/m^2.day
solargrid['solrad'] <- solargrid$solrad*3.6/1000

for(i in 1:length(tETlabels)){
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
	# apply mask to solar radiation
	raddata <- solargrid[gridmask,]$solrad

	# calculate tET
	tET <- turc(tempdata, rhdata, raddata)
	tETframe <- data.frame(x, y, tET)
	names(tETframe) <- c(xvar, yvar, 'tET')
	save(tETframe, file=paste(basepath, tETlabels[[i]], "_tET.Rdata", sep=""))	
	
	# plot it
	tETrange <- c(min(tETframe[['tET']]), mean(tETframe[['tET']]), 
			max(tETframe[['tET']]))
	tETraster <- ETplot(tETframe, tETrange)
	ggsave(filename=paste(basepath, tETlabels[[i]], "_turcplot.pdf", sep=""), 
			plot=tETraster, width=8, height=8, dpi=300)
}