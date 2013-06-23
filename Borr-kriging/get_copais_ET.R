# get_copais_ET.R
# 
# Author: Michael
###############################################################################

source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

#function
copais <- function(Tavg, RHavg, Rs){
	# Rs is shortwave (aka global?) radiation
	m1 <- 0.057
	m2 <- 0.277
	m3 <- 0.643
	m4 <- 0.0124
	c1 <-  0.6416 - 0.00784*RHavg + 0.372*Rs - 0.00264*Rs*RHavg
	c2 <- -0.0033 + 0.00812*Tavg + 0.101*Rs + 0.00584*Rs*Tavg
	copais <- m1 + m2*c2 + m3*c1 + m4*c1*c2
	return(copais)
}

# get filepaths
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'
xvar <- "projx"
yvar <- "projy"
tempvar <- "temperature_avg"
rhvar <- "humidity_avg"

# get directories
cETlabels <- scratchlabels('cET')
templabels <- scratchlabels(tempvar)
rhlabels <- scratchlabels(rhvar)

# create directories
for(label in cETlabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

# get solar radiation
solargrid <- gridtoDF(paste(basepath, 'solargrid.txt', sep=""), 'solrad')
# solar radiation: 3.6 * W*hr/m^2 / 2.43 = mm/day
solargrid['solrad'] <- solargrid$solrad*3.6/(1000*2.43)

for(i in 1:length(cETlabels)){
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
	# calculate cET
	cET <- copais(tempdata, rhdata, raddata)
	cETframe <- data.frame(x, y, cET)
	names(cETframe) <- c(xvar, yvar, 'cET')
	save(cETframe, file=paste(basepath, cETlabels[[i]], "_cET.Rdata", sep=""))	
	
	# plot it
	cETrange <- c(min(cETframe[['cET']]), mean(cETframe[['cET']]), 
			max(cETframe[['cET']]))
	cETraster <- ETplot(cETframe, cETrange)
	ggsave(filename=paste(basepath, cETlabels[[i]], "_copaisplot.pdf", sep=""), 
			plot=cETraster, width=8, height=8, dpi=300)
}