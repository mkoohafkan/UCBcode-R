# get_temp_diffs.R
# 
# Author: Michael
###############################################################################

# function
source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

tempdiffplot <- function(df, therange){
	# colors
	midcolor <- "#FFFFBF"
	lowcolor <- "#006837"
	highcolor <- "#A50026"
	#labels and limits
	themid <- therange[2]
	thelims <- c(round_any(therange[1], 2, f=floor), round_any(therange[3], 2, f=ceiling) )
	thebreaks <- round_any(c(therange[1], therange[3]), 1)
	thelabels <- thebreaks
	# define the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=themid, breaks=thelims, 
			label=thelabels, name=expression(degree~C))
	# make the plot
	formattedplot <- qplot(df[,1], df[,2], fill=df[,3], geom="raster") + theme_bw() +
			coord_fixed() + ylab("y (km)") + xlab("x (km)") + thescale +
			theme(axis.title.x = element_text(size=18), 
					axis.text.x = element_text(size=14), 
					axis.title.y = element_text(size=18), 
					axis.text.y = element_text(size=14), 
					legend.text = element_text(size = 18))	
	return(formattedplot)
}

# get filepaths
basepath <- 'C:/Users/Michael/Desktop/KrigLayers/pureR/daily/'
minvar <-'temperature_min'
maxvar <- 'temperature_max'

minlabels <- scratchlabels(minvar)
maxlabels <- scratchlabels(maxvar)

tempdifflabels <- scratchlabels('tempdiff')

# create directories
for(label in tempdifflabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

# prep
for(i in 1:length(tempdifflabels)){
	# min temp
	load(paste(basepath, minlabels[[i]], "_krigeobj.Rdata", sep=""))
	x <- krigeobj[['projx']]
	y <- krigeobj[['projy']]
	mindata <- krigeobj[[minvar]]
	samplex <- sampleobj[['projx']]
	sampley <- sampleobj[['projy']]
	samplemindata <- sampleobj[[minvar]]
	rm(krigeobj, sampleobj)
	# max temp
	load(paste(basepath, maxlabels[[i]], "_krigeobj.Rdata", sep=""))
	maxdata <- krigeobj[[maxvar]]
	samplemaxdata <- sampleobj[[maxvar]]
	rm(krigeobj, sampleobj)
	
	# calculate temp diff
	tempdiff <- maxdata - mindata
	sampletempdiff <- samplemaxdata - samplemindata
	krigeobj <- data.frame(x, y, tempdiff)
	sampleobj <- data.frame(samplex, sampley, sampletempdiff)
	names(krigeobj) <- c('projx', 'projy', 'tempdiff')
	names(sampleobj) <- names(krigeobj)
	save(krigeobj, sampleobj, file=paste(basepath, tempdifflabels[[i]], "_krigeobj.Rdata", sep=""))
	
	# plot it
	tempdiffrange <- c(min(krigeobj[['tempdiff']]), mean(krigeobj[['tempdiff']]), 
			max(krigeobj[['tempdiff']]))
	tempdiffrange[2] <- mean(c(tempdiffrange[1], tempdiffrange[3]))
	tempdiffraster <- tempdiffplot(krigeobj, tempdiffrange)
	ggsave(filename=paste(basepath, tempdifflabels[[i]], "_tempdiffplot.pdf", sep=""), 
			plot=tempdiffraster, width=8, height=8, dpi=300)
}