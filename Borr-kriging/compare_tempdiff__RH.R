# TODO: Add comment
# 
# Author: Michael
###############################################################################


source("C:/Users/Michael/Desktop/GITrepo/R/borr-kriging/thefuncs.R")

fitresidplot <- function(df, therange){
	# colors
	midcolor <- "white"
	lowcolor <- "darkgreen"
	highcolor <- "darkorange"
	#labels and limits
	themid <- therange[2]
	thelims <- c(round_any(therange[1], 2, f=floor), round_any(therange[3], 2, f=ceiling) )
	thebreaks <- round_any(c(therange[1], therange[3]), 1)
	thelabels <- thebreaks
	# define the scale
	thescale <- scale_fill_gradient2(guide=guide_colourbar(title.vjust = 1000), 
			low=lowcolor, high=highcolor, mid=midcolor, 
			limits=thelims, midpoint=0, breaks=thelims, 
			label=thelabels, name="residual")
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
tempvar <-'tempdiff'
rhvar <- 'humidity_avg'

imgtype <- '.png'

templabels <- scratchlabels(tempvar)
rhlabels <- scratchlabels(rhvar)

complabels <- scratchlabels('RH_vs_tempdiff')
compformula <- as.formula(paste(rhvar, "~", tempvar))

# create directories
for(label in complabels) dir.create(file.path(basepath, label), recursive=T, showWarnings=F)

for(i in 1:length(complabels)){
	# temp
	load(paste(basepath, templabels[[i]], "_krigeobj.Rdata", sep=""))
	x <- krigeobj[['projx']]
	y <- krigeobj[['projy']]
	tempdata <- krigeobj[[tempvar]]
	samplex <- sampleobj[['projx']]
	sampley <- sampleobj[['projy']]
	sampletempdata <- sampleobj[[tempvar]]
	rm(krigeobj, sampleobj)
	# RH
	load(paste(basepath, rhlabels[[i]], "_krigeobj.Rdata", sep=""))
	rhdata <- krigeobj[[rhvar]]
	samplerhdata <- sampleobj[[rhvar]]
	rm(krigeobj, sampleobj)
	
	# plot linear relationship for sampleobj 
	sampledf <- data.frame(sampletempdata, samplerhdata)
	names(sampledf) <- c(tempvar, rhvar)
	samplefit <- lm(compformula, sampledf)
	cat("sampleset R squared = ", summary(samplefit)$r.squared, "\n")
	sampleresid <- samplefit$residuals
	sampleobj <- data.frame(samplex, sampley, sampletempdata, samplerhdata, sampleresid)
	names(sampleobj) <- c('projx', 'projy', tempvar, rhvar, 'residuals')
	samplefitplot <- ggplot(data = sampleobj, aes_string(x = tempvar, y = rhvar)) +
					 geom_smooth(se=TRUE, method = "lm", colour="red") + 
					 geom_point()
	ggsave(filename=paste(basepath, complabels[[i]], "_samplefitplot.pdf", sep=""), 
			plot=samplefitplot, width=8, height=4, dpi=300)
	
	# plot linear relationship for krigeobj
	df <- data.frame(tempdata, rhdata)
	names(df) <- c(tempvar, rhvar)
	krigefit <- lm(compformula, df)
	krigeobj <- data.frame(x, y, krigefit$residuals)
	names(krigeobj) <- c('projx', 'projy', 'residuals')
	cat("krigeset R squared = ", summary(krigefit)$r.squared, "\n")
	krigefitplot <- ggplot(data = df, aes_string(x = tempvar, y = rhvar)) +
			geom_point(size=0.25, color="black", alpha=0.25) +
			geom_smooth(se=TRUE, method = "lm", colour="red")
	ggsave(filename=paste(basepath, complabels[[i]], "_krigefitplot.pdf", sep=""), 
		   plot=krigefitplot, width=8, height=4, dpi=300)
	
	# save the data
	save(sampleobj, krigeobj, file=paste(basepath, complabels, "_krigeobj.Rdata", sep=""))

	# plot the residual map
	residrange <- c(min(krigeobj[['residuals']]), mean(krigeobj[['residuals']]), 
			max(krigeobj[['residuals']]))
	residrange[2] <- mean(c(residrange[1], residrange[3]))
	compraster <- fitresidplot(krigeobj, residrange)
	ggsave(filename=paste(basepath, complabels[[i]], "_fitresidplot.pdf", sep=""), 
			plot=compraster, width=8, height=8, dpi=300)
}
